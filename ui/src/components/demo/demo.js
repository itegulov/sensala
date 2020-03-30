import React, {Component} from 'react';
import * as d3 from 'd3';
import dagreD3 from 'dagre-d3';

import api from '../../api';

import './demo.css';

class Demo extends Component {
    constructor(props) {
        super(props);
        this.state = {discourse: '', isLoading: false, term: ''};

        this.handleInterpret = this.handleInterpret.bind(this);
        this.handleDiscourseChange = this.handleDiscourseChange.bind(this);
        this.populate = this.populate.bind(this);
        this.populateSensala = this.populateSensala.bind(this);
        this.renderText = this.renderText.bind(this);
    }
    
    populateSensala(data, nodes, edges) {
        let nodeID = Object.keys(nodes).length;

        if (typeof data === 'string') {
            nodes[nodeID] = {
                label: data,
                nodeclass: 'word',
                id: nodeID + ""
            };
            return nodes[nodeID];
        }
        
        let className = Object.getOwnPropertyNames(data)[0];

        let newNode = {
            label: className,
            nodeclass: className,
            id: nodeID + ""
        };
        nodes[nodeID] = newNode;
        
        let inside = Object.values(data)[0];
        for (let value of Object.values(inside)) {
            if (Array.isArray(value)) {
                value.forEach(function (child) {
                    for (let insideValue of Object.values(child)) {
                        let newChild = this.populateSensala(insideValue, nodes, edges);
                        edges.push({
                            source: newNode.id,
                            target: newChild.id,
                            id: newNode.id + "-" + newChild.id
                        });
                    }
                }, this);
            } else {
                let newChild = this.populateSensala(value, nodes, edges);
                edges.push({
                    source: newNode.id,
                    target: newChild.id,
                    id: newNode.id + "-" + newChild.id
                });
            }
        }
        return newNode;
    }

    populate(data, nodes, edges) {
        let nodeID = Object.keys(nodes).length;

        let newNode = {
            label: data.label,
            nodeclass: data.nodeType,
            id: nodeID + ""
        };

        nodes[nodeID] = newNode;

        data.children.forEach(function (child) {
            let newChild = this.populate(child, nodes, edges);

            edges.push({
                source: newNode.id,
                target: newChild.id,
                id: newNode.id + "-" + newChild.id
            });

        }, this);

        return newNode;
    }
    
    renderText(root, svgCanvas, populateFunction) {
        let nodes = {};
        let edges = [];

        populateFunction(root, nodes, edges);

        let g = new dagreD3.graphlib.Graph()
            .setGraph({})
            .setDefaultEdgeLabel(function () {
                return {};
            });

        for (let key in nodes) {
            let node = nodes[key];
            g.setNode(node.id, {
                label: node.label,
                class: node.nodeclass,
                //  round edges
                rx: 5,
                ry: 5
            });
        }

        edges.forEach(function (e) {
            g.setEdge(e.source, e.target, {
                lineTension: .8,
                lineInterpolate: "bundle"
            });
        });

        let render = new dagreD3.render();

        let svg = d3.select(svgCanvas),
            svgGroup = svg.append("g");

        render(d3.select(svgCanvas + " g"), g);
        
        // Enable zooming and panning on svg
        let zoom = d3.zoom().on('zoom', function() {
            svgGroup.attr('transform', d3.event.transform);
        });
        svg.call(zoom);

        // Fit svg to the size of graph
        let graphWidth = g.graph().width + 80;
        let graphHeight = g.graph().height + 0;
        let width = parseInt(svg.style("width").replace(/px/, ""));
        let height = parseInt(svg.style("height").replace(/px/, ""));

        let zoomScale = Math.max(Math.min(width / graphWidth, height / graphHeight));
        let translate = [(width / 2) - ((graphWidth * zoomScale) / 2), 0];
        let transform = d3.zoomIdentity
            .translate(translate[0], translate[1])
            .scale(zoomScale);
        
        svg.transition().duration(0).call(zoom.transform, transform);
    }

    handleInterpret() {
        let discourse = this.state.discourse;
        d3.select("#svg-canvas-stanford").selectAll("*").remove();
        d3.select("#svg-canvas-sensala").selectAll("*").remove();
        this.setState({isLoading: true});
        console.log(discourse);
        api.post(`/eval?discourse=${discourse}`)
            .then(res => {
                let messages = res.data;
                let nlTree = messages[0].result;
                let sensalaTree = messages[1].result;
                let term = messages[2].result;
                this.setState({term: term, isLoading: false});
                this.renderText(nlTree, "#svg-canvas-stanford", this.populate);
                this.renderText(sensalaTree, "#svg-canvas-sensala", this.populateSensala);
            }).catch(error => console.log(error));
    }

    handleDiscourseChange(event) {
        this.setState({discourse: event.target.value});
    }

    render() {
        return (
            <div>
                <div className="container-fluid mt-3">
                    <div className="row justify-content-center text-center mb-3">
                        <div className="col-md-6">
                            <h1>Sensala</h1>
                        </div>
                    </div>

                    <div className="form-group row justify-content-md-center">
                        <div className="col-md-6">
                            <input type="text" className="form-control" name="discourse"
                                   id="discourse" autoComplete="off"
                                   placeholder="Discourse to interpret"
                                   onChange={this.handleDiscourseChange}/>
                        </div>
                    </div>
                    <div className="form-group row justify-content-md-center">
                        <button type="submit" className="btn btn-primary"
                                onClick={this.handleInterpret}
                                id="interpret">Interpret!
                        </button>
                    </div>

                    <div className="row justify-content-center text-center mt-1" id="attach">
                        <svg id="svg-canvas-stanford" className="main-svg col-md-6"/>
                        <svg id="svg-canvas-sensala" className="main-svg col-md-6"/>
                    </div>

                    <div className="row justify-content-center text-center mt-5">
                        <div className="col-md-12">
                            {this.state.isLoading ? <div id="loader" className="loader"/> : ''}
                        </div>
                    </div>

                    <div className="row justify-content-center text-center mt-5">
                        <div className="col-md-12">
                            { !this.state.isLoading && this.state.term !== '' ?
                                <h2 id="term">{this.state.term}</h2> :
                                '' }
                        </div>
                    </div>
                </div>
            </div>
        );
    }
}

export default Demo;
