import React, { Component } from 'react';

import './demo.css';

class Demo extends Component {
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
                                   placeholder="Discourse to interpret"/>
                        </div>
                    </div>
                    <div className="form-group row justify-content-md-center">
                        <button type="submit" className="btn btn-primary"
                                id="interpret">Interpret!
                        </button>
                    </div>

                    <div className="row justify-content-center text-center mt-1" id="attach">
                        <svg id="svg-canvas-stanford" className="main-svg col-md-6"/>
                        <svg id="svg-canvas-sensala" className="main-svg col-md-6"/>
                    </div>

                    <div className="row justify-content-center text-center mt-5">
                        <div className="col-md-12">
                            <div id="loader" className="loader"/>
                        </div>
                    </div>

                    <div className="row justify-content-center text-center mt-5">
                        <div className="col-md-12">
                            <h2 id="term">

                            </h2>
                        </div>
                    </div>
                </div>
            </div>
        );
    }
}

export default Demo;
