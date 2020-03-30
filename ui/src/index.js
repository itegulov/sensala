import React from 'react';
import ReactDOM from 'react-dom';
import {BrowserRouter as Router, Route, Switch} from 'react-router-dom';
import Container from 'react-bootstrap/Container'

import Homepage from './components/homepage';
import Demo from './components/demo/demo';

ReactDOM.render(
    <Router>
        <Switch>
            <Route exact path="/" component={Homepage}/>
            <Route exact path="/demo" component={Demo}/>
        </Switch>
    </Router>,
    document.querySelector('#root')
);