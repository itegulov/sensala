import axios from 'axios/index';

const instance = axios.create({
    baseURL: 'http://localhost:8080'
});

export default instance;
