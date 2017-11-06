#!/usr/bin/env bash

stack exec network-monitor-server &

cd frontend
npm start