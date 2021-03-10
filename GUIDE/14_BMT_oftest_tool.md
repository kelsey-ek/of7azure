# Oftest tool usage by Kelsey

## Table of Contents

+ [1. Concept](#concept)
+ [2. Process](#process)


# OFTest Install Guide

## Table of Contents 

- [1. Overview](#1-overview)

## 1. Overview

This guide demonstrates how to install oftest on the linux system.  
Please note that at this point, only python3.6.8 will work.

## 2. Installing Packages

### 2.1 python3

#### 2.1.1 using yum

If your yum is set to install python3.6.8, you can use below commands to install.

```
sudo yum install python3 pip3
```

#### 2.2 Manual install

In case your yum doesn't support installing Python3.6.8, you need to install python manually.

```
sudo yum install gcc
sudo yum install openssl-devel
```

```
wget https://www.python.org/ftp/python/3.6.8/Python-3.6.8.tar.xz
tar xvf Python-3.6.8.tar.xz

sudo ./configure --prefix=/usr/local --enable-shared LDFLAGS="-Wl,-rpath /usr/local/lib"
sudo make altinstall
sudo ln -s /opt/python3/bin/python3.6 /usr/bin/python3
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
```

```
wget https://bootstrap.pypa.io/get-pip.py
python3 get-pip.py
```

### 2.2 x3270

Please make sure to install x3270l 3.6ga12 or below.
4.0ga9 has a bug and will not work with oftest.

```
sudo yum install x3270
```

### 2.3 oftest

ask grdc team to provide the oftest whl file.
openframe_test-0.0.1.dev10-cp36-cp36m-linux_x86_64.whl is only available at this point.

```
pip install openframe_test-0.0.1.dev10-cp36-cp36m-linux_x86_64.whl --user
```


# OFTest User Guide <!-- omit in toc -->

## Table of Contents <!-- omit in toc -->

- [1. Overview](#1-overview)
- [2. gen3270](#2-gen3270)
  - [2.1 Usage](#21-usage)
  - [2.2 INPUT](#22-input)
  - [2.3 TEMPLATE](#23-template)
  - [2.4 OUTPUT](#24-output)
  - [2.5 DELIMITER](#25-delimiter)
  - [2.5 LOG](#25-log)
- [3. test3270](#3-test3270)
  - [3.1 Usage](#31-usage)
  - [3.2 INPUT](#32-input)
  - [3.3 SERVER](#33-server)
  - [3.4 WAIT](#34-wait)
  - [3.5 LOG](#35-log)
  - [3.6 PROCESS](#36-process)
  - [3.7 SSL](#37-ssl)
  - [3.8 VISIBLE](#38-visible)
- [4. Appendix](#4-appendix)
  - [4.1 Example](#41-example)


## 1. Overview

OFTest is a software that is designed to perform automated 3270 transactions in the Openframe environment.


OFTest consists of two submodules
1. gen3270  
2. test3270

![alt-text](./reference_images/overview.png)


## 2. gen3270

Generates test3270 input from template and stream of inputs for given 3270 test scenario.

### 2.1 Usage

```
hostname@oframe:/home/oframe/oftest>oftest gen3270
usage: oftest gen3270 [-h] -i INPUT -o OUTPUT -t TEMPLATE [-d DELIMITER]
                      [-l LOG]

```

### 2.2 INPUT

Each line of gen3270 input represents a single test.  
In a single test, there are multiple fields which can be split by delimiter specified 2.5 delimiter.
The first field is a mapped to a test name and the rest of the fields are mapped to $input in the template described in 2.3 Template

Below is a example of the gen3270 input.  

```
TS 000000001 20120423174951175  000000043001190492
CP 000000012 20120423174951176 -000000000000000001  000000000000000000 0 345GK3027HF419
```

### 2.3 TEMPLATE

1. name
    - The template contains the list of tests which can be distinguished by the name.
    - For each test defined in the template gets processed when the first field of the input is matched to the test name.  
    - "LOGIN" test is a special test which gets processed unconditionally.
2. comment
    - This field can be used for adding comments
3. input
    - This field defines the input that we want to send to 3270 terminal.  
    - Each $input variable gets replaced with the field defined in gen3270 input
    - $input supports both delimiter based or fixed length based field replacement
        - To use delimiter based replacement, just simply use $input
        - To use fixed length based replacement, you need to specify the length of given $input by adding length which is surrounded with brackets [, ]
    - You can cast the input with a type you desire, by adding (int) or (float) in front of $input
    - To define key strokes, you need to surround the string with left and right arrows <, >
    - Available keys are defined in http://x3270.bgp.nu/Unix/x3270-man.html#Keypad

Below is an example which uses fixed length field replacement.

```
{
  "tests": [
  {
      "name": "LOGIN",
      "comment": "LOGIN",
      "inputs": [
          "ZREFCE",
          "<Enter>"
      ]
  },
  {
      "name": "CP",
      "comment": "$input[28]",
      "inputs": [
        "<Clear>",
        "CP01",
        "<Enter>",
        "(int)$input[20]",
        "<Tab>",
        "(int)$input[20]",
        "<Tab>",
        "(int)$input[2]",
        "$input[21]",
        "<Enter>",
        "<PF(3)>"
      ]
    },
    {
      "name": "TS",
      "comment": "$input[28]",
      "inputs": [
        "<Clear>",
        "TS01",
        "<Enter>",
        "(int)$input[20]",
        "<Enter>",
        "<PF(3)>"
      ]
    }
  ]
}

```


### 2.4 OUTPUT

The output of gen3270 is used as the input of the test3270 to run the predefined 3270 test scenarios.


Below is a example.
```
{
  "tests": [
    {
      "name": "LOGIN",
      "comment": "LOGIN",
      "inputs": [
        "ZREFCE",
        "<Enter>"
      ]
    },
    {
      "name": "TS",
      "comment": "000000001 20120423174951175 ",
      "inputs": [
        "<Clear>",
        "TS01",
        "<Enter>",
        "43001190492",
        "<Enter>",
        "<PF(3)>"
      ]
    },
    {
      "name": "CP",
      "comment": "000000012 20120423174951176 ",
      "inputs": [
        "<Clear>",
        "CP01",
        "<Enter>",
        "-1",
        "<Tab>",
        "0",
        "<Tab>",
        "0",
        "345GK3027HF419",
        "<Enter>",
        "<PF(3)>"
      ]
    }
  ]
}
```

### 2.5 DELIMITER

The delimiter in gen3270 is used to distinguish between the fields in a single test input.
The default value for the delimiter is 0x00 but you can override it by specifying -d option in the gen3270 command.
For example, you can use -d ' ' to make the delimiter as space instead of NULL.

### 2.5 LOG

The LOG option in gen3270 is used to print the logs in the STDOUT.  
Available options are DEBUG, INFO, WARNING, ERROR, CRITICAL and INFO is set as default.

## 3. test3270

Accepts test3270 input and perform the automated 3270 testing by utilizing x3270 terminal emulator.

### 3.1 Usage

```
hostname@oframe:/home/oframe/oftest/TXNFILES>oftest test3270
usage: oftest test3270 [-h] -i INPUT -s SERVER [-w WAIT] [-l LOG] [-p PROCESS]
                       [--ssl] [--visible]
```

### 3.2 INPUT

The input can be either manually created or can be generated through gen3270.  
test3270 runs the test based on this input.  

### 3.3 SERVER

The target server that we want to communicate with.
Available port is defined in $OFGW_HOME/ofgwconfig/vtam.properties.

### 3.4 WAIT

A wait time between AID (Action IDentifier) keys which triggers sending data to server which is ENTER and PF Keys.
Note that 350ms wait time is already in place in x3270 terminal emulator.

### 3.5 LOG

The LOG option in gen3270 is used to print the logs in the STDOUT.  
Available options are DEBUG, INFO, WARNING, ERROR, CRITICAL and INFO is set as default.

### 3.6 PROCESS

The number of processes that runs in parallel.
It is recommended to not use the --visible option when using this option.  

### 3.7 SSL

SSL option lets you enable ssl communication with the Openframe server.
You can enable this option by adding --ssl option to the oftest command.  

### 3.8 VISIBLE

VISIBLE option lets you use x3270 which is X server based GUI.
You can enable this option by adding --visible option to the oftest command.  

## 4. Appendix

### 4.1 Example

Step1. gen3270 input
```
TS 000000001 20120423174951175  000000043001190492
CP 000000012 20120423174951176 -000000000000000001  000000000000000000 0 345GK3027HF419
```

Step2. gen3270 template
```
{
  "tests": [
  {
      "name": "LOGIN",
      "comment": "LOGIN",
      "inputs": [
          "ZREFCE",
          "<Enter>"
      ]
  },
  {
      "name": "CP",
      "comment": "$input[28]",
      "inputs": [
        "<Clear>",
        "CP01",
        "<Enter>",
        "(int)$input[20]",
        "<Tab>",
        "(int)$input[20]",
        "<Tab>",
        "(int)$input[2]",
        "$input[21]",
        "<Enter>",
        "<PF(3)>"
      ]
    },
    { 
      "name": "TS",
      "comment": "$input[28]",
      "inputs": [
        "<Clear>",
        "TS01",
        "<Enter>",
        "(int)$input[20]",
        "<Enter>",
        "<PF(3)>"
      ]
    }
  ]
}
```

Step3. run gen3270
```
oftest gen3270 -i input.txt -t template.json -o output.json -d ' '
```

Step4. gen3270 output which is used as test3270 input 
```
{
  "tests": [
    {
      "name": "LOGIN",
      "comment": "LOGIN",
      "inputs": [
        "ZREFCE",
        "<Enter>"
      ]
    },
    {
      "name": "TS",
      "comment": "000000001 20120423174951175 ",
      "inputs": [
        "<Clear>",
        "TS01",
        "<Enter>",
        "43001190492",
        "<Enter>",
        "<PF(3)>"
      ]
    },
    {
      "name": "CP",
      "comment": "000000012 20120423174951176 ",
      "inputs": [
        "<Clear>",
        "CP01",
        "<Enter>",
        "-1",
        "<Tab>",
        "0",
        "<Tab>",
        "0",
        "345GK3027HF419",
        "<Enter>",
        "<PF(3)>"
      ]
    }
  ]
}
```

Step5. run 5 process concurrently in background.

```
hostname@oframe:/home/oframe>oftest test3270 -i output.json -s <ip>:<port> -p 5
```

> Note: 127.0.0.1 may not work with -p option based on your system setting.


## 4. Oftest

### 4.1. Usage

```bash
jangwon@jangwon:~/git/zref$ oftest -h
usage: oftest [-h] -i INPUT -s SERVER [-w WAIT] [-l LOG] [--ssl] [--visible]

optional arguments:
  -h, --help            show this help message and exit
  -i INPUT, --input INPUT
                        test input as JSON format
  -s SERVER, --server SERVER
                        3270 server URL <ip>:<port> ex) 127.0.0.1:5556
  -w WAIT, --wait WAIT  wait given seconds after the test is over
  -l LOG, --log LOG     set log level (DEBUG|INFO|WARNING|ERROR|CRITICAL).
                        default is INFO
  --ssl                 enable ssl
  --visible             make 3270 emulator visible
```

### 4.2. run test script

- sequence.sh
- parallel.sh

### 5. TPC-E

- TPC-E is an On-Line Transaction Processing Benchmark
  - Approved in February of 2007, TPC Benchmark E is an on-line transaction processing (OLTP) benchmark. TPC-E is more complex than previous OLTP benchmarks such as TPC-C because of its diverse transaction types, more complex database and overall execution structure. TPC-E involves a mix of twelve concurrent transactions of different types and complexity, either executed on-line or triggered by price or time criteria. The database is comprised of thirty-three tables with a wide range of columns, cardinality, and scaling properties. TPC-E is measured in transactions per second (tpsE). While the benchmark portrays the activity of a stock brokerage firm, TPC-E is not limited to the activity of any particular business segment, but rather represents any industry that must report upon and execute transactions of a financial nature.


