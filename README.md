# CobBank
## Overview
Welcome to the CobBank Project! This banking system is designed using Java Spring Boot, Angular, and COBOL to emulate legacys systems. This README will guide you through the project's features, setup instructions, and more.

## Table of Contents

- Introduction
- Features
- Technologies Used
- Architecture
- Setup Instructions
- Contact

### Introduction
This personal project aims to create a complete banking system where users can create accounts and perform transactions. All banking transactions are executed using COBOL programs. Due to the difficulty of accessing real mainframes, OpenCobol was selected to emulate these systems.

The system architecture consists of a web interface built with Angular and a backend powered by Java Spring Boot, which serves as a middleware to facilitate communication between the Angular front-end and the COBOL back-end. This setup leverages the strengths of each technology to deliver a robust and efficient solution.

The Angular front-end provides an advanced user experience and enhances development efficiency through component-based design, enabling extensive code reuse.

Security is a priority in this project. JWT is used to secure communications between Angular and Spring Boot. For communication between Spring Boot and COBOL, Docker Compose is utilized to conceal access URLs. In the future, a gateway may be implemented to further protect routes from unauthorized IP addresses.

### Features
- User Registration and Login: Secure user authentication and registration.
- Account Management: Real-time balance inquiry and detailed transaction history.
- Bank Transactions: Internal and external transfers with enhanced security.

### Technologies Used
- Backend: Java Spring Boot
- Frontend: Angular
- Legacy Integration: COBOL with OpenCOBOL, OCESQL for database connections, and CGI to emulate CICS web services.
- Database: PostgreSQL (or any other preferred DBMS)
- Security: JWT, OAuth2

### Architecture
The system architecture integrates modern web technologies with legacy COBOL systems to ensure robust performance and security. The following diagram illustrates the architecture:

### Setup Instructions
Follow these steps to set up the project locally:

1. Clone the repository

```
    git clone https://github.com/kerestes/CobBank.git
```

2. Using Docker Compose

```
    docker-compose up --build
```

### Contact
For any questions or suggestions, feel free to contact the project maintainers:

RODRIGUES KERESTES Alexandre: alexandrekerestes@gmail.com