\c cobbank;

CREATE TABLE IF NOT EXISTS adresse(
	adresse_id SERIAL PRIMARY KEY,
	ville CHAR(50) NOT NULL,
	adresse_nom CHAR(100) NOT NULL,
	num INT NOT NULL,
	code_postal INT NOT NULL,
	pays CHAR(50) NOT NULL
);

CREATE TABLE IF NOT EXISTS client (
	client_id SERIAL PRIMARY KEY,
	nom CHAR(100) NOT NULL,
	prenom CHAR(50) NOT NULL,
	mail CHAR(200) NOT NULL UNIQUE,
	telephone CHAR(20) NOT NULL,
	n_identite CHAR(20) NOT NULL,
	principal_adresse_id INT NOT NULL,
	FOREIGN KEY (principal_adresse_id) REFERENCES adresse(adresse_id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS compte(
	compte_id SERIAL PRIMARY KEY,
	date_creation TIMESTAMP NOT NULL,
	status SMALLINT,
	proprietaire_id INT NOT NULL,
	coproprietaire_id INT,
	type SMALLINT,
	code_guichet INT,
	num_compte CHAR(11),
	clef_rib SMALLINT,
	FOREIGN KEY (proprietaire_id) REFERENCES client(client_id) ON DELETE RESTRICT ON UPDATE CASCADE,
	FOREIGN KEY (coproprietaire_id) REFERENCES client(client_id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS transaction(
	transaction_id SERIAL PRIMARY KEY,
	compte_id INT NOT NULL,
	compte_id_2 INT,
	origine CHAR(200),
	destination CHAR(200),
	type_transaction SMALLINT,
	date_transaction TIMESTAMP NOT NULL,
	montant NUMERIC(20, 5),
	monnaie CHAR(3),
	consolide BOOL,
	FOREIGN KEY (compte_id) REFERENCES compte(compte_id) ON DELETE RESTRICT ON UPDATE CASCADE,
	FOREIGN KEY (compte_id_2) REFERENCES compte(compte_id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS consolidation(
	consolidation_id SERIAL PRIMARY KEY,
	compte_id INT NOT NULL,
	date_consolidation DATE NOT NULL,
	FOREIGN KEY (compte_id) REFERENCES compte(compte_id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS transaction_consolidation(
	transaction_consolidation_id SERIAL PRIMARY KEY,
	transaction_id INT NOT NULL,
	consolidation_id INT NOT NULL,
	FOREIGN KEY (transaction_id) REFERENCES transaction(transaction_id) ON DELETE RESTRICT ON UPDATE CASCADE,
	FOREIGN KEY (consolidation_id) REFERENCES consolidation(consolidation_id) ON DELETE RESTRICT ON UPDATE CASCADE
);
