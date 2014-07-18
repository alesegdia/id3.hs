<?php

if($argc != 3) {
	print "error.";
	exit();
}

$fin = fopen($argv[1],"r");
$fout = fopen("$argv[2].hs","w");

$attrs = array();
$clases = array();
$ctrs = Array();

// cargar
if(($fhash = @fopen("hashes.dat","r"))!=false) {
	$data = fread($fhash, filesize("hashes.dat"));
	$data = json_decode($data, true);
	$attrs = $data[0];
	$clases = $data[1];
	$ctrs = $data[2];
	fclose($fhash);
}

$colclase = 8;
$idx = 0;
$jdy = 0;
$ctr = 0;
$ctr2 = 0;
$flag = false;
$limit = 9;

print "Generando $argv[2].hs a partir de $argv[1]...\n";

fwrite($fout,"module $argv[2] where\n\n");
fwrite($fout,"import DataSet\n\n");
fprintf($fout,"%s :: DataSet\n%s = ([\n",strtolower($argv[2]),strtolower($argv[2]));
if($fin) {
	while(($line = fgets($fin))!=false) {
		$line = trim($line);
		$ex = split(", ",$line);
		fprintf($fout,"\t[");
		foreach ($ex as $attr) {
			if(!isset($attrs[$attr])) {
				if($jdy == $colclase) {
					array_unshift($clases,trim($attr));
					$attrs[$attr]=$ctr2;
					$ctr2++;
				} else {
					if(!isset($ctrs[$jdy])) $ctrs[$jdy] = 0;
					$attrs[$attr]=$ctrs[$jdy]; #$ctr;
					$attrnames[$ctrs[$jdy]]=$attr;
					$ctrs[$jdy] = $ctrs[$jdy]+1;
				}
			}
			$val = $attrs[$attr];
			fprintf($fout,"%d,",$attrs[$attr]);
			$jdy++;
		}
		fseek($fout,-1,SEEK_CUR);
		fprintf($fout,"],\n");
		$idx++;
		$jdy=0;
	}
	fseek($fout,-2,SEEK_CUR);
	fwrite($fout,"],\n");
	fprintf($fout,"\t[");
	for($i=0;$i<$limit-1;$i++) {
		fprintf($fout,"%d,",$i);
	}
	fseek($fout,-1,SEEK_CUR);
	fprintf($fout,"],\n");

	fprintf($fout,"\t[");
	for($i=0;$i<count($clases);$i++) {
		fprintf($fout,"\"%s\",",$clases[$i]);
	}
	fseek($fout,-1,SEEK_CUR);
	fprintf($fout,"])\n");

}

// guardar
$fhash = fopen("hashes.dat","w");
$arr = Array($attrs, $clases, $ctrs);
fprintf($fhash,"%s",json_encode($arr, true));
fclose($fhash);

fclose($fin);
fclose($fout);

print_r ($attrs);
print_r($clases);
print_r($ctrs);


?>
