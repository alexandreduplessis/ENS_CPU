#include <iostream>
#include <bitset>
#include <fstream>
#include <string>
using namespace std;

string read_rom(){
	fstream newfile;
  	newfile.open("rom",ios::in);
  	string rom;
  	if (newfile.is_open()){
		getline(newfile, rom);
		newfile.close();
   	}
   	return rom;
}

int main(int argc, char** argv) {
	bitset<16> _l_0;
	bitset<16> _l_1;
	bitset<1> _l_10;
	bitset<1> _l_100;
	bitset<1> _l_101;
	bitset<1> _l_102;
	bitset<1> _l_103;
	bitset<1> _l_104;
	bitset<1> _l_105;
	bitset<1> _l_106;
	bitset<1> _l_107;
	bitset<1> _l_108;
	bitset<1> _l_109;
	bitset<2> _l_11;
	bitset<1> _l_110;
	bitset<1> _l_111;
	bitset<1> _l_112;
	bitset<1> _l_113;
	bitset<1> _l_114;
	bitset<1> _l_115;
	bitset<1> _l_116;
	bitset<1> _l_117;
	bitset<1> _l_118;
	bitset<1> _l_119;
	bitset<1> _l_12;
	bitset<1> _l_120;
	bitset<1> _l_121;
	bitset<1> _l_122;
	bitset<1> _l_123;
	bitset<1> _l_124;
	bitset<1> _l_125;
	bitset<1> _l_126;
	bitset<1> _l_127;
	bitset<16> _l_128;
	bitset<16> _l_129;
	bitset<1> _l_13;
	bitset<16> _l_130;
	bitset<16> _l_131;
	bitset<16> _l_132;
	bitset<16> _l_133;
	bitset<16> _l_134;
	bitset<16> _l_135;
	bitset<16> _l_136;
	bitset<16> _l_137;
	bitset<16> _l_138;
	bitset<16> _l_139;
	bitset<1> _l_14;
	bitset<16> _l_140;
	bitset<16> _l_141;
	bitset<16> _l_142;
	bitset<16> _l_143;
	bitset<16> _l_144;
	bitset<16> _l_145;
	bitset<16> _l_146;
	bitset<16> _l_147;
	bitset<16> _l_148;
	bitset<16> _l_149;
	bitset<3> _l_15;
	bitset<16> _l_150;
	bitset<16> _l_151;
	bitset<16> _l_152;
	bitset<16> _l_153;
	bitset<16> _l_154;
	bitset<16> _l_155;
	bitset<16> _l_156;
	bitset<16> _l_157;
	bitset<16> _l_158;
	bitset<16> _l_159;
	bitset<1> _l_16;
	bitset<1> _l_160;
	bitset<16> _l_161;
	bitset<16> _l_162;
	bitset<16> _l_163;
	bitset<16> _l_164;
	bitset<16> _l_165;
	bitset<16> _l_166;
	bitset<16> _l_167;
	bitset<16> _l_168;
	bitset<1> _l_169;
	bitset<1> _l_17;
	bitset<16> _l_170;
	bitset<16> _l_171;
	bitset<16> _l_172;
	bitset<16> _l_173;
	bitset<1> _l_174;
	bitset<16> _l_175;
	bitset<16> _l_176;
	bitset<1> _l_177;
	bitset<16> _l_178;
	bitset<1> _l_179;
	bitset<1> _l_18;
	bitset<16> _l_180;
	bitset<16> _l_181;
	bitset<16> _l_182;
	bitset<16> _l_183;
	bitset<16> _l_184;
	bitset<16> _l_185;
	bitset<16> _l_186;
	bitset<16> _l_187;
	bitset<1> _l_188;
	bitset<16> _l_189;
	bitset<4> _l_19;
	bitset<16> _l_190;
	bitset<16> _l_191;
	bitset<16> _l_192;
	bitset<1> _l_193;
	bitset<16> _l_194;
	bitset<16> _l_195;
	bitset<1> _l_196;
	bitset<16> _l_197;
	bitset<1> _l_198;
	bitset<16> _l_199;
	bitset<16> _l_2;
	bitset<1> _l_20;
	bitset<16> _l_200;
	bitset<1> _l_201;
	bitset<16> _l_202;
	bitset<16> _l_203;
	bitset<1> _l_204;
	bitset<1> _l_205;
	bitset<1> _l_206;
	bitset<1> _l_207;
	bitset<1> _l_208;
	bitset<1> _l_209;
	bitset<1> _l_21;
	bitset<1> _l_210;
	bitset<1> _l_211;
	bitset<1> _l_212;
	bitset<1> _l_213;
	bitset<1> _l_214;
	bitset<1> _l_215;
	bitset<1> _l_216;
	bitset<1> _l_217;
	bitset<1> _l_218;
	bitset<1> _l_219;
	bitset<1> _l_22;
	bitset<1> _l_220;
	bitset<2> _l_221;
	bitset<1> _l_222;
	bitset<1> _l_223;
	bitset<1> _l_224;
	bitset<1> _l_225;
	bitset<1> _l_226;
	bitset<1> _l_227;
	bitset<1> _l_228;
	bitset<1> _l_229;
	bitset<5> _l_23;
	bitset<3> _l_230;
	bitset<1> _l_231;
	bitset<1> _l_232;
	bitset<1> _l_233;
	bitset<1> _l_234;
	bitset<1> _l_235;
	bitset<1> _l_236;
	bitset<1> _l_237;
	bitset<1> _l_238;
	bitset<4> _l_239;
	bitset<1> _l_24;
	bitset<1> _l_240;
	bitset<1> _l_241;
	bitset<1> _l_242;
	bitset<1> _l_243;
	bitset<1> _l_244;
	bitset<1> _l_245;
	bitset<1> _l_246;
	bitset<1> _l_247;
	bitset<5> _l_248;
	bitset<1> _l_249;
	bitset<1> _l_25;
	bitset<1> _l_250;
	bitset<1> _l_251;
	bitset<1> _l_252;
	bitset<1> _l_253;
	bitset<1> _l_254;
	bitset<1> _l_255;
	bitset<1> _l_256;
	bitset<6> _l_257;
	bitset<1> _l_258;
	bitset<1> _l_259;
	bitset<1> _l_26;
	bitset<1> _l_260;
	bitset<1> _l_261;
	bitset<1> _l_262;
	bitset<1> _l_263;
	bitset<1> _l_264;
	bitset<1> _l_265;
	bitset<7> _l_266;
	bitset<1> _l_267;
	bitset<1> _l_268;
	bitset<1> _l_269;
	bitset<6> _l_27;
	bitset<1> _l_270;
	bitset<1> _l_271;
	bitset<1> _l_272;
	bitset<1> _l_273;
	bitset<1> _l_274;
	bitset<8> _l_275;
	bitset<1> _l_276;
	bitset<1> _l_277;
	bitset<1> _l_278;
	bitset<1> _l_279;
	bitset<1> _l_28;
	bitset<1> _l_280;
	bitset<1> _l_281;
	bitset<1> _l_282;
	bitset<1> _l_283;
	bitset<9> _l_284;
	bitset<1> _l_285;
	bitset<1> _l_286;
	bitset<1> _l_287;
	bitset<1> _l_288;
	bitset<1> _l_289;
	bitset<1> _l_29;
	bitset<1> _l_290;
	bitset<1> _l_291;
	bitset<1> _l_292;
	bitset<10> _l_293;
	bitset<1> _l_294;
	bitset<1> _l_295;
	bitset<1> _l_296;
	bitset<1> _l_297;
	bitset<1> _l_298;
	bitset<1> _l_299;
	bitset<16> _l_3;
	bitset<1> _l_30;
	bitset<1> _l_300;
	bitset<1> _l_301;
	bitset<11> _l_302;
	bitset<1> _l_303;
	bitset<1> _l_304;
	bitset<1> _l_305;
	bitset<1> _l_306;
	bitset<1> _l_307;
	bitset<1> _l_308;
	bitset<1> _l_309;
	bitset<7> _l_31;
	bitset<1> _l_310;
	bitset<12> _l_311;
	bitset<1> _l_312;
	bitset<1> _l_313;
	bitset<1> _l_314;
	bitset<1> _l_315;
	bitset<1> _l_316;
	bitset<1> _l_317;
	bitset<1> _l_318;
	bitset<1> _l_319;
	bitset<1> _l_32;
	bitset<13> _l_320;
	bitset<1> _l_321;
	bitset<1> _l_322;
	bitset<1> _l_323;
	bitset<1> _l_324;
	bitset<1> _l_325;
	bitset<1> _l_326;
	bitset<1> _l_327;
	bitset<1> _l_328;
	bitset<14> _l_329;
	bitset<1> _l_33;
	bitset<1> _l_330;
	bitset<1> _l_331;
	bitset<1> _l_332;
	bitset<1> _l_333;
	bitset<1> _l_334;
	bitset<1> _l_335;
	bitset<1> _l_336;
	bitset<1> _l_337;
	bitset<15> _l_338;
	bitset<1> _l_339;
	bitset<1> _l_34;
	bitset<1> _l_340;
	bitset<1> _l_341;
	bitset<1> _l_342;
	bitset<1> _l_343;
	bitset<1> _l_344;
	bitset<1> _l_345;
	bitset<1> _l_346;
	bitset<16> _l_347;
	bitset<16> _l_348;
	bitset<16> _l_349;
	bitset<8> _l_35;
	bitset<16> _l_350;
	bitset<16> _l_351;
	bitset<1> _l_352;
	bitset<16> _l_353;
	bitset<1> _l_354;
	bitset<16> _l_355;
	bitset<1> _l_356;
	bitset<1> _l_357;
	bitset<1> _l_358;
	bitset<1> _l_359;
	bitset<1> _l_36;
	bitset<1> _l_360;
	bitset<16> _l_361;
	bitset<1> _l_362;
	bitset<16> _l_363;
	bitset<1> _l_364;
	bitset<15> _l_365;
	bitset<1> _l_366;
	bitset<16> _l_367;
	bitset<16> _l_368;
	bitset<1> _l_369;
	bitset<1> _l_37;
	bitset<14> _l_370;
	bitset<2> _l_371;
	bitset<16> _l_372;
	bitset<16> _l_373;
	bitset<1> _l_374;
	bitset<12> _l_375;
	bitset<4> _l_376;
	bitset<16> _l_377;
	bitset<16> _l_378;
	bitset<1> _l_379;
	bitset<1> _l_38;
	bitset<8> _l_380;
	bitset<8> _l_381;
	bitset<16> _l_382;
	bitset<16> _l_383;
	bitset<1> _l_384;
	bitset<1> _l_385;
	bitset<15> _l_386;
	bitset<16> _l_387;
	bitset<16> _l_388;
	bitset<1> _l_389;
	bitset<9> _l_39;
	bitset<2> _l_390;
	bitset<14> _l_391;
	bitset<16> _l_392;
	bitset<16> _l_393;
	bitset<1> _l_394;
	bitset<4> _l_395;
	bitset<12> _l_396;
	bitset<16> _l_397;
	bitset<16> _l_398;
	bitset<1> _l_399;
	bitset<1> _l_4;
	bitset<1> _l_40;
	bitset<8> _l_400;
	bitset<8> _l_401;
	bitset<16> _l_402;
	bitset<16> _l_403;
	bitset<1> _l_404;
	bitset<16> _l_405;
	bitset<1> _l_406;
	bitset<1> _l_408;
	bitset<1> _l_409;
	bitset<1> _l_41;
	bitset<1> _l_410;
	bitset<1> _l_411;
	bitset<1> _l_412;
	bitset<1> _l_413;
	bitset<1> _l_414;
	bitset<1> _l_415;
	bitset<1> _l_416;
	bitset<1> _l_417;
	bitset<1> _l_418;
	bitset<1> _l_419;
	bitset<1> _l_42;
	bitset<1> _l_420;
	bitset<1> _l_421;
	bitset<1> _l_422;
	bitset<1> _l_423;
	bitset<1> _l_424;
	bitset<1> _l_425;
	bitset<1> _l_426;
	bitset<1> _l_427;
	bitset<1> _l_428;
	bitset<1> _l_429;
	bitset<10> _l_43;
	bitset<1> _l_430;
	bitset<1> _l_431;
	bitset<1> _l_432;
	bitset<1> _l_433;
	bitset<1> _l_434;
	bitset<1> _l_435;
	bitset<1> _l_436;
	bitset<1> _l_437;
	bitset<1> _l_438;
	bitset<1> _l_439;
	bitset<1> _l_44;
	bitset<1> _l_440;
	bitset<16> _l_441;
	bitset<1> _l_442;
	bitset<1> _l_443;
	bitset<1> _l_444;
	bitset<1> _l_445;
	bitset<1> _l_446;
	bitset<1> _l_447;
	bitset<1> _l_448;
	bitset<1> _l_449;
	bitset<1> _l_45;
	bitset<1> _l_450;
	bitset<1> _l_451;
	bitset<1> _l_452;
	bitset<1> _l_453;
	bitset<1> _l_454;
	bitset<1> _l_455;
	bitset<1> _l_456;
	bitset<1> _l_457;
	bitset<1> _l_458;
	bitset<1> _l_459;
	bitset<1> _l_46;
	bitset<1> _l_460;
	bitset<1> _l_461;
	bitset<1> _l_462;
	bitset<1> _l_463;
	bitset<1> _l_464;
	bitset<1> _l_465;
	bitset<1> _l_466;
	bitset<1> _l_467;
	bitset<1> _l_468;
	bitset<1> _l_469;
	bitset<11> _l_47;
	bitset<1> _l_470;
	bitset<1> _l_471;
	bitset<1> _l_472;
	bitset<1> _l_473;
	bitset<1> _l_474;
	bitset<1> _l_475;
	bitset<1> _l_476;
	bitset<1> _l_477;
	bitset<1> _l_478;
	bitset<1> _l_479;
	bitset<1> _l_48;
	bitset<1> _l_49;
	bitset<1> _l_5;
	bitset<1> _l_50;
	bitset<12> _l_51;
	bitset<1> _l_52;
	bitset<1> _l_53;
	bitset<1> _l_54;
	bitset<13> _l_55;
	bitset<1> _l_56;
	bitset<1> _l_57;
	bitset<1> _l_58;
	bitset<14> _l_59;
	bitset<1> _l_6;
	bitset<1> _l_60;
	bitset<1> _l_61;
	bitset<1> _l_62;
	bitset<15> _l_63;
	bitset<1> _l_64;
	bitset<1> _l_65;
	bitset<1> _l_66;
	bitset<16> _l_67;
	bitset<16> _l_68;
	bitset<38> _l_69;
	bitset<1> _l_7;
	bitset<10> _l_70;
	bitset<16> _l_71;
	bitset<3> _l_72;
	bitset<1> _l_73;
	bitset<1> _l_74;
	bitset<1> _l_75;
	bitset<1> _l_76;
	bitset<1> _l_77;
	bitset<1> _l_78;
	bitset<1> _l_79;
	bitset<1> _l_8;
	bitset<1> _l_80;
	bitset<1> _l_81;
	bitset<1> _l_82;
	bitset<1> _l_83;
	bitset<4> _l_84;
	bitset<4> _l_85;
	bitset<4> _l_86;
	bitset<1> _l_87;
	bitset<1> _l_88;
	bitset<1> _l_89;
	bitset<1> _l_9;
	bitset<1> _l_90;
	bitset<1> _l_91;
	bitset<1> _l_92;
	bitset<1> _l_93;
	bitset<1> _l_94;
	bitset<1> _l_95;
	bitset<1> _l_96;
	bitset<1> _l_97;
	bitset<1> _l_98;
	bitset<1> _l_99;
	bitset<16> pc_reg;
	bitset<16> reg__l_68 = 0;
	bitset<16> reg__l_129 = 0;
	bitset<16> reg__l_131 = 0;
	bitset<16> reg__l_133 = 0;
	bitset<16> reg__l_135 = 0;
	bitset<16> reg__l_137 = 0;
	bitset<16> reg__l_139 = 0;
	bitset<16> reg__l_141 = 0;
	bitset<16> reg__l_143 = 0;
	bitset<16> reg__l_145 = 0;
	bitset<16> reg__l_147 = 0;
	bitset<16> reg__l_149 = 0;
	bitset<16> reg__l_151 = 0;
	bitset<16> reg__l_153 = 0;
	bitset<16> reg__l_155 = 0;
	bitset<16> reg__l_157 = 0;
	bitset<16> reg__l_159 = 0;
	bitset<(65536)> rom__l_69 = bitset<(65536)>{read_rom()};
	bitset<65536> ram__l_203 = {0};
	while (1) {
		_l_0=reg__l_68;
		_l_12=bitset<1>{(_l_0.to_string())[2]};
		_l_128=reg__l_129;
		_l_130=reg__l_131;
		_l_132=reg__l_133;
		_l_134=reg__l_135;
		_l_136=reg__l_137;
		_l_138=reg__l_139;
		_l_140=reg__l_141;
		_l_142=reg__l_143;
		_l_144=reg__l_145;
		_l_146=reg__l_147;
		_l_148=reg__l_149;
		_l_150=reg__l_151;
		_l_152=reg__l_153;
		_l_154=reg__l_155;
		_l_156=reg__l_157;
		_l_158=reg__l_159;
		_l_16=bitset<1>{(_l_0.to_string())[3]};
		_l_199=bitset<16>{0000000000000000};
		_l_20=bitset<1>{(_l_0.to_string())[4]};
		_l_24=bitset<1>{(_l_0.to_string())[5]};
		_l_28=bitset<1>{(_l_0.to_string())[6]};
		_l_32=bitset<1>{(_l_0.to_string())[7]};
		_l_36=bitset<1>{(_l_0.to_string())[8]};
		_l_366=bitset<1>{"0"};
		_l_371=bitset<2>{00};
		_l_376=bitset<4>{0000};
		_l_381=bitset<8>{00000000};
		_l_385=bitset<1>{"0"};
		_l_390=bitset<2>{00};
		_l_395=bitset<4>{0000};
		_l_4=bitset<1>{"1"};
		_l_40=bitset<1>{(_l_0.to_string())[9]};
		_l_400=bitset<8>{00000000};
		_l_44=bitset<1>{(_l_0.to_string())[10]};
		_l_48=bitset<1>{(_l_0.to_string())[11]};
		_l_5=bitset<1>{(_l_0.to_string())[0]};
		_l_52=bitset<1>{(_l_0.to_string())[12]};
		_l_56=bitset<1>{(_l_0.to_string())[13]};
		_l_6=_l_5^_l_4;
		_l_60=bitset<1>{(_l_0.to_string())[14]};
		_l_64=bitset<1>{(_l_0.to_string())[15]};
		_l_69=bitset<38>{((rom__l_69).to_string()).substr ((_l_0).to_ulong()*38, 38)};
		_l_7=_l_6&_l_4;
		_l_70=bitset<(_l_70).size()>{((_l_69).to_string()).substr (12, 10)};
		_l_198=bitset<1>{(_l_70.to_string())[5]};
		_l_201=bitset<1>{(_l_70.to_string())[4]};
		_l_204=bitset<1>{(_l_70.to_string())[0]};
		_l_352=bitset<1>{(_l_70.to_string())[0]};
		_l_354=bitset<1>{(_l_70.to_string())[1]};
		_l_356=bitset<1>{(_l_70.to_string())[0]};
		_l_357=bitset<1>{(_l_70.to_string())[1]};
		_l_358=_l_356&_l_357;
		_l_359=bitset<1>{(_l_70.to_string())[2]};
		_l_360=_l_358&_l_359;
		_l_362=bitset<1>{(_l_70.to_string())[2]};
		_l_404=bitset<1>{(_l_70.to_string())[0]};
		_l_406=bitset<1>{(_l_70.to_string())[3]};
		_l_71=bitset<(_l_71).size()>{((_l_69).to_string()).substr (22, 16)};
		_l_203=bitset<16>{((ram__l_203).to_string()).substr ((_l_71).to_ulong())*16, (16))};
		_l_72=bitset<(_l_72).size()>{((_l_70).to_string()).substr (7, 3)};
		_l_442=bitset<1>{(_l_72.to_string())[0]};
		_l_443=bitset<1>{(_l_72.to_string())[1]};
		_l_445=~_l_443;
		_l_446=_l_442&_l_445;
		_l_444=bitset<1>{(_l_72.to_string())[2]};
		_l_447=~_l_444;
		_l_448=_l_446&_l_447;
		_l_449=bitset<1>{(_l_72.to_string())[0]};
		_l_452=~_l_449;
		_l_450=bitset<1>{(_l_72.to_string())[1]};
		_l_453=_l_452&_l_450;
		_l_451=bitset<1>{(_l_72.to_string())[2]};
		_l_454=~_l_451;
		_l_455=_l_453&_l_454;
		_l_457=bitset<1>{(_l_72.to_string())[0]};
		_l_458=bitset<1>{(_l_72.to_string())[1]};
		_l_460=_l_457&_l_458;
		_l_459=bitset<1>{(_l_72.to_string())[2]};
		_l_461=~_l_459;
		_l_462=_l_460&_l_461;
		_l_464=bitset<1>{(_l_72.to_string())[0]};
		_l_467=~_l_464;
		_l_465=bitset<1>{(_l_72.to_string())[1]};
		_l_468=~_l_465;
		_l_469=_l_467&_l_468;
		_l_466=bitset<1>{(_l_72.to_string())[2]};
		_l_470=_l_469&_l_466;
		_l_472=bitset<1>{(_l_72.to_string())[0]};
		_l_473=bitset<1>{(_l_72.to_string())[1]};
		_l_475=~_l_473;
		_l_476=_l_472&_l_475;
		_l_474=bitset<1>{(_l_72.to_string())[2]};
		_l_477=_l_476&_l_474;
		_l_73=bitset<1>{(_l_72.to_string())[0]};
		_l_74=bitset<1>{(_l_72.to_string())[1]};
		_l_75=_l_73|_l_74;
		_l_76=bitset<1>{(_l_72.to_string())[2]};
		_l_77=_l_75|_l_76;
		_l_78=bitset<1>{(_l_70.to_string())[6]};
		_l_79=bitset<1>{(_l_70.to_string())[0]};
		_l_8=bitset<1>{(_l_0.to_string())[1]};
		_l_80=_l_78&_l_79;
		_l_81=bitset<1>{(_l_70.to_string())[0]};
		_l_82=~_l_81;
		_l_83=_l_78&_l_82;
		_l_84=bitset<(_l_84).size()>{((_l_69).to_string()).substr (0, 4)};
		_l_85=bitset<(_l_85).size()>{((_l_69).to_string()).substr (4, 4)};
		_l_160=bitset<1>{(_l_85.to_string())[0]};