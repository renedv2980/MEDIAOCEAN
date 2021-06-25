*          DATA SET TAREP69    AT LEVEL 010 AS OF 05/02/14                      
*PHASE T70369C,*                                                                
         TITLE 'T70369 - ALLTAX CALCULATOR'                                     
T70369   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70369                                                         
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         ZAP   CKCNT,=P'0'                                                      
                                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MAIN10                                                           
         BAS   RE,VKEY                                                          
         B     YES                                                              
                                                                                
MAIN10   CLI   MODE,PRINTREP                                                    
         BNE   YES                                                              
                                                                                
         BAS   RE,VOPT             VALIDATE OPTIONS                             
         BAS   RE,PREP             PRINT REPORT                                 
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO VALIDATE KEY                                          
***********************************************************************         
VKEY     NTR1                                                                   
                                                                                
         XC    CAN2USR,CAN2USR                                                  
                                                                                
         LA    R2,TAXDATEH         VALIDATE PERIOD                              
         CLI   5(R2),0                                                          
         BNE   VKEY020                                                          
         GOTO1 DATCON,DMCB,(5,0),(1,STDATE)                                     
         B     VKEY030                                                          
                                                                                
VKEY020  GOTO1 ANY                                                              
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   STDATE,PVALPSTA                                                  
*&&DO                                                                           
VKEY025  LA    R2,TAXEMPH          VALIDATE EMPLOYER                            
         CLI   5(R2),0                                                          
         BNE   VKEY028                                                          
         MVI   5(R2),2                                                          
         MVC   8(2,R2),=C'TP'      DEFAULT TP                                   
         OI    6(R2),X'80'         TRANSMIT                                     
                                                                                
VKEY028  CLI   5(R2),2                                                          
         BH    FLDINV                                                           
         CLC   =C'TP',8(R2)        ONLY PP AND TP ARE VALID                     
         BE    *+10                                                             
         CLC   =C'PP',8(R2)                                                     
         BNE   FLDINV                                                           
*&&                                                                             
VKEY030  LA    R2,TAXUNT1H         VALIDATE TAX UNIT                            
         CLI   5(R2),0                                                          
         BE    FLDINV                                                           
         MVI   ISCAN,C'N'                                                       
         MVC   WORK(3),TAXUNT1                                                  
         OC    WORK(3),SPACES                                                   
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BE    VKEY040                                                          
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',WORK)                                         
         BNE   FLDINV                                                           
         TM    TGTASTAT,TASUINGF   ENSURE TAX UNIT IS VALID GOING               
         BO    FLDINV                                                           
         MVI   ISCAN,C'Y'                                                       
VKEY040  MVC   TAXUNT1,WORK                                                     
                                                                                
         CLC   TAXUNT1,=C'CN '     CAN FED NEEDS PROV FOR CALC                  
         BNE   VKEY100                                                          
         MVI   ISCAN,C'Y'                                                       
         LA    R2,TAXUNT2H         VALIDATE PROVINCE                            
         CLI   5(R2),0             MUST EXIST                                   
         BE    FLDINV                                                           
         MVC   WORK(3),TAXUNT2                                                  
         OC    WORK(3),SPACES                                                   
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',WORK)                                         
         BNE   FLDINV                                                           
         TM    TGTASTAT,TASUINGF   ENSURE TAX UNIT IS VALID GOING               
         BO    FLDINV                                                           
         MVC   TAXUNT2,WORK                                                     
                                                                                
VKEY100  LA    R2,TAXFREQH         VALIDATE FREQUENCY                           
         CLI   5(R2),0                                                          
         BE    FLDINV                                                           
         CLI   TAXFREQ,TASEFAN     ANNUALLY                                     
         BE    VKEY120                                                          
         CLI   TAXFREQ,TASEFQT     QUARTERLY                                    
         BE    VKEY120                                                          
         CLI   TAXFREQ,TASEFMO     MONTHLY                                      
         BE    VKEY120                                                          
         CLI   TAXFREQ,TASEFSM     SEMI-MONTHLY                                 
         BE    VKEY120                                                          
         CLI   TAXFREQ,TASEFBW     BIWEEKLY                                     
         BE    VKEY120                                                          
         CLI   TAXFREQ,TASEF4W     4 WEEKS                                      
         BE    VKEY120                                                          
         CLI   TAXFREQ,TASEFDA     DAILY                                        
         BE    VKEY120                                                          
         CLI   TAXFREQ,TASEFSA     SEMI-ANNUAL                                  
         BE    VKEY120                                                          
         CLI   TAXFREQ,TASEFWK     WEEKLY                                       
         BNE   FLDINV                                                           
                                                                                
VKEY120  LA    R2,TAXGRSPH         GROSS PAY                                    
         CLI   5(R2),0                                                          
         BE    FLDINV                                                           
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   CDEARN,4(R1)                                                     
                                                                                
VKEY150  CLI   ISCAN,C'Y'          CANADIAN, NEED NET CLAIM AMOUNT              
         BNE   VKEY600                                                          
         XC    NETCL1,NETCL1                                                    
         XC    NETCL2,NETCL2                                                    
                                                                                
         LA    R2,TAXC2URH         CAN$ TO US$ CONVERSION RATE                  
         CLI   5(R2),0                                                          
         BE    VKEY153                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         CLC   =F'65535',4(R1)     CAN'T BE TOO GREAT                           
         BNH   FLDINV                                                           
                                                                                
         MVC   CAN2USR,6(R1)                                                    
                                                                                
VKEY153  LA    R2,TAXNCL1H                                                      
         CLI   5(R2),0                                                          
         BE    VKEY155                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   NETCL1,4(R1)                                                     
                                                                                
VKEY155  LA    R2,TAXNCL2H                                                      
         CLI   5(R2),0                                                          
         BE    VKEY158                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   NETCL2,4(R1)                                                     
                                                                                
VKEY158  LA    R2,TAXPZONH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY160                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   PZONE,4(R1)                                                      
                                                                                
VKEY160  DS    0H                                                               
         LA    R2,TAXYPNWH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY165                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   YTDPNW,4(R1)                                                     
                                                                                
VKEY165  DS    0H                                                               
         LA    R2,TAXYPNTH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY170                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   YTDPNT,4(R1)                                                     
                                                                                
VKEY170  DS    0H                                                               
         LA    R2,TAXYUIWH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY175                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   YTDUIW,4(R1)                                                     
                                                                                
VKEY175  DS    0H                                                               
         LA    R2,TAXYUITH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY177                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   YTDUIT,4(R1)                                                     
                                                                                
VKEY177  DS    0H                                                               
         LA    R2,TAXYQPWH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY179                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   YTDQPW,4(R1)                                                     
                                                                                
VKEY179  DS    0H                                                               
         LA    R2,TAXYQPTH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY180                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   YTDQPT,4(R1)                                                     
                                                                                
VKEY180  DS    0H                                                               
         LA    R2,TAXTDEDH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY185                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             INVALID?                                     
         BNE   FLDINV                                                           
         MVC   TOTDED,4(R1)                                                     
                                                                                
VKEY185  DS    0H                                                               
         MVI   XMPTHLTH,0                                                       
         LA    R2,TAXXHLTH         EXEMPT FROM HEALTH CONTRIBUTIONS             
         CLI   5(R2),0                                                          
         BE    VKEY190                                                          
         CLI   8(R2),C'Y'          INVALID?                                     
         BNE   FLDINV                                                           
         MVI   XMPTHLTH,C'Y'                                                    
                                                                                
VKEY190  DS    0H                                                               
                                                                                
VKEY500  DS    0H                                                               
VKEY600  CLI   ISCAN,C'Y'          CANADIAN DON'T NEED THESE                    
         BE    VKEY800                                                          
         LA    R2,TAXSTATH         VALIDATE MARITAL STATUS                      
         CLI   5(R2),0                                                          
         BE    FLDINV                                                           
         CLI   TAXSTAT,C'S'        SINGLE                                       
         BE    VKEY700                                                          
         CLI   TAXSTAT,C'M'        MARRIED                                      
         BNE   FLDINV                                                           
                                                                                
VKEY700  LA    R2,TAXEXSH          VALIDATE EXEMPTIONS                          
         MVC   EXMPS,=C'00'                                                     
         CLI   5(R2),0                                                          
         BE    VKEY900                                                          
         TM    4(R2),X'08'         HAS TO BE NUMERIC                            
         BZ    FLDINV                                                           
         CLI   5(R2),1                                                          
         BNE   VKEY750                                                          
         MVC   EXMPS+1(1),8(R2)                                                 
         B     VKEY900                                                          
VKEY750  MVC   EXMPS,8(R2)                                                      
                                                                                
VKEY800  DS    0H                                                               
VKEY900  J     XIT                                                              
                                                                                
***********************************************************************         
*              ROUTINE TO VALIDATE OPTIONS                                      
***********************************************************************         
VOPT     NTR1                                                                   
         LA    R2,TAXOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         JE    VOPTX                                                            
                                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         JE    FLDINV                                                           
         ZIC   R0,4(R1)            R0 = NUM OF SCAN BLOCK ENTRIES               
                                                                                
VOPT3    CLC   =C'TEST',SCDATA1     USE TASEGUEA INSTEAD                        
         JNE   FLDINV                                                           
         OI    PROSTAT,PSTSEGA                                                  
                                                                                
         LA    R3,SCANNEXT         BUMP TO NEXT ELEMENT                         
         BCT   R0,VOPT3                                                         
                                                                                
VOPTX    J     XIT                                                              
         DROP  R3                  DROP THE SCAND DSECT                         
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
PREP     NTR1                                                                   
                                                                                
         TM    PROSTAT,PSTSEGA                                                  
         BZ    PREP005                                                          
         LOAD  EP=TASEGUEA,ERRET=LOADERR                                        
         B     PREP010                                                          
*                                                                               
PREP005  LOAD  EP=TASEGUE,ERRET=LOADERR                                         
PREP010  ST    R0,TASEGUE                                                       
                                                                                
***********************************************************************         
*        CALL TO TASEGUE                                              *         
***********************************************************************         
PREP500  XC    SEGBLK,SEGBLK       CLEAR OUT TASEGUE BLOCK                      
         USING TASEGUED,R6                                                      
         LA    R6,SEGBLK                                                        
                                                                                
         CLC   TAXUNT1,=C'FD '                                                  
         BE    PREP600                                                          
         CLI   ISCAN,C'Y'                                                       
         BE    PREP600                                                          
         MVC   TAXUNT2,TAXUNT1                                                  
         MVC   TAXUNT1,=C'FD '                                                  
                                                                                
PREP600  MVC   TASEUNIT,TAXUNT1    TAX UNIT                                     
         OC    TASEUNIT,SPACES                                                  
         MVC   TASEFREQ,TAXFREQ    ANNUAL TAX CALCULATION                       
         MVC   TASESTAT,TAXSTAT    MARRIAGE STATUS                              
         MVC   TASEEXS,EXMPS       EXEMPTIONS                                   
         MVI   TASERES,C'N'                                                     
         CLI   TAXRES,C'Y'                                                      
         BNE   *+8                                                              
         MVI   TASERES,C'Y'        RESIDENT                                     
                                                                                
         GOTO1 DATCON,DMCB,(1,STDATE),(20,TASECKDT)                             
         MVC   TASEEMP,=C'TP '     EMPLOYER                                     
         MVC   TASEEARN,CDEARN                                                  
*                                                                               
         MVC   TASEUNT2,TAXUNT2    TAX UNIT 2                                   
         MVC   TASENCL1,NETCL1                                                  
         MVC   TASENCL2,NETCL2                                                  
         MVC   TASEPZON,PZONE                                                   
         MVC   TASEYPWG,YTDPNW                                                  
         MVC   TASEYPTX,YTDPNT                                                  
         MVC   TASEYUWG,YTDUIW                                                  
         MVC   TASEYUTX,YTDUIT                                                  
         MVC   TASEYQPW,YTDQPW                                                  
         MVC   TASEYQPT,YTDQPT                                                  
         MVC   TASETDED,TOTDED                                                  
         MVC   TASEXHLT,XMPTHLTH                                                
                                                                                
PREP800  XC    TASETAX(76),TASETAX CLEAR TASEGUE RETURN AMOUNTS                 
                                                                                
         GOTO1 TASEGUE,DMCB,(R6)   CALL TASEGUE AND PASS SEGUE BLOCK            
                                                                                
         CLC   TASEUNIT,=C'FD '                                                 
         BE    PREP801                                                          
         CLI   ISCAN,C'Y'                                                       
         BNE   PREP803                                                          
PREP801  MVC   P+2(10),=C'CHECK DATE'                                           
         GOTO1 DATCON,DMCB,(1,STDATE),(21,P+18)                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
PREP803  MVC   P+2(4),=C'UNIT'                                                  
         MVC   P+25(3),TASEUNIT                                                 
         CLI   ISCAN,C'Y'                                                       
         BNE   PREP805                                                          
         MVI   P+29,C'/'                                                        
         MVC   P+31(3),TASEUNT2                                                 
PREP805  GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         CLI   TASERES,C' '                                                     
         BNH   PREP806                                                          
         MVC   P+2(8),=C'RESIDENT'                                              
         MVC   P+27(1),TASERES                                                  
         OI    P+27,C' '                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
PREP806  MVC   P+2(8),=C'PAY FREQ'                                              
         MVC   P+27(1),TASEFREQ                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         OC    CAN2USR,CAN2USR                                                  
         BZ    PREP806B                                                         
         MVC   P+22(4),=C'CAN$'                                                 
         MVC   P+43(3),=C'US$'                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+15(13),=C'-------------'                                       
         MVC   P+35(13),=C'-------------'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
PREP806B MVC   P+2(9),=C'GROSS PAY'                                             
         ICM   R4,15,TASEEARN                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
         CLI   ISCAN,C'Y'                                                       
         BNE   PREP808                                                          
         MVC   P+2(9),=C'NET CLAIM'                                             
         ICM   R4,15,TASENCL1                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
         OC    TASEYPWG,TASEYPWG                                                
         BZ    PREP806G                                                         
         MVC   P+2(11),=C'YTD PENS WG'                                          
         ICM   R4,15,TASEYPWG                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP806G OC    TASEYPTX,TASEYPTX                                                
         BZ    PREP806H                                                         
         MVC   P+2(11),=C'YTD PENS TX'                                          
         ICM   R4,15,TASEYPTX                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP806H OC    TASEYUWG,TASEYUWG                                                
         BZ    PREP806I                                                         
         MVC   P+2(11),=C'YTD UIC WG '                                          
         ICM   R4,15,TASEYUWG                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP806I OC    TASEYUTX,TASEYUTX                                                
         BZ    PREP806J                                                         
         MVC   P+2(11),=C'YTD UIC TX '                                          
         ICM   R4,15,TASEYUTX                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP806J OC    TASEYQPW,TASEYQPW                                                
         BZ    PREP806K                                                         
         MVC   P+2(11),=C'YTD QPIP WG '                                         
         ICM   R4,15,TASEYQPW                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP806K OC    TASEYQPT,TASEYQPT                                                
         BZ    PREP806L                                                         
         MVC   P+2(11),=C'YTD QPIP TX '                                         
         ICM   R4,15,TASEYQPT                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP806L OC    TASETDED,TASETDED                                                
         BZ    PREP806M                                                         
         MVC   P+2(11),=C'TOTAL DEDCT'                                          
         ICM   R4,15,TASETDED                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP806M CLI   TASEXHLT,C'Y'                                                    
         BNE   PREP806X                                                         
         MVC   P+2(11),=C'EXEMPT HLTH'                                          
         MVC   P+27(1),TASEXHLT                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
PREP806X B     PREP808                                                          
                                                                                
PREP807  DS    0H                                                               
         MVC   P+2(7),=C'M. STAT'                                               
         MVC   P+27(1),TASESTAT                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVC   P+2(10),=C'EXEMPTIONS'                                           
         MVC   P+26(2),TASEEXS                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
PREP808  MVC   P+2(26),=C'=========================='                           
         OC    CAN2USR,CAN2USR                                                  
         BZ    PREP809                                                          
         MVI   P+31,C'|'                                                        
         MVC   P+35(13),=C'============='                                       
PREP809  GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVC   P+2(3),=C'TAX'                                                   
         ICM   R4,15,TASETAX                                                    
         BAS   RE,PRTAMTS                                                       
                                                                                
         CLI   ISCAN,C'Y'          CANDIAN PRINTS OTHER STUFF                   
         BE    PREP900                                                          
                                                                                
         CLC   TASEUNIT,=C'FD '                                                 
         BE    PREP810                                                          
         MVC   P+2(3),=C'SUI'                                                   
         ICM   R4,15,TASESUI                                                    
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP810  MVC   P+2(4),=C'FICA'                                                  
         CLC   TASEUNIT,=C'FD '                                                 
         BE    *+10                                                             
         MVC   P+2(4),=C'SDI '                                                  
         ICM   R4,15,TASEFICA                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
         OC    TASESFLI,TASESFLI                                                
         BZ    PREP899                                                          
         MVC   P+2(4),=C'FLI'                                                   
         ICM   R4,15,TASESFLI                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP899  CLC   TASEUNIT,=C'FD '                                                 
         BNE   PREPXIT                                                          
         CLC   TAXUNT2,SPACES                                                   
         BNH   PREPXIT                                                          
         CLC   TAXUNT2,=C'FD '                                                  
         BE    PREPXIT                                                          
         MVC   TASEUNIT,TAXUNT2                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PREP800                                                          
                                                                                
PREP900  OC    TASECPN,TASECPN                                                  
         BZ    PREP910                                                          
         MVC   P+2(8),=C'CPP/QPP TAX'                                           
         ICM   R4,15,TASECPN                                                    
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP910  OC    TASEUIC,TASEUIC                                                  
         BZ    PREP920                                                          
         MVC   P+2(7),=C'UIC TAX'                                               
         ICM   R4,15,TASEUIC                                                    
         BAS   RE,PRTAMTS                                                       
                                                                                
PREP920  OC    TASEQPIP,TASEQPIP                                                
         BZ    PREP930                                                          
         MVC   P+2(8),=C'QPIP TAX'                                              
         ICM   R4,15,TASEQPIP                                                   
         BAS   RE,PRTAMTS                                                       
                                                                                
         B     PREPXIT                                                          
                                                                                
PREP930  DS    0H                                                               
PREPXIT  J     XIT                                                              
                                                                                
LOADERR  DC    H'0'    COULDN'T LOAD - R0=A(PHASE NAME), RF=RETURN CODE         
                                                                                
         EJECT                                                                  
PRTAMTS  NTR1                                                                   
         LR    R5,R4                                                            
         EDIT  (R4),(13,P+15),2                                                 
                                                                                
         OC    CAN2USR,CAN2USR                                                  
         BZ    PRTAMTSX                                                         
                                                                                
         MVI   P+31,C'|'                                                        
                                                                                
         XR    RE,RE                                                            
         ICM   RE,3,CAN2USR                                                     
                                                                                
         XR    R4,R4                                                            
         MR    R4,RE                                                            
         LHI   RF,5000                                                          
         DR    R4,RF                                                            
         LTR   R5,R5               R5 = QUOTIENT                                
         BM    *+8                                                              
         AHI   R5,1                ROUND                                        
         SRA   R5,1                                                             
                                                                                
         EDIT  (R5),(13,P+35),2                                                 
PRTAMTSX GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
TASEGUE  DS    A                   A(ALLTAX SEGUE ROUTINE)                      
                                                                                
FILTCAT  DS    CL3                 FILTER ON CATEGORY                           
SAVEKEY  DS    CL(L'TLCAKEY)                                                    
                                                                                
AGYCODE  DS    CL(L'TLCOAGY)       AGENCY CODE                                  
INVCODE  DS    CL6                 INVOICE CODE                                 
CKDATE   DS    CL8                 CHECK DATE                                   
CDCHK    DS    CL8                 CHECK NUMBER                                 
CURREMP  DS    CL3                 CURRENT EMPLOYER                             
EMPREQ   DS    CL3                 REQUESTED EMPLOYER                           
CURRSSN  DS    CL9                 CURRENT SSN                                  
CURRCHK  DS    CL8                 CURRENT CHECK NUMBER                         
MSTAT    DS    CL1                 MARRIAGE STATUS                              
EXMPS    DS    CL2                 EXEMPTIONS                                   
RESSTAT  DS    CL1                 RESIDENCE (Y/N)                              
UNTWAGES DS    F                   UNIT TAXABLE WAGES                           
PREWAGES DS    F                   PREV YTD TAXABLE WAGES                       
UTAX     DS    F                   UNIT TAX                                     
UTAXC    DS    F                   UNIT TAX CALCULATED                          
PRETAXC  DS    F                   PREVIOUS CALCULATED NY TAX                   
TMPAMT   DS    CL12                                                             
WHFLAT   DS    F                                                                
CDEARN   DS    F                                                                
CDNET    DS    F                                                                
CWTAX    DS    F                                                                
CWFIC    DS    F                                                                
CWSDI    DS    F                                                                
SWFIC    DS    F                                                                
SWSDI    DS    F                                                                
FRSTTAX  DS    F                   CHECK BEFORE STDATE TAX                      
FRSTCHK  DS    C                                                                
W4NAM2   DS    CL(L'TAW4NAM2)                                                   
W4NAM1   DS    CL(L'TAW4NAM1)                                                   
STDATE   DS    XL3                                                              
ENDATE   DS    XL3                                                              
TAXUNIT  DS    CL3                                                              
USEQU    DS    CL3                                                              
PDW4TY   DS    CL1                                                              
USSTA2   DS    XL1                                                              
XMPTHLTH DS    XL1                 EXEMPT FROM HEALTH CONTRIBUTION              
CURRCW   DS    A                   CURRENT TACW ELEMENT                         
CURRCY   DS    A                   CURRENT TACY ELEMENT                         
CKCNT    DS    PL12                CHECK COUNT                                  
                                                                                
ISCAN    DS    C                   UNIT IS CANADIAN                             
CAN2USR  DS    H                   CAN$ TO US$ CONVERSION RATE                  
NETCL1   DS    F                   NET CLAIM 1                                  
NETCL2   DS    F                   NET CLAIM 2                                  
PZONE    DS    F                   PRESCRIBED ZONE                              
YTDPNW   DS    F                   YTD PENSION WAGES                            
YTDPNT   DS    F                   YTD PENSION TAX                              
YTDUIW   DS    F                   YTD UIC WAGES                                
YTDUIT   DS    F                   YTD UIC TAX                                  
YTDQPW   DS    F                   YTD QPIP WAGES                               
YTDQPT   DS    F                   YTD QPIP TAX                                 
TOTDED   DS    F                   TOTAL DEDUCT                                 
                                                                                
CYAMTS   DS    0XL20                                                            
CYTAX    DS    F                   TAX                                          
CYFIC    DS    F                   FICA                                         
CYSDI    DS    F                   SDI                                          
CYSUI    DS    F                   SUI                                          
CYFLI    DS    F                   FLI                                          
                                                                                
SYAMTS   DS    0XL20                                                            
SYTAX    DS    F                   TAX                                          
SYFIC    DS    F                   FICA                                         
SYSDI    DS    F                   SDI                                          
SYSUI    DS    F                   SUI                                          
SYFLI    DS    F                   FLI                                          
                                                                                
XYAMTS   DS    0XL20                                                            
XYTAX    DS    F                   TAX                                          
XYFIC    DS    F                   FICA                                         
XYSDI    DS    F                   SDI                                          
XYSUI    DS    F                   SUI                                          
XYFLI    DS    F                   FLI                                          
                                                                                
PROSTAT  DS    X                                                                
PSTRACE  EQU   X'80'                                                            
PSTSEGA  EQU   X'40'               USE TASEGUEA                                 
                                                                                
         DS    0F                                                               
SEGBLK   DS    CL(TASELQ)          TASEGUE BLOCK                                
                                                                                
         DS    0F                                                               
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
MYDLNQ   EQU   *-MYD                                                            
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPFDD                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE TASEGUED                                                       
TWADCON  EJECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TAREP69   05/02/14'                                      
         END                                                                    
