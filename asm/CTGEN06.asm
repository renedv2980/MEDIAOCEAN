*          DATA SET CTGEN06    AT LEVEL 044 AS OF 08/08/16                      
*PHASE TA0B06A                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE DLFLD                                                                  
         TITLE 'CTGEN06 - USER ID INFORMATION DATA MAINTENANCE'                 
GEN06    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN6**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTIREC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
D        USING DLCBD,DLCB                                                       
*                                                                               
         LLC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              APMVALK                                      
         B     VALREC              APMVALR                                      
         B     DISKEY              APMDISK                                      
         B     DISREC              APMDISR                                      
         B     EXIT                APMDELR                                      
         B     EXIT                APMRESR                                      
         B     VALSEL              APMVALP                                      
         B     GETSEL              APMGETS                                      
         B     DISSEL              APMDISS                                      
         B     EXIT                APMVALS                                      
         B     EXIT                APMFLST                                      
         B     EXIT                APMPROC                                      
         B     EXIT                APMFSCR                                      
         B     LSTSCR              APMLSCR                                      
         B     VALREQ              APMVALQ                                      
         B     PRTREP              APMREPP                                      
         B     EXIT                APMSETT                                      
         B     EXIT                APMPUTK                                      
         B     VALREC              APMNEWK                                      
         B     EXIT                APMFRP                                       
         B     EXIT                APMDISS2                                     
*                                                                               
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
EXITCC   XIT1  ,                                                                
*                                                                               
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF USER ID RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         GOTO1 AFVAL,IDPIDAH       VALIDATE USER ID                             
         BNE   VALKEYX                                                          
         TM    FVIIND,FVINUM       CHECK FOR NUMERIC INPUT                      
         BO    VKUSR10                                                          
         CLI   FVILEN,3              ELSE USER ID CHARACTERS                    
         BL    EFTS                                                             
         MVC   CTIKID,FVIFLD                                                    
         B     VKUSRX                                                           
*                                  BUILD KEY OF NUMERIC REC & READ              
VKUSR10  OC    SCFULL(4),SCFULL    NUMBER IN SCFULL FROM AFVAL                  
         BZ    EIIF                CHECK NUMBER IN RANGE                        
         OC    SCFULL(2),SCFULL                                                 
         BNZ   EFTB                CHECK NUMBER IN RANGE                        
         MVC   CTIKNUM,SCFULL+2                                                 
         LA    R1,IORD+IOCONFIL+IO2                                             
         GOTO1 AIO                 READ ID RECORD                               
         BNE   ERNF                RECORD MUST BE PRESENT & CORRECT             
         L     R2,AIOAREA2                                                      
         LA    R3,CTIDATA                                                       
         SR    RF,RF                                                            
VKUSR20  CLI   0(R3),0             FIND PASSIVE POINTER                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VKUSR20                                                          
         MVC   IDPIDA,2(R3)                                                     
         MVI   IDPIDAH+5,8                                                      
         NI    IDPIDAH+4,X'F7'                                                  
         OI    IDPIDAH+6,X'80'                                                  
         B     VALKEY                                                           
VKUSRX   EQU   *                                                                
* VALIDATE THIS/LAST ACTIONS                                                    
*                                                                               
VKIO     MVC   APRECKEY(L'CTIKEY),CTIKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BH    *+12                NRF - CHECK IF DELETED                       
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY                                                          
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
         MVI   APINDS,APIOKADD                                                  
*                                                                               
VALKEYY  CLI   APACTN,ACTCPY                                                    
         BNE   VALKEYY1                                                         
         TM    ACLFMIND,ACLFMIFK   FIRST VALKEY                                 
         BNZ   VALKEYY1                                                         
         MVC   IOKEY,SAVKEY                                                     
         GOTO1 AIO,IOLOCK+IORDD+IOCONFIL+IO1                                    
         BE    *+6                                                              
         DC    H'00'                                                            
VALKEYY1 MVC   SAVKEY,APRECKEY                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE USER ID RECORDS                            *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1         INITIALISE RECORD                            
         MVC   CTIKEY,APRECKEY                                                  
*                                  CHANGE FUNCTION - SAVE ORIG STATUS           
VRINI    CLI   APACTN,ACTCHA                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   APFLAG,0                                                         
         LA    R3,CTIDATA          AND STRIP DOWN RECORD                        
VRINI10  CLI   0(R3),0                                                          
         BE    VRINIX                                                           
         CLI   0(R3),X'01'         ACTIVITY                                     
         BE    VRINI30             DELETE ELEMENT                               
         CLI   0(R3),X'33'                                                      
         BE    VRINI30                                                          
         CLI   0(R3),X'34'                                                      
         BE    VRINI30                                                          
         CLI   0(R3),X'36'                                                      
         BE    VRINI30                                                          
         CLI   0(R3),X'30'                                                      
         BE    VRINI30                                                          
         CLI   0(R3),X'3A'                                                      
         BE    VRINI30                                                          
         CLI   0(R3),X'42'                                                      
         BE    VRINI30                                                          
         CLI   0(R3),X'4C'                                                      
         BE    VRINI30                                                          
         CLI   0(R3),CTPNIELQ                                                   
         BE    VRINI30                                                          
         CLI   0(R3),X'02'                                                      
         BNE   VRINI20                                                          
         MVC   IDNUM,2(R3)                                                      
         MVI   APFLAG,1                                                         
VRINI20  ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRINI10                                                          
*                                                                               
VRINI30  SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),CTIREC),0,0                    
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRINI10                                                          
*                                                                               
VRINIX   CLI   APFLAG,1                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   IDALP,FVIFLD                                                     
         MVI   APWORK,0                                                         
         B     VRDDT                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD DESTINATION DETAILS ELEMENT                                             
***********************************************************************         
         SPACE 1                                                                
VRDDT    XC    APELEM,APELEM                                                    
         MVI   APELEM,X'30'                                                     
         MVI   APELEM+1,166                                                     
         LA    R3,APELEM                                                        
         USING CTDSTD,R3                                                        
         LA    R1,IDPDNAMH         VALIDATE DEST NAME                           
         LR    R8,R1                                                            
         GOTO1 AFVAL                                                            
         BNE   VRDDT10                                                          
         OI    APWORK,X'80'                                                     
         MVC   CTDSTNAM,FVIFLD     MOVE NAME TO ELEMENT                         
         SPACE 1                                                                
VRDDT10  LA    R1,IDPDADDH         VALIDATE DEST ADDR                           
         GOTO1 AFVAL                                                            
         BNE   VRDDT20                                                          
         TM    APWORK,X'80'        WAS NAME INPUT                               
         BO    VRDDT30                                                          
         ST    R8,FVADDR           NAME NOT INPUT                               
         B     EMIF                                                             
         SPACE 1                                                                
VRDDT20  TM    APWORK,X'80'        ADDRESS NOT INPUT                            
         BO    EMIF                                                             
         B     VRDDT40                                                          
         SPACE 1                                                                
VRDDT30  MVC   CTDSTADD,FVIFLD     MOVE ADDRESS TO ELEMENT                      
         MVC   CTDSTAD2,IDPDAD2                                                 
         MVC   CTDSTAD3,IDPDAD3                                                 
         OC    CTDSTAD2,SPACES                                                  
         OC    CTDSTAD3,SPACES                                                  
         OI    APWORK,X'40'                                                     
         SPACE 1                                                                
VRDDT40  LA    R1,IDPDLO1H         VALIDATE LOGO 1                              
         GOTO1 AFVAL                                                            
         BNE   VRDDT50                                                          
         TM    APWORK,X'C0'        WAS NAME/ADDRESS INPUT                       
         BO    VRDDT60                                                          
         ST    R8,FVADDR           NAME NOT INPUT                               
         B     EMIF                                                             
         SPACE 1                                                                
VRDDT50  TM    APWORK,X'C0'        LOGO 1 NOT INPUT                             
         BO    EMIF                                                             
         B     VRDDT70                                                          
         SPACE 1                                                                
VRDDT60  MVC   CTDSTLG1,FVIFLD     MOVE LOGO 1 TO ELEMENT                       
         OI    APWORK,X'20'                                                     
         SPACE 1                                                                
VRDDT70  LA    R1,IDPDLO2H         VALIDATE LOGO2                               
         GOTO1 AFVAL                                                            
         BNE   VRDDT80                                                          
         TM    APWORK,X'E0'        OTHERS INPUT                                 
         BO    *+12                                                             
         ST    R8,FVADDR           NAME NOT INPUT                               
         B     EMIF                                                             
         MVC   CTDSTLG2,FVIFLD     MOVE LOGO 2 TO ELEMENT                       
         SPACE 1                                                                
VRDDT80  LA    R1,IDPDPWRH         VALIDATE POWER CODE                          
         GOTO1 AFVAL                                                            
         BNE   VRDDT90                                                          
         TM    APWORK,X'E0'        OTHERS INPUT                                 
         BO    VRDDT100                                                         
         ST    R8,FVADDR           NAME NOT INPUT                               
         B     EMIF                                                             
         SPACE 1                                                                
VRDDT90  TM    APWORK,X'E0'        POWER CODE NOT INPUT                         
         BO    EMIF                                                             
         B     VRDDT110                                                         
         SPACE 1                                                                
*                                  CHECK FOR INVALID OV MVS JOBNAME             
VRDDT100 CLI   FVIFLD,C'@'                                                      
         BE    VRDDT105                                                         
         CLI   FVIFLD,C'$'                                                      
         BE    VRDDT105                                                         
         CLI   FVIFLD,C'#'                                                      
         BE    VRDDT105                                                         
*                                                                               
         CLI   FVIFLD,C'A'                                                      
         BL    EFNH                                                             
         CLI   FVIFLD,C'I'                                                      
         BNH   VRDDT105                                                         
*                                                                               
         CLI   FVIFLD,C'J'                                                      
         BL    EFNH                                                             
         CLI   FVIFLD,C'R'                                                      
         BNH   VRDDT105                                                         
*                                                                               
         CLI   FVIFLD,C'S'                                                      
         BL    EFNH                                                             
         CLI   FVIFLD,C'Z'                                                      
         BH    EFNH                                                             
*                                                                               
VRDDT105 MVC   CTDSTPOW,FVIFLD     POWER CODE TO ELEMENT                        
         OI    APWORK,X'10'                                                     
         SPACE 1                                                                
VRDDT110 LA    R1,IDPDLFCH         VALIDATE LASER FORM CHECKS                   
         GOTO1 AFVAL                                                            
         BNE   VRDDT150                                                         
         CLI   FVILEN,2                                                         
         BL    EFTS                                                             
         TM    APWORK,X'E0'        OTHERS INPUT                                 
         BO    VRDDT130                                                         
         ST    R8,FVADDR           NAME NOT INPUT                               
         B     EMIF                                                             
         SPACE 1                                                                
VRDDT130 MVC   CTDSTLFC,FVIFLD     LASER FORM CHECKS TO ELEMENT                 
         OI    APWORK,X'10'                                                     
         SPACE 1                                                                
VRDDT150 CLI   APWORK,0            TEST ANY DEST DETAIL FIELD INPUT             
         BE    VRDDTX              NO                                           
         GOTO1 AADDELN,CTIREC      ADD DEST DETAIL ELEMENT                      
         BNE   VALRECER            RECORD TOO BIG                               
VRDDTX   B     VRSHI                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD SHIPPING INFO ELEMENT                                                   
***********************************************************************         
         SPACE 1                                                                
VRSHI    LA    R1,IDPDSHAH                                                      
         GOTO1 AFVAL                                                            
         BNE   VRSHIX                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'4C'                                                     
         MVI   APELEM+1,X'06'                                                   
         MVI   APELEM+2,C'P'                                                    
         SR    R4,R4                                                            
         IC    R4,FVILEN                                                        
*&&US                                                                           
         CLI   FVIFLD,C'0'         CHECK FOR NUMERIC SHIPPING UNIT              
         BL    VRSHI10                                                          
         CLI   FVIFLD,C'9'                                                      
         BH    VRSHI10                                                          
         GOTO1 ABLDSHP,APPARM,FVIFLD,APELEM+6,(R4)                              
         CLI   8(R1),0                                                          
         BE    EFTB                                                             
         ZIC   R4,8(R1)            R4 = LENGTH OF ELEMENT DATA                  
         B     VRSHI20                                                          
*&&                                                                             
*                                                                               
VRSHI10  MVC   APELEM+6(60),FVIFLD  OTHERWISE MOVE IN AS IS                     
*                                                                               
VRSHI20  LA    R1,IDPDSHBH                                                      
         GOTO1 AFVAL                                                            
         BNE   VRSHI30                                                          
         MVC   APELEM+66(60),FVIFLD                                             
         IC    R4,FVILEN                                                        
         LA    R4,60(R4)                                                        
*                                                                               
VRSHI30  LA    R4,6(R4)                                                         
         STC   R4,APELEM+1                                                      
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
VRSHIX   B     VRORG                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD ORIGIN DETAIL ELEMENT                                                   
***********************************************************************         
         SPACE 1                                                                
VRORG    MVI   APWORK,0                                                         
         LA    R3,APELEM                                                        
         USING CTORGD,R3                                                        
         MVI   APELEM,X'36'                                                     
         MVI   APELEM+1,X'44'                                                   
         LA    R1,IDPONAMH                                                      
         GOTO1 AFVAL                                                            
         BNE   *+14                                                             
         MVC   CTORGNAM,FVIFLD     ORIGIN NAME                                  
         OI    APWORK,X'80'                                                     
         LR    R8,R1                                                            
         LA    R1,IDPOADDH                                                      
         GOTO1 AFVAL                                                            
         BNE   VRORG10                                                          
         MVC   CTORGADD,FVIFLD     ORIGIN ADDRESS                               
         TM    APWORK,X'80'                                                     
         BO    VRORG20                                                          
         ST    R8,FVADDR                                                        
         B     EMIF                                                             
         SPACE 1                                                                
VRORG10  TM    APWORK,X'80'                                                     
         BO    EMIF                                                             
         SPACE 1                                                                
VRORG20  CLI   APWORK,0            DO NOT ADD ELEMENT IF FLDS N/I               
         BE    VRORGX                                                           
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
VRORGX   B     VRDEI                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE & BUILD DESTINATION ID ELEMENTS                            *         
***********************************************************************         
         SPACE 1                                                                
VRDEI    LA    R9,IDPDIDAH                                                      
         LA    R8,3                                                             
*                                                                               
VRDEI10  MVI   FVINDX,0                                                         
         LR    R1,R9                                                            
         GOTO1 AFVAL                                                            
         BNE   VRDEI20                                                          
         GOTO1 VSCANNER,APPARM,FVADDR,LINES                                     
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINEST,4(R1)                                                    
         MVI   FVINDX,1                                                         
         LA    R4,LINES                                                         
         B     VRDEI30                                                          
*                                                                               
VRDEI20  ZIC   RE,0(R9)                                                         
         AR    R9,RE                                                            
         BCT   R8,VRDEI10                                                       
         B     VRDEIX                                                           
*                                                                               
VRDEI30  CLC   FVINDX,NLINEST                                                   
         BH    VRDEI20                                                          
         LA    R3,APELEM                                                        
         USING CTVALD,R3                                                        
         MVI   APELEM,X'34'                                                     
         MVI   APELEM+1,X'10'                                                   
         CLI   1(R4),0                                                          
         BNE   VRDEI50                                                          
         CLI   0(R4),3                                                          
         BL    EFTS                                                             
         BAS   RE,WILDCARD                                                      
         BE    VRDEI42                                                          
         L     R2,AIOAREA2                                                      
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,12(R4)                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         LA    R1,CTIDATA                                                       
         L     R2,AIOAREA1                                                      
         SR    RE,RE                                                            
*                                                                               
VRDEI40  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VRDEI40                                                          
         MVC   CTVALNUM,2(R1)                                                   
         MVC   CTVALDST,12(R4)                                                  
         XC    CTVALSPR,CTVALSPR                                                
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         B     VRDEI80                                                          
*                                                                               
VRDEI42  EQU   *                   PROCESS WILDCARD ID XX*                      
         XC    CTVALNUM,CTVALNUM                                                
         MVC   CTVALDST,12(R4)                                                  
         XC    CTVALSPR,CTVALSPR                                                
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         B     VRDEI80                                                          
*                                                                               
VRDEI50  ZIC   R1,0(R4)            VALIDATE LIST-ID/AGY                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'LIST'   CHECK KEYWORD                                
         BE    VRDEI70                                                          
*                                                                               
         EX    R1,*+8                                                           
         B     VRDEI60                                                          
         CLC   12(0,R4),=C'AGY'    CHECK FOR VALID KEYWORD 'AGY'                
         BNE   EIIF                                                             
*                                                                               
VRDEI60  CLI   1(R4),2                                                          
         BNE   EIIF                MUST BE 2 CHARS                              
         L     R2,AIOAREA2         SWITCH I/O AREAS                             
         USING CT9BREC,R2                                                       
         XC    CT9BKEY,CT9BKEY     BUILD LIST KEY                               
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01                                                
         MVC   CT9BKAGY,22(R4)                                                  
         MVC   IOKEY(L'IOKEY),CT9BKEY                                           
         GOTO1 AIO,IOHI+IOCONFIL+IO2                                            
         BL    EIIO                                                             
         CLC   CT9BKAGY,22(R4)     SAME AGENCY?                                 
         BNE   ERNF                NO - NOT FOUND                               
*                                                                               
         L     R2,AIOAREA1                                                      
         USING CTIREC,R2                                                        
         MVC   CTVALDST(2),=X'0001'     AGY SUBCODE                             
         MVC   CTVALDST+2(10),22(R4)                                            
         XC    CTVALSPR,CTVALSPR                                                
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         B     VRDEI80                                                          
*                                                                               
VRDEI70  CLI   1(R4),6                                                          
         BH    EFTL                                                             
         L     R2,AIOAREA2         SWITCH I/O AREAS                             
         USING CTWREC,R2                                                        
         XC    CTWKEY,CTWKEY       BUILD KEY OF LIST RECORD                     
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'I'                                                     
         MVC   CTWKID,22(R4)                                                    
         MVC   IOKEY(L'IOKEY),CTWKEY                                            
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         L     R2,AIOAREA1                                                      
         USING CTIREC,R2                                                        
         XC    CTVALDST(16),CTVALDST                                            
         MVC   CTVALDST+2(10),22(R4)                                            
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
VRDEI80  DS    0H                                                               
         LA    R4,L'LINES(R4)                                                   
         SR    R1,R1                                                            
         IC    R1,FVINDX                                                        
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         B     VRDEI30                                                          
*                                                                               
VRDEIX   B     VRPRT                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE & BUILD PRINTER ELEMENTS                                   *         
***********************************************************************         
         SPACE 1                                                                
VRPRT    LA    R9,IDPDPTAH                                                      
         LA    R8,3                                                             
         MVI   VPRTFLAG,0                                                       
         SPACE 1                                                                
VRPRT10  MVI   FVINDX,0                                                         
         LR    R1,R9                                                            
         GOTO1 AFVAL                                                            
         BNE   VRPRT20                                                          
         GOTO1 VSCANNER,APPARM,FVADDR,LINES                                     
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINEST,4(R1)                                                    
         MVI   FVINDX,1                                                         
         LA    R4,LINES                                                         
         B     VRPRT30                                                          
         SPACE 1                                                                
VRPRT20  ZIC   RE,0(R9)                                                         
         AR    R9,RE                                                            
         BCT   R8,VRPRT10                                                       
         B     VRPRT200                                                         
         SPACE 1                                                                
VRPRT30  CLC   FVINDX,NLINEST                                                   
         BH    VRPRT20                                                          
         LA    R3,APELEM                                                        
         USING CTPRND,R3                                                        
         MVI   APELEM,X'3A'                                                     
         MVI   APELEM+1,X'0B'                                                   
         TM    2(R4),X'80'                                                      
         BZ    VRPRT80                                                          
         TM    VPRTFLAG,VPRTFPNM                                                
         BO    ENPN                                                             
         CLC   4(4,R4),=F'255'                                                  
         BH    EFTB                                                             
         MVC   CTPRNNUM,7(R4)                                                   
         CLI   1(R4),4             ALLOW 4 CHR LUIDS                            
         BL    EFTS                                                             
         CLI   1(R4),8                                                          
         BH    EFTL                                                             
         MVC   CTPRNLIN(8),22(R4)                                               
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
         SPACE 1                                                                
VRPRT40  CLI   0(R1),0                                                          
         BE    VRPRT70                                                          
         CLI   0(R1),X'3A'                                                      
         BE    VRPRT60                                                          
         SPACE 1                                                                
VRPRT50  IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VRPRT40                                                          
         SPACE 1                                                                
VRPRT60  CLC   CTPRNNUM,2(R1)                                                   
         BE    EDIF                                                             
         CLC   CTPRNLIN(8),3(R1)                                                
         BE    EDIF                                                             
         B     VRPRT50                                                          
         SPACE 1                                                                
VRPRT70  EQU   *                                                                
         OI    VPRTFLAG,VPRTFLID                                                
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         SPACE 1                                                                
VRPRT72  EQU   *                                                                
         LA    R4,L'LINES(R4)                                                   
         SR    R1,R1                                                            
         IC    R1,FVINDX                                                        
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         B     VRPRT30                                                          
         SPACE 1                                                                
VRPRT80  ZIC   R1,0(R4)            VALIDATE LIST-ID                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'LIST'   CHECK KEYWORD                                
         BNE   VRPRT100                                                         
         TM    VPRTFLAG,VPRTFPNM                                                
         BO    ENPN                                                             
         CLI   1(R4),6             VALIDATE LIST ENTRY                          
         BH    EFTL                                                             
         L     R2,AIOAREA2         SWITCH I/O AREAS                             
         USING CTWREC,R2                                                        
         XC    CTWKEY,CTWKEY       BUILD KEY OF LIST RECORD                     
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'R'                                                     
         MVC   CTWKID,22(R4)                                                    
         MVC   IOKEY(L'CTWKEY),CTWKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         L     R2,AIOAREA1                                                      
         USING CTIREC,R2                                                        
         XC    CTPRNNUM(9),CTPRNNUM                                             
         MVC   CTPRNNUM+2(6),22(R4)                                             
         B     VRPRT70                                                          
*                                                                               
VRPRT100 EQU   *                                                                
         ZIC   R1,0(R4)            VALIDATE PRINTER NAME KEYWORD                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'PNAME'  CHECK KEYWORD                                
         BNE   VRPRT120                                                         
         TM    VPRTFLAG,VPRTFLID                                                
         BO    ENPN                                                             
         TM    VPRTFLAG,VPRTFPNM                                                
         BO    EDIF                                                             
         CLI   1(R4),0             SINGLE FIELD                                 
         BNE   ETOO                                                             
         OI    VPRTFLAG,VPRTFPNM                                                
         B     VRPRT72                                                          
*                                                                               
VRPRT120 EQU   *                                                                
         ZIC   R1,0(R4)            VALIDATE PNAME DEFAULT CONTROL               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'DEFAULT'                                             
         BNE   EIPI                                                             
         TM    VPRTFLAG,VPRTFLID                                                
         BO    EDNM                                                             
         TM    VPRTFLAG,VPRTFPNM                                                
         BZ    EDPN                                                             
         CLI   1(R4),9                                                          
         BNE   VRPRT140                                                         
         CLC   22(9,R4),=CL9'PRINCIPAL'                                         
         BNE   VRPRT140                                                         
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    EPND                                                             
         OI    VPRTFLAG,VPRTFPID                                                
         B     VRPRT72                                                          
*                                                                               
VRPRT140 EQU   *                                                                
         CLI   1(R4),10            VALIDATE PNAME DEFAULT USERID                
         BH    EFTL                                                             
         L     R2,AIOAREA2         SWITCH I/O AREAS                             
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF USERID RECORD                   
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,22(R4)                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    EIDI                                                             
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    EIID                                                             
         CLI   1(R3),4                                                          
         BNE   EIID                                                             
         MVC   APHALF,2(R3)                                                     
         L     R2,AIOAREA1                                                      
         USING CTIREC,R2                                                        
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         USING CTPNID,R3                                                        
         MVI   CTPNIEL,CTPNIELQ                                                 
         MVI   CTPNILEN,CTPNILNQ                                                
         MVC   CTPNIUID,APHALF                                                  
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         OI    VPRTFLAG,VPRTFPDI                                                
         B     VRPRT72                                                          
         SPACE 1                                                                
VRPRT200 EQU   *                                                                
         LA    R3,CTIDATA                                                       
         SR    RF,RF                                                            
         USING CTIDOD,R3                                                        
VRPRT210 CLI   CTIDOEL,0                                                        
         BE    VRPRT220                                                         
         CLI   CTIDOEL,CTIDOELQ                                                 
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRPRT210                                                         
         NI    CTIDOFL2,X'FF'-(CTIDOFPN+CTIDOFPP+CTIDOFPI)                      
         TM    VPRTFLAG,VPRTFLID                                                
         BO    VRPRTX                                                           
         TM    VPRTFLAG,VPRTFPNM                                                
         BZ    *+8                                                              
         OI    CTIDOFL2,CTIDOFPN                                                
         TM    VPRTFLAG,VPRTFPID                                                
         BZ    *+12                                                             
         OI    CTIDOFL2,CTIDOFPP                                                
         B     VRPRTX                                                           
         TM    VPRTFLAG,VPRTFPDI                                                
         BZ    VRPRTX                                                           
         OI    CTIDOFL2,CTIDOFPI                                                
         B     VRPRTX                                                           
         SPACE 1                                                                
VRPRT220 TM    VPRTFLAG,VPRTFPNM                                                
         BZ    VRPRTX                                                           
         B     EIIF                                                             
         SPACE 1                                                                
VRPRTX   EQU   *                                                                
         B     VROUT                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE & BUILD OUTPUT TYPE ELEMENT                                *         
***********************************************************************         
         SPACE 1                                                                
VROUT    LA    R1,IDPOUTIH                                                      
         GOTO1 AFVAL                                                            
         BNE   VROUTX                                                           
         CLI   FVILEN,1                                                         
         BL    EFTS                                                             
         L     R2,AIOAREA2                                                      
         USING CTOREC,R2           BUILD OUTPUT TYPE KEY                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKEY,C'O'                                                      
         MVC   CTOKID,FVIFLD       MOVE ID TO KEY                               
         MVC   IOKEY(L'CTOKEY),CTOKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         L     R2,AIOAREA1                                                      
         USING CTIREC,R2                                                        
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'42'                                                     
         USING CTOCOD,R3                                                        
         MVC   CTOCODE,FVIFLD                                                   
         MVI   CTOCOLEN,16                                                      
         GOTO1 AADDELN,CTIREC      ADD OUTPUT CODE ELEMENT                      
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
VROUTX   B     VROTH                                                            
         EJECT                                                                  
* VALIDATE OTHER ID FIELDS                                                      
*                                                                               
VROTH    DS    0H                                                               
*&&UK                                                                           
VRUKA    XC    APELEM,APELEM       UK AGENCY DATA                               
         MVI   APELEM,X'33'                                                     
         MVI   APELEM+1,X'09'                                                   
         LA    R3,APELEM                                                        
         USING CTUKAD,R3                                                        
         GOTO1 AFVAL,IDPPLNOH                                                   
         MVC   CTUKALIN,FVIFLD                                                  
         GOTO1 AFVAL,IDPIPAH                                                    
         MVC   CTUKAIPA,FVIFLD                                                  
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
VRUKAX   EQU   *                                                                
*&&                                                                             
*&&US                                                                           
VRUSA    XC    APELEM,APELEM       US AGENCY DATA                               
         LA    R3,APELEM                                                        
         USING CTUSAD,R3                                                        
         MVI   CTUSAEL,CTUSAELQ    X'33'                                        
         MVI   CTUSALEN,CTUSALN2   LENGTH W/VENDOR NAME                         
*                                                                               
         GOTO1 AFVAL,IDPDARPH      DARE PARTNER ID                              
         BNE   VRUSAX              IF NONE, THEN FORGET OTHER DARE INFO         
         GOTOR TSTALPNM,APPARM,(L'IDPDARP,IDPDARP) TEST ALPHANUMERIC            
         BNE   EIIF                                                             
*                                                                               
         MVC   CTUSADPI,FVIFLD                                                  
*                                                                               
         GOTO1 AFVAL,IDPDARMH      MQ SERIES ID NUMBER                          
         BE    *+16                                                             
         CLI   CTUSADPI,C'M'       IS DARE PARTNER CODE 'M' (FOR MQ)?           
         BE    EMIF                YES, THEN MQID# IS REQUIRED                  
         B     VRUSA2              NO, CHECK ROUTING CODE                       
*                                                                               
         TM    FVIIND,FVINUM       CHECK FOR NUMERIC INPUT                      
         BZ    EFNN                                                             
         OC    SCFULL(4),SCFULL    NUMBER IN SCFULL FROM AFVAL                  
         BZ    EIIF                CHECK NUMBER IN RANGE                        
         OC    SCFULL(2),SCFULL                                                 
         BNZ   EFTB                CHECK NUMBER IN RANGE                        
         CLI   CTUSADPI,C'M'       IS DARE PARTNER CODE 'M' (FOR MQ)?           
         BNE   EMQI                NO -- NO MQID ALLOWED                        
*                                                                               
         MVC   IOKEYSAV,IOKEY                                                   
         LA    RE,IOKEY                                                         
         USING MQDEFD,RE           R2=A(MQ DEFINITION RECORD)                   
         XC    MQDKEY,MQDKEY       BUILD KEY                                    
         MVI   MQDKSYS,MQDKSYSQ    MQ DEFINITION RECORD TYPE                    
         MVI   MQDKTYP,MQDKTYPQ                                                 
         MVC   MQDAPPL,=CL8'DARE'  APPLICATION: DARE                            
         MVI   MQDOTYP,MQDOTYPQ    OBJECT TYPE: QUEUE                           
         MVC   MQDID,SCFULL+2      QUEUE ID NUMBER                              
         XC    MQDID,=X'FFFF'      NUMBER IS STORED IN 1'S COMPLEMENT           
         DROP  RE                                                               
         GOTO1 AIO,IORD+IOGENDIR+IO3                                            
         BL    EIIO                I/O ERROR                                    
         BNE   ERNF                RECORD NOT FOUND                             
         MVC   CTUSADMQ,SCFULL+2   KEY FOUND: ID NUMBER IS OK                   
         MVC   IOKEY,IOKEYSAV                                                   
*                                                                               
VRUSA2   GOTO1 AFVAL,IDPDARRH      DARE ROUTING CODE                            
         BNE   VRUSA3              STILL NEED TO CHANGE THE RECORD              
         GOTOR TSTALPNM,APPARM,(L'IDPDARR,IDPDARR) TEST ALPHANUMERIC            
         BNE   EIIF                                                             
         MVC   CTUSADRC,FVIFLD                                                  
*                                                                               
VRUSA3   NI    CTUSADFL,X'FF'-CTUSADFX                                          
         GOTO1 AFVAL,IDPDARXH      DARE XML USER?                               
         BNE   VRUSA5              STILL NEED TO CHANGE THE RECORD              
         CLI   FVIFLD,C'Y'                                                      
         BE    VRUSA4                                                           
         CLI   FVIFLD,C'N'                                                      
         BNE   EIIF                                                             
         B     VRUSA5                                                           
*                                                                               
VRUSA4   OI    CTUSADFL,CTUSADFX                                                
*                                                                               
VRUSA5   GOTO1 AFVAL,IDPDARVH      DARE VENDOR CODE                             
         BNE   VRUSA6                                                           
*                                                                               
         MVC   IOKEYSAV,IOKEY                                                   
         LA    RE,IOKEY                                                         
         USING CTEVKEY,RE                                                       
         XC    CTEVKEY,CTEVKEY                                                  
         MVI   CTEVKTYP,CTEVKTYQ   X'00'                                        
         MVI   CTEVKSTY,CTEVKSTQ   X'38'                                        
         MVC   CTEVKVND,FVIFLD                                                  
         GOTO1 AIO,IORD+IOGENDIR+IO3                                            
         BL    EIIO                I/O ERROR                                    
         BNE   ERNF                RECORD NOT FOUND                             
         MVC   CTUSAVEN,FVIFLD                                                  
         MVC   IOKEY,IOKEYSAV                                                   
*                                                                               
VRUSA6   GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
VRUSAX   EQU   *                                                                
*&&                                                                             
VROTHX   B     VRUPD                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORD                                                       *         
***********************************************************************         
         SPACE 1                                                                
VRUPD    GOTO1 ASETACN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         LA    R1,GENACTH          SET CURSOR IN CASE OF ERROR                  
         ST    R1,FVADDR                                                        
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         MVC   KEYSAVE,IOKEY                                                    
         L     RF,=A(CHKBIG)       CHECK RECORD WILL BE TOO BIG                 
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   VALRECER            EXIT WITH ERROR MESSAGE                      
*                                  WRITE ID RECORD                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,IDNUM                                                    
*                                  WRITE ID# PASSIVE RECORD                     
         MVI   APELEM,X'02'                                                     
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTIREC                                                   
         MVI   APELEM,X'02'                                                     
         MVI   APELEM+1,X'0C'                                                   
         MVC   APELEM+2(10),KEYSAVE+CTIKID-CTIKEY                               
         GOTO1 AADDELN,CTIREC                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         L     R2,AIOAREA2                                                      
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R2,AIOAREA1                                                      
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VALRECX                                                          
*                                  EXIT RECORD VALIDATION AND UPDATE OK         
VALRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECER B     EXIT                                                             
         SPACE 1                                                                
TSTALPNM NTR1                                                                   
         LLC   R0,0(R1)            RO=L'FIELD                                   
         ICM   R2,7,1(R1)          R2=A(FIELD)                                  
TAN010   CLI   0(R2),C' '                                                       
         BE    TAN020                                                           
         CLI   0(R2),C'A'                                                       
         BL    TANNO                                                            
         CLI   0(R2),C'I'                                                       
         BNH   TAN020                                                           
         CLI   0(R2),C'J'                                                       
         BL    TANNO                                                            
         CLI   0(R2),C'R'                                                       
         BNH   TAN020                                                           
         CLI   0(R2),C'S'                                                       
         BL    TANNO                                                            
         CLI   0(R2),C'Z'                                                       
         BNH   TAN020                                                           
         CLI   0(R2),C'0'                                                       
         BL    TANNO                                                            
         CLI   0(R2),C'9'                                                       
         BH    TANNO                                                            
TAN020   AHI   R2,1                                                             
         JCT   R0,TAN010                                                        
TANYES   B     EXITY                                                            
TANNO    B     EXITN                                                            
*                                                                               
* ROUTINE TO CHECK FOR WILD CARD STYLE USERID IN COMPATIBLE ID LIST             
* R4=A(SCANNER CONTROL BLOCK)                                                   
* IF WILDCARD (I.E. STRING ENDS WITH '*') RETURN IN LIDSAVE                     
* AND RETURN CC .EQ.                                                            
*                                                                               
WILDCARD NTR1                                                                   
         ZIC   RF,0(R4)                                                         
         LA    R1,11(RF,R4)                                                     
WCAR010  CLI   0(R1),C'*'                                                       
         BE    WCAR020                                                          
         CLI   0(R1),C' '                                                       
         BE    WCAR012                                                          
         B     WCARNO                                                           
WCAR012  BCTR  R1,0                                                             
         BCT   RF,WCAR010                                                       
         B     WCARNO                                                           
WCAR020  EQU   *                                                                
         BCTR  RF,0                                                             
         STC   RF,WILDCLEN                                                      
         B     WCAROK                                                           
WCAROK   SR    RC,RC                                                            
WCARNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF USER ID RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
DISKEY   LA    R2,APRECKEY                                                      
         MVC   IDPIDA,CTIKID                                                    
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY USER ID RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
DISREC   L     R2,AIOAREA1                                                      
         TWAXC IDPDNAMH                                                         
         XC    DUB,DUB                                                          
         LA    R1,REC                                                           
         ST    R1,DUB1                                                          
         LA    R1,REC+500                                                       
         ST    R1,DUB2                                                          
*                                  CLEAR REC WORK AREA (C++ BUG)                
         LA    RE,REC                                                           
         ICM   RF,15,=AL4(L'REC)                                                
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         MVI   IDOPTF2,0                                                        
         XC    PNMDID,PNMDID                                                    
         LA    R3,CTIDATA                                                       
*                                                                               
DRDAT10  CLI   0(R3),0                                                          
         BE    DRDAT30                                                          
         CLI   0(R3),X'30'         DESTINATION DETAIL                           
         BE    DRDDT                                                            
         CLI   0(R3),X'33'         UK/US AGENCY VALUES                          
         BE    DROTH                                                            
         CLI   0(R3),X'34'         DESTINATION ID                               
         BE    DRDEI                                                            
         CLI   0(R3),X'36'         ORIGIN DETAIL                                
         BE    DRORG                                                            
         CLI   0(R3),X'3A'         PRINTER ID                                   
         BE    DRPID                                                            
         CLI   0(R3),X'42'         OUTPUT TYPE                                  
         BE    DROUT                                                            
         CLI   0(R3),X'4C'         SHIPPING INFO                                
         BE    DRSHI                                                            
         CLI   0(R3),CTIDOELQ      ID OPTIONS ELEMENT                           
         BE    DRIDO                                                            
         CLI   0(R3),CTPNIELQ      PRINTER NAME DEAULT ID ELEMENT               
         BE    DRPNI                                                            
DRDAT20  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DRDAT10                                                          
*                                                                               
*              AT END OF ELEMENT DISPLAY                                        
*              DISPLAY PRINTER/ID CHAINS                                        
*                                                                               
DRDAT30  L     R4,DUB1+4                                                        
         LTR   R4,R4                                                            
         BZ    DRDAT40                                                          
         GOTO1 =V(SCINKEY),APPARM,(3,IDPDIDAH),(12,REC),(R4),RR=RB              
         SPACE 1                                                                
DRDAT40  L     R4,DUB2+4                                                        
         LTR   R4,R4                                                            
         BZ    DRDAT50                                                          
         GOTO1 =V(SCINKEY),APPARM,(3,IDPDPTAH),(12,REC+500),(R4),RR=RB          
         SPACE 1                                                                
DRDAT50  EQU   *                                                                
         TM    IDOPTF2,CTIDOFPN                                                 
         BZ    DRDATX                                                           
         MVC   IDPDPTA(5),=CL5'PNAME'                                           
         TM    IDOPTF2,CTIDOFPP+CTIDOFPI                                        
         BZ    DRDATX                                                           
         MVC   IDPDPTA+5(9),=CL9',DEFAULT='                                     
         TM    IDOPTF2,CTIDOFPP                                                 
         BZ    *+14                                                             
         MVC   IDPDPTA+14(9),=CL9'PRINCIPAL'                                    
         B     DRDATX                                                           
         TM    IDOPTF2,CTIDOFPI                                                 
         BZ    DRDATX                                                           
         L     R2,AIOAREA2         SWITCH I/O AREAS                             
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF USERID RECORD                   
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID+8(2),PNMDID                                               
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   1(R3),12                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         MVC   IDPDPTA+14(10),2(R3)                                             
         B     DRDATX                                                           
         SPACE 1                                                                
DRDATX   GOTO1 ADISACT,CTIREC                                                   
         B     EXIT                                                             
         EJECT                                                                  
*              DISPLAY DESTINATION DETAIL ELEMENT                               
*                                                                               
         USING CTDSTD,R3                                                        
DRDDT    MVC   IDPDNAM,CTDSTNAM                                                 
         MVI   IDPDNAMH+5,33                                                    
         MVI   IDPDNAMH+7,33                                                    
         OI    IDPDNAMH+6,X'80'                                                 
         MVC   IDPDADD,CTDSTADD                                                 
         MVI   IDPDADDH+5,33                                                    
         MVI   IDPDADDH+7,33                                                    
         OI    IDPDADDH+6,X'80'                                                 
         MVC   IDPDAD2,SPACES                                                   
         MVI   IDPDAD2H+5,33                                                    
         MVI   IDPDAD2H+7,33                                                    
         OI    IDPDAD2H+6,X'80'                                                 
         MVC   IDPDAD3,SPACES                                                   
         MVI   IDPDAD3H+5,33                                                    
         MVI   IDPDAD3H+7,33                                                    
         OI    IDPDAD3H+6,X'80'                                                 
         CLI   CTDSTLEN,166                                                     
         BL    DRDDT10                                                          
         MVC   IDPDAD2,CTDSTAD2                                                 
         MVC   IDPDAD3,CTDSTAD3                                                 
DRDDT10  MVC   IDPDLO1,CTDSTLG1                                                 
         MVI   IDPDLO1H+5,7                                                     
         MVI   IDPDLO1H+7,7                                                     
         OI    IDPDLO1H+6,X'80'                                                 
         OC    CTDSTLG2,CTDSTLG2                                                
         BZ    DRDDT20                                                          
         MVC   IDPDLO2,CTDSTLG2                                                 
         MVI   IDPDLO2H+5,7                                                     
         MVI   IDPDLO2H+7,7                                                     
         OI    IDPDLO2H+6,X'80'                                                 
         SPACE 1                                                                
DRDDT20  MVC   IDPDPWR,CTDSTPOW                                                 
         MVI   IDPDPWRH+5,4                                                     
         MVI   IDPDPWRH+7,4                                                     
         OI    IDPDPWRH+6,X'80'                                                 
         SPACE 1                                                                
DRDDT30  MVC   IDPDLFC,CTDSTLFC                                                 
         MVI   IDPDLFCH+5,2                                                     
         MVI   IDPDLFCH+7,2                                                     
         OI    IDPDLFCH+6,X'80'                                                 
         B     DRDAT20                                                          
*                                                                               
*              DISPLAY ORIGIN DETAIL ELEMENT                                    
*                                                                               
         USING CTORGD,R3                                                        
DRORG    MVC   IDPONAM,CTORGNAM                                                 
         MVI   IDPONAMH+5,33                                                    
         MVI   IDPONAMH+7,33                                                    
         OI    IDPONAMH+6,X'80'                                                 
         MVC   IDPOADD,CTORGADD                                                 
         MVI   IDPOADDH+5,33                                                    
         MVI   IDPOADDH+7,33                                                    
         OI    IDPOADDH+6,X'80'                                                 
         B     DRDAT20                                                          
         EJECT                                                                  
*              ADD ID TO DISPLAY STREAM                                         
*                                                                               
         USING CTVALD,R3                                                        
DRDEI    LM    RE,RF,DUB1                                                       
         MVC   0(12,RE),=CL12' '                                                
         MVC   0(10,RE),CTVALDST                                                
         OC    0(2,RE),0(RE)                                                    
         BNZ   *+10                                                             
         MVC   0(2,RE),=C'L='                                                   
*                                                                               
         CLC   0(2,RE),=X'0001'                                                 
         BNE   *+10                                                             
         MVC   0(2,RE),=C'A='                                                   
*                                                                               
         LA    RE,12(RE)                                                        
         LA    RF,1(RF)                                                         
         STM   RE,RF,DUB1                                                       
         B     DRDAT20                                                          
*                                                                               
*              ADD PRINTER TO STREAM                                            
*                                                                               
         USING CTPRND,R3                                                        
DRPID    LM    RE,RF,DUB2                                                       
         MVC   0(12,RE),=CL12' '                                                
         OC    CTPRNNUM(2),CTPRNNUM                                             
         BZ    DRPID10                                                          
         EDIT  (B1,CTPRNNUM),(3,0(RE)),ALIGN=LEFT                               
         LR    R4,RE                                                            
         AR    R4,R0                                                            
         MVI   0(R4),C'='                                                       
         MVC   1(8,R4),CTPRNLIN                                                 
DRPID20  LA    RE,12(RE)                                                        
         LA    RF,1(RF)                                                         
         STM   RE,RF,DUB2                                                       
         B     DRDAT20                                                          
         SPACE 1                                                                
DRPID10  MVC   0(2,RE),=C'L='                                                   
         MVC   2(6,RE),CTPRNLIN+1                                               
         B     DRPID20                                                          
         EJECT                                                                  
*                                                                               
*              DISPLAY OUTPUT TYPE ELEMENT                                      
*                                                                               
         USING CTOCOD,R3                                                        
DROUT    MVC   IDPOUTI,CTOCODE                                                  
         MVI   IDPOUTIH+5,10                                                    
         MVI   IDPOUTIH+7,10                                                    
         OI    IDPOUTIH+6,X'80'                                                 
         B     DRDAT20                                                          
*                                                                               
*              DISPLAY SHIPPING INFO ELEMENT                                    
*                                                                               
         USING CTSHPD,R3                                                        
DRSHI    SR    R8,R8                                                            
         IC    R8,CTSHPLEN                                                      
         SH    R8,=H'6'                                                         
         SR    R4,R4                                                            
         CH    R8,=H'60'                                                        
         BNH   *+12                                                             
         LR    R4,R8                                                            
         LA    R8,60                                                            
         SR    R4,R8                                                            
*&&US                                                                           
         TM    6(R3),X'80'         TEST SPECIAL NUMERIC SHIPPING UNIT           
         BO    DRSHI10                                                          
         GOTO1 ADISPSHP,APPARM,IDPDSHA,6(R3),(R8)                               
         ZIC   R8,8(R1)                                                         
         B     DRSHI20                                                          
*&&                                                                             
*                                                                               
DRSHI10  BCTR  R8,0                OTHERWISE MOVE IN AS IS                      
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   IDPDSHA(0),6(R3)                                                 
         LA    R8,1(R8)                                                         
*                                                                               
DRSHI20  STC   R8,IDPDSHAH+7                                                    
         LTR   R4,R4                                                            
         BZ    DRDAT20                                                          
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   IDPDSHB(0),66(R3)                                                
         LA    R4,1(R4)                                                         
         STC   R4,IDPDSHBH+7                                                    
         B     DRDAT20                                                          
*                                                                               
DROTH    EQU   *                                                                
*&&UK                                                                           
         USING CTUKAD,R3           DISPLAY UK AGENCY DATA FIELDS                
DRUKV    EQU   *                                                                
         MVC   IDPPLNO,CTUKALIN                                                 
         OI    IDPPLNOH+6,X'80'                                                 
         MVC   IDPIPA,CTUKAIPA                                                  
         OI    IDPIPAH+6,X'80'                                                  
         B     DRDAT20                                                          
*&&                                                                             
*&&US                                                                           
         USING CTUSAD,R3           DISPLAY US AGENCY DATA FIELDS                
DRUSV    MVC   IDPDARP,CTUSADPI    DARE PARTNER ID                              
         OI    IDPDARPH+6,X'80'                                                 
         SR    RE,RE                                                            
         ICM   RE,3,CTUSADMQ                                                    
         EDIT  (RE),IDPDARM,ALIGN=LEFT  DARE MQ SERIES ID NUMBER                
         OI    IDPDARMH+6,X'80'                                                 
         MVC   IDPDARR,CTUSADRC    DARE ROUTING CODE                            
         OI    IDPDARRH+6,X'80'                                                 
*                                                                               
         MVC   IDPDARV,SPACES      DARE VENDOR CODE                             
         OI    IDPDARVH+6,X'80'                                                 
         CLI   CTUSALEN,CTUSALN2                                                
         BL    DRUSV1                                                           
         CLC   CTUSAVEN,SPACES                                                  
         BL    DRUSV1                                                           
         MVC   IDPDARV,CTUSAVEN                                                 
*                                                                               
DRUSV1   OI    IDPDARXH+6,X'80'                                                 
         MVI   IDPDARX,C'N'        DARE XML USER? DEFAULT = NO                  
         TM    CTUSADFL,CTUSADFX                                                
         BZ    DRDAT20                                                          
         MVI   IDPDARX,C'Y'        DARE XML USER?                               
         OI    IDPDARXH+6,X'80'                                                 
         B     DRDAT20                                                          
*&&                                                                             
*                                                                               
         USING CTIDOD,R3                                                        
DRIDO    MVC   IDOPTF2,CTIDOFL2                                                 
         B     DRDAT20                                                          
*                                                                               
         USING CTPNID,R3                                                        
DRPNI    MVC   PNMDID,CTPNIUID                                                  
         B     DRDAT20                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
VALSEL   LA    R2,APRECKEY                                                      
         XC    SELDATA,SELDATA                                                  
*                                                                               
         LA    R4,LSTIDH                                                        
         BRAS  RE,VALPARS          GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
         XC    CTIKEY,CTIKEY       BUILD AN INITIAL KEY                         
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SELID                                                     
*                                                                               
         MVI   GETSEQF,0           INTERNAL READ SEQUENCE FLAG                  
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
GETSEL   LA    R2,IOKEY            READ NEXT LIST RECORD                        
         MVC   CTIKEY,APRECKEY       FROM LAST SAVED KEY                        
         TM    GETSEQF,APILRERD    TEST GETSEL READ SEQUENCE BROKEN             
         BZ    GSEL02                                                           
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     GSEL04                                                           
GSEL02   TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    GSEL10                                                           
         NI    APINDS,X'FF'-APILRERD                                            
GSEL04   GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GSEL20                                                           
         B     GETSELX                                                          
GSEL10   TM    APINDS,APILNSEQ     TEST FIRST LINE IN LIST SEQUENCE             
         BNZ   GSEL20                                                           
*                                  READ PAST PASSIVE # RECORDS                  
         OC    CTIKID(L'CTIKID-L'CTIKNUM),CTIKID                                
         BNZ   *+8                                                              
         MVI   CTIKID+L'CTIKID-L'CTIKNUM-1,1                                    
*                                                                               
         LA    R1,IOCONFIL+IOHI+IO1                                             
         L     RF,=A(GETREC)       GO SELECT NEXT RECORD                        
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   GETSELX                                                          
         B     GETSELM                                                          
GSEL20   LA    R1,IOCONFIL+IOSQ+IO1                                             
         L     RF,=A(GETREC)       GO SELECT NEXT RECORD                        
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   GETSELX             (EOF)                                        
*                                                                               
GETSELM  CLI   OPTOUT,0                                                         
         BE    GETSELY                                                          
         L     R2,AIOAREA1                                                      
         MVI   APELEM,CTOCOELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    *+14                                                             
         USING CTOCOD,R3                                                        
         CLC   CTOCODE,OPTOUT                                                   
         BE    GETSELY                                                          
         LA    R2,IOKEY                                                         
         B     GSEL20                                                           
*                                                                               
GETSELY  EQU   *                                                                
*                                                                               
         L     R2,AIOAREA1                                                      
*                                                                               
         MVC   APRECKEY(L'CTIKEY),CTIKEY                                        
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
DISSEL   EQU   *                                                                
*                                                                               
         L     R4,APPARM                                                        
         L     RF,=A(LINE)                                                      
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
DISSELX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
*                                                                               
         LA    R4,REPIDH                                                        
         BRAS  RE,VALPARS          GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
*&&US                                                                           
         GOTO1 AFVAL,REPRTCDH      DARE ROUTING CODE FILTER                     
         BNE   VALREQ10                                                         
         CLI   FVIFLD,C'Y'         Y = ONLY SHOW RECS W/ ROUTING CODES          
         BNE   *+12                                                             
         MVI   SELRTCD,C'Y'                                                     
         B     VALREQ10                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
VALREQ10 EQU   *                                                                
*&&                                                                             
*                                                                               
         LA    R2,APRECKEY         BUILD AN INITIAL KEY                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SELID                                                     
*                                                                               
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC                                                
         MVI   REPMIDSI,REPMSPAC                                                
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT USER ID LIST                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
*                                                                               
PRTREP   L     R9,AREP                                                          
*                                                                               
         CLC   =C'DOWN',REPOTYP                                                 
         BNE   PREP04                                                           
         CLC   =C'NOW',REPWHEN     ONLY PRINT THIS FOR NOW REQUEST              
         BNE   PREP02                                                           
         XC    REPAPHS,REPAPHS     CLEAR SPECS                                  
         MVC   REPP1(17),=C'IDI DATA DOWNLOAD'                                  
         MVI   REPLINE,1                                                        
         GOTO1 VREPORT,REPD                                                     
         MVI   REPLINE,99                                                       
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         L     RF,=V(DLFLD)                                                     
         A     RF,APRELO                                                        
         ST    RF,VDLFLD                                                        
*                                                                               
PREP02   XC    D.DLCBD(DLCBL),D.DLCBD                                           
         MVI   D.DLCBACT,DLCBINIT         DOWNLOAD ACTION IS START              
         LARL  RF,DLHOOK                  DUMMY HOOK                            
         ST    RF,D.DLCBAPR                                                     
         LA    RF,REPP1                                                         
         ST    RF,D.DLCBAPL                                                     
         LA    RF,L'REPP1                                                       
         STH   RF,D.DLCXMAXL                                                    
         MVI   D.DLCXDELC,C' '            DELIMITER                             
         MVI   D.DLCXEOTC,C'"'            TEXT DELIMITER                        
         MVI   D.DLCXEOLC,X'5E'           SEMI-COLON, END-OF-LINE               
         MVI   D.DLCXEORC,C':'            END-OF-REPORT                         
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
         MVC   D.DLCBFLD,SPACES           MUST CLEAR FIRST TIME                 
         MVI   D.DLCBFLX,C' '                                                   
         MVC   D.DLCBFLX+1(L'DLCBFLX-1),D.DLCBFLX                               
*                                                                               
PREP04   L     R9,AREP                                                          
         LA    R2,IOKEY                                                         
         MVC   CTIKEY,APRECKEY                                                  
*                                  READ PAST PASSIVE # RECORDS                  
         OC    CTIKID(L'CTIKID-L'CTIKNUM),CTIKID                                
         BNZ   *+8                                                              
         MVI   CTIKID+L'CTIKID-L'CTIKNUM-1,1                                    
*                                                                               
         LA    R1,IOHI+IOCONFIL+IO1                                             
         L     RF,=A(GETREC)         GO GET REC WIV                             
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   PRTREPX                                                          
         B     PREP100                                                          
*                                                                               
PREP010  TM    GETSEQF,APILRERD    TEST GETSEL READ SEQUENCE BROKEN             
         BZ    PREP020                                                          
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     PREP030                                                          
*                                                                               
PREP020  TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    PREP040                                                          
         NI    APINDS,X'FF'-APILRERD                                            
*                                                                               
PREP030  LA    R2,IOKEY                                                         
         MVC   CTIKEY,APRECKEY                                                  
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    PREP040                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     PRTREPX                                                          
*                                                                               
PREP040  LA    R1,IOSQ+IOCONFIL+IO1                                             
         L     RF,=A(GETREC)       GO GET NEXT REC                              
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   PRTREPX                                                          
*                                                                               
PREP100  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTIKEY),CTIKEY                                        
         CLC   =C'DOWN',REPOTYP                                                 
         BE    PREP110                                                          
         LA    R4,REPP1-14                                                      
         LARL  RF,LINE                                                          
         BASR  RE,RF               GO BUILD A PRINT LINE                        
         GOTO1 VREPORT,REPD                                                     
         B     PREP010                                                          
*                                                                               
PREP110  L     R4,AIOAREA3                                                      
         LARL  RF,DLLINE                                                        
         BASR  RE,RF               GO DOWNLOAD AND PRINT                        
         B     PREP010                                                          
*                                                                               
PRTREPX  CLC   =C'DOWN',REPOTYP                                                 
         BNE   PRTREPX2                                                         
*                                                                               
         MVI   D.DLCBACT,DLCBEOR          END OF REPORT                         
         GOTO1 VDLFLD,DLCB                                                      
PRTREPX2 B     EXIT                                                             
         EJECT                                                                  
*                                  GETTXT MESSAGE # ERROR EXITS                 
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EFTL     MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
EFNN     MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
EFTS     MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
EFNH     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EMIF     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
EIIO     MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
EDIF     MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
ERAE     MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
EIRT     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  ?? ALREADY EXISTS                            
ERTB     MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  RECORD TOO BIG                               
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE > MAX                            
ENPN     MVC   FVMSGNO,=AL2(CE#NUMPN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  NUMERIC ID INVALID WITH PNAME                
ETOO     MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     NO                  TOO MANY INPUT FIELDS                        
EIPI     MVC   FVMSGNO,=AL2(CE#INVPI)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  INVALID PRINTER ID                           
EDNM     MVC   FVMSGNO,=AL2(CE#DENUM)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  DEFAULT INVALID WITH NUMERICID               
EDPN     MVC   FVMSGNO,=AL2(CE#DEPNM)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  DEFAULT INVALID WITHOUT PNAME                
EIID     MVC   FVMSGNO,=AL2(CE#INVID)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  INVALID USER ID                              
EPND     MVC   FVMSGNO,=AL2(CE#PIDND)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PRINCIPAL ID NOT DEFINED                     
EIDI     MVC   FVMSGNO,=AL2(CE#DEFVP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  DEFAULT= REQUIRES VALID PARAMETER            
EMQI     MVC   FVMSGNO,=AL2(CE#MQIDQ)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  MQID ONLY ALLOWED WITH MQ PARTNER            
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
REPDESCL DC    C'USER ID LIST'                                                  
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'USER ID LIST'                                            
         SPEC  H2,57,C'------------'                                            
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
*                                  HEADINGS FOR REPORT SCREEN                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    80C' '                                                           
FFILL    DC    80X'FF'                                                          
XAUTH    DC    XL2'FFFF'                                                        
YAUTH    DC    XL2'000F'                                                        
NAUTH    DC    XL2'0000'                                                        
CAPFILL  DC    (L'APWORK)X'40'                                                  
                                                                                
**********************************************************************          
* VALIDATE I/P PARAMETERS FOR LIST/REPORT                            *          
* R4 = A(FIRST FIELD HEADER IN STANDARD DISPLAY)                     *          
*   APPLICABLE TO BOTH LIST AND REPORT SCREEN FIELD OFFSETS          *          
**********************************************************************          
         USING CTIREC,R2                                                        
         USING LSTIDH,R4                                                        
VALPARS  NTR1  BASE=*,LABEL=*                                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VPID     GOTO1 AFVAL,LSTIDH        STORE USER ID                                
         BNE   VPIDX               (IF ENTERED)                                 
         ZIC   R1,FVILEN                                                        
         LHI   RE,-1                                                            
         LA    RF,FVIFLD                                                        
VPID1    CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VPID2               FOR KEY COMPARE IN GETREC                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VPID1                                                         
VPID2    STC   RE,SELKEYCL                                                      
         MVC   SELID,FVIFLD                                                     
         MVC   SELIDL,FVILEN                                                    
         MVC   SELIDSP,0(RF)                                                    
VPIDX    EQU   *                                                                
*                                                                               
VPSYS    GOTO1 AFVAL,LSTSYSH       VALIDATE SYSTEM                              
         BNE   VPSYSX                                                           
         L     RF,ASYSLST          LOOK UP NAME IN SYSLST                       
         LA    RF,6(,RF)                                                        
         USING SYSLSTD,RF                                                       
         ZIC   RE,FVILEN                                                        
         BCTR  RE,0                                                             
VPSYS3   CLI   SYSLNUM,0                                                        
         BE    VPSYS1                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),FVIFLD                                               
         BE    VPSYS4                                                           
         LA    RF,SYSLLEN(,RF)                                                  
         B     VPSYS3                                                           
VPSYS4   MVC   SELSYS,SYSLNUM                                                   
         B     VPSYSX                                                           
VPSYS1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPSYSX   EQU   *                                                                
*                                                                               
VPPGM    EQU   *                   VALIDATE PROGRAM                             
         GOTO1 AFVAL,LSTPGMH                                                    
         BNE   VPPGMX                                                           
         OC    SELSYS,SELSYS                                                    
         BNZ   VPPGM2                                                           
         MVC   FVMSGNO,=AL2(CE#PFNVS)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     VALPARSX            PROGRAM NAME INVALID WITHOUT SYS             
VPPGM2   GOTO1 AVALPGM,APPARM,(SELSYS,LSTPGMH)                                  
         BNE   VPPGM1                                                           
         MVC   SELPGM,APWORK                                                    
         B     VPPGMX                                                           
VPPGM1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPPGMX   EQU   *                                                                
*                                                                               
VPAGY    EQU   *                   VALIDATE AGENCY ALPHA ID                     
         GOTO1 AFVAL,LSTAGYAH                                                   
         BNE   VPAGYX                                                           
         USING CT5REC,R1                                                        
         LA    R1,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         DROP  R1                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    VPAGY2                                                           
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALPARSX            ACCESS RECORD NOT FOUND                      
VPAGY2   MVC   SELAGY,FVIFLD                                                    
         B     VPAGYX                                                           
VPAGY1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPAGYX   EQU   *                                                                
*                                                                               
VPPWD    EQU   *                   VALIDATE PASSWORD REQUIRED                   
         GOTO1 AFVAL,LSTPWDH                                                    
         BNE   VPPWDX                                                           
         CLI   FVIFLD,C'Y'                                                      
         BE    VPPWD2                                                           
         CLI   FVIFLD,C'N'                                                      
         BNE   VPPWD1                                                           
VPPWD2   MVC   SELPWD,FVIFLD                                                    
         B     VPPWDX                                                           
VPPWD1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPPWDX   EQU   *                                                                
*                                                                               
VPCID    EQU   *                   VALIDATE COMPATIBLE USER ID                  
         GOTO1 AFVAL,LSTCIDH                                                    
         BNE   VPCIDX                                                           
         OC    SELAGY,SELAGY                                                    
         BNZ   VPCID2                                                           
         MVC   FVMSGNO,=AL2(CE#CIDAG)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     VALPARSX            PROGRAM NAME INVALID WITHOUT SYS             
VPCID2   L     R2,AIOAREA2         SWITCH IO AREAS                              
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,FVIFLD                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   VPCID1                                                           
         MVC   IOKEY,APRECKEY                                                   
         MVC   SELCID,CTIKID                                                    
         B     VPCIDX                                                           
VPCID1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPCIDX   EQU   *                                                                
*                                                                               
VPLUID   EQU   *                   VALIDATE PRINTER/SHUTTLE LUID                
         GOTO1 AFVAL,LSTLUIDH                                                   
         BNE   VPLUIDX                                                          
         L     R2,AIOAREA2         SWITCH IO AREAS                              
         USING CTTREC,R2                                                        
         XC    CTTKEY,CTTKEY       BUILD KEY OF LUID RECORD                     
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKTID,FVIFLD                                                   
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   VPLUID1                                                          
         MVC   IOKEY,APRECKEY                                                   
         MVC   SELLUID,CTTKTID                                                  
         B     VPLUIDX                                                          
VPLUID1  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPLUIDX  EQU   *                                                                
*                                                                               
VPPWR    EQU   *                   VALIDATE POWER CODE                          
         GOTO1 AFVAL,LSTPWRH                                                    
         BNE   VPPWRX                                                           
         MVC   SELPWR,FVIFLD                                                    
         B     VPPWRX                                                           
VPPWRX   EQU   *                                                                
*                                                                               
VPLFC    EQU   *                   VALIDATE LASER FORM CHECKS                   
         GOTO1 AFVAL,LSTLFCH                                                    
         BNE   VPLFCX                                                           
         MVC   SELLFC,FVIFLD                                                    
         B     VPLFCX                                                           
VPLFCX   EQU   *                                                                
*&&US                                                                           
VPDAR    EQU   *                   VALIDATE DARE FILTER                         
         XC    SELDAR,SELDAR                                                    
         GOTO1 AFVAL,LSTDARH                                                    
         BNE   VPDARX                                                           
         MVC   SELDAR,FVIFLD                                                    
*                                                                               
         CLC   SELDAR(3),=C'MQ='      FILTER ON MQID#                           
         BNE   VPDARX                                                           
         CLI   FVILEN,4               MUST BE MQ=#                              
         BL    VPDARER                                                          
         LHI   RE,5                   MAX OF 5 DIGITS                           
         LA    RF,SELDAR+3            AFTER "MQ="                               
VPDAR2   CLI   0(RF),C' '                                                       
         BNH   VPDAR4                                                           
         CLI   0(RF),C'9'                                                       
         BH    VPDARER                                                          
         CLI   0(RF),C'0'                                                       
         BL    VPDARER                MUST BE NUMERIC                           
         LA    RF,1(,RF)                                                        
         BCT   RE,VPDAR2                                                        
VPDAR4   LLC   R1,FVXLEN                                                        
         AHI   R1,-3                  MINUS THE "MQ="                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SELDAR+3(0)                                                  
         CVB   R1,DUB                                                           
         STCM  R1,3,SELDARN           HALF WORD MQID#                           
         B     VPDARX                                                           
*                                                                               
VPDARER  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPDARX   EQU   *                                                                
*&&                                                                             
VALPARSX XIT1  ,                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
         SPACE 1                                                                
         DROP  RB,RA                                                            
         SPACE 1                                                                
**********************************************************************          
* GET NEXT RECORD FOR LIST/REPORT, FILTERING ON I/P PARAMETERS       *          
**********************************************************************          
         SPACE 1                                                                
         USING CTIREC,R2                                                        
GETREC   NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETREC,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GRE'    INSERT NAME                                  
*                                                                               
         B     GETRECIO            PRESERVE VALUE OF R1 ON ENTRY                
GETRECRD TM    GETSEQF,APILRERD    READ NEXT RECORD                             
         BZ    GETRECSQ            CHECK SEQUENCE BROKEN                        
         NI    GETSEQF,X'FF'-APILRERD                                           
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETRECN                                                          
GETRECSQ LA    R1,IOCONFIL+IOSQ+IO1                                             
GETRECIO GOTO1 AIO                                                              
         BNE   GETRECN                                                          
         L     R2,AIOAREA1                                                      
         OC    CTIKID-CTIKEY(10,R2),CTIKID-CTIKEY(R2)                           
         BZ    GETRECN                                                          
*                                  TEST FOR MAX IOS                             
         CLI   APACTN,ACTREP       TEST REPORT MODE                             
         BNE   GREC010                                                          
         CLI   INWHEN,MIXIOKN      TEST NOW MODE                                
         BNE   GREC020                                                          
*                                                                               
GREC010  GOTO1 VGETFACT,APPARM,0   GET A(SYSTEM INFO BLOCK)                     
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MH    RF,=H'9'                                                         
         D     RE,=F'10'           90 PERCENT OF MAX IOS IN RF                  
         CLM   RF,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BH    GREC020                                                          
         MVC   FVMSGNO,=AL2(CE#IOCNT)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    RF,GENACTH                                                       
         ST    RF,APCURSOR                                                      
         B     GETRECNO                                                         
         DROP  R1                                                               
*                                                                               
GREC020  L     R2,AIOAREA1                                                      
*                                  CHECK STILL CORRECT RECORD TYPE              
         CLC   IOKEYSAV(CTIKID-CTIKEY),CTIKEY                                   
         BNE   GETRECN                                                          
         SPACE 1                                                                
*                                  * FILTER ON SELECTION CRITERIA *             
         SPACE 1                                                                
GRID     CLI   SELIDSP,C' '       USER ID - FILTER ONLY IF IT                   
         BNH   GRIDX                 CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GRID1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTIKID(0),SELID                                                  
         BH    GETRECN             (NO MORE RELEVENT RECORDS)                   
GRID1    GOTO1 ATXTFLT,APPARM,(SELIDL,SELID),(8,CTIKID)                         
         BNE   GETRECRD            READ NEXT RECORD                             
GRIDX    EQU   *                                                                
*                                                                               
GRSYS    EQU   *                   FILTER ON SYSTEM                             
         OC    SELSYS,SELSYS                                                    
         BZ    GRPGMX                                                           
         MVI   APELEM,X'21'        GET SYSTEM ELEMS                             
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         B     GRSYS3                                                           
GRSYS2   ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    GETRECRD            READ NEXT RECORD                             
         CLI   0(R3),X'21'                                                      
         BNE   GRSYS2                                                           
         USING CTSYSD,R3                                                        
GRSYS3   CLC   CTSYSNUM,SELSYS                                                  
         BNE   GRSYS2                                                           
GRSYSX   EQU   *                                                                
*                                                                               
GRPGM    EQU   *                   FILTER ON PROGRAM                            
         OC    SELPGM,SELPGM                                                    
         BZ    GRPGMX                                                           
         MVC   PROGRAM,SELPGM                                                   
         LA    R1,CTSYSPGM         POINT TO SYSTEM ELEMENT                      
         ZIC   RE,CTSYSLEN                                                      
*                                  FIND PROGRAM IN ELEMENT                      
GRPGM10  CH    RE,=Y(CTSYSL1Q)                                                  
         BNH   GRPGM30             END OF ELEMENT                               
         CLC   SELPGM,0(R1)                                                     
         BE    GRPGM20             PROGRAM FOUND                                
         LA    R1,L'CTSYSPGM(R1)   GET NEXT PROGRAM                             
         SH    RE,=Y(L'CTSYSPGM)                                                
         B     GRPGM10                                                          
GRPGM20  OC    1(2,R1),1(R1)       CHECK PROGRAM ACCESS=N                       
         BZ    GETRECRD                                                         
         B     GRPGMX                                                           
GRPGM30  OC    CTSYSALL,CTSYSALL   CHECK ALL ACCESS=N                           
         BZ    GETRECRD                                                         
         B     GRPGMX                                                           
GRPGMX   EQU   *                                                                
*                                                                               
GRAGY    EQU   *                   FILTER ON AGENCY ALPHA ID                    
         OC    SELAGY,SELAGY                                                    
         BZ    GRAGYX                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAGYELQ     GET AGENCY ELEMENT                           
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         USING CTAGYD,R3                                                        
         CLC   SELAGY,CTAGYID                                                   
         BNE   GETRECRD                                                         
GRAGYX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
GRPWD    EQU   *                   FILTER ON PASSWORD REQUIRED                  
         OC    SELPWD,SELPWD                                                    
         BZ    GRPWDX                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'07'        GET IDOPTS ELEMENT                           
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         CLI   SELPWD,C'Y'                                                      
         BNE   GRPWD10                                                          
         TM    2(R3),X'80'                                                      
         BZ    GETRECRD                                                         
         B     GRPWDX                                                           
GRPWD10  TM    2(R3),X'80'                                                      
         BNZ   GETRECRD                                                         
GRPWDX   EQU   *                                                                
*                                                                               
GRCID    EQU   *                   FILTER ON COMPATIBLE USER ID                 
         OC    SELCID,SELCID                                                    
         BZ    GRCIDX                                                           
*                                  BUILD COMPATIBLE ID TABLE                    
         OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         GOTO1 VGETIDS,APPARM,(C'C',(R2)),0,VDMGR                               
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'               IO ERROR                                     
         CLI   0(R1),0                                                          
         BE    GETRECRD            NULL TABLE                                   
         L     R1,4(R1)                                                         
GRCID1   CLI   0(R1),X'FF'         END OF TABLE ?                               
         BE    GETRECRD            NO MATCH, READ NEXT RECORD                   
         CLC   0(10,R1),=CL10'ALL'                                              
         BNE   GRCID2                                                           
         TM    CUSTAT,CUSDDS       ONLY VALID FOR DDS TERMINALS                 
         BZ    GRCID3                                                           
         B     GRCIDX                                                           
GRCID2   CLC   0(10,R1),SELCID                                                  
         BE    GRCIDX                                                           
GRCID3   LA    R1,12(R1)           GET NEXT TABLE ENTRY                         
         B     GRCID1                                                           
GRCIDX   EQU   *                                                                
*                                                                               
GRLUID   EQU   *                   FILTER ON PRINTER/SHUTTLE LUID               
         OC    SELLUID,SELLUID                                                  
         BZ    GRLUIDX                                                          
         LR    R3,R2                                                            
         MVI   APELEM,CTPRNELQ     GET LUID ELEMS                               
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         B     GRLUID3                                                          
GRLUID2  ZIC   RE,1(R3)            FOND PRINTER ELEMENTS                        
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    GETRECRD                                                         
         CLI   0(R3),CTPRNELQ                                                   
         BNE   GRLUID2                                                          
         USING CTPRND,R3                                                        
GRLUID3  OC    CTPRNFLG,CTPRNFLG   CHECK IF SYSLIST                             
         BZ    GRLUID4                                                          
         CLC   SELLUID,CTPRNLIN                                                 
         BNE   GRLUID2                                                          
         B     GRLUIDX                                                          
*                                  SEARCH SYSLIST RECORD                        
GRLUID4  OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         L     R8,AIOAREA2         SWITCH I/O AREAS                             
         USING CTWREC,R8                                                        
         XC    CTWKEY,CTWKEY       BUILD KEY OF LIST RECORD                     
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'R'                                                     
         MVC   CTWKID,CTPRNLST                                                  
         MVC   IOKEY(L'CTWKEY),CTWKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    *+14                CHECK RECORD FOUND OK                        
         MVC   FVMSGNO,=AL2(FVFOK)   ELSE IGNORE AIO ERROR MESSAGE              
         B     GRLUID2                                                          
         MVI   APELEM,CTLSTELQ     GET LUID ELEMS                               
         GOTO1 AGETELS,CTWREC                                                   
         DROP  R8                                                               
         ICM   R8,15,APPARM                                                     
         BZ    GRLUID2                                                          
         B     GRLUID6                                                          
GRLUID5  ZIC   RE,1(R8)                                                         
         AR    R8,RE                                                            
         CLI   0(R8),0                                                          
         BE    GRLUID2                                                          
         CLI   0(R8),CTLSTELQ                                                   
         BNE   GRLUID5                                                          
         USING CTLSTD,R8                                                        
GRLUID6  CLI   CTLSTIND,X'80'                                                   
         BE    GRLUID5                                                          
         CLC   SELLUID,CTLSTDTA                                                 
         BNE   GRLUID5                                                          
         B     GRLUIDX                                                          
*                                                                               
GRLUIDX  EQU   *                                                                
*                                                                               
GRPWR    EQU   *                   FILTER ON POWER CODE                         
         OC    SELPWR,SELPWR                                                    
         BZ    GRPWRX                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTDSTELQ     GET DESTINATION ELEMENT                      
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         USING CTDSTD,R3                                                        
         CLC   SELPWR,CTDSTPOW                                                  
         BNE   GETRECRD                                                         
         B     GRPWRX                                                           
GRPWRX   EQU   *                                                                
*                                                                               
GRLFC    EQU   *                   FILTER ON LASER FORM CHECK CODE              
         OC    SELLFC,SELLFC                                                    
         BZ    GRLFCX                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTDSTELQ     GET DESTINATION ELEMENT                      
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         USING CTDSTD,R3                                                        
         CLC   SELLFC,CTDSTLFC                                                  
         BNE   GETRECRD                                                         
         B     GRLFCX                                                           
GRLFCX   EQU   *                                                                
*&&US                                                                           
GRDAR    EQU   *                   FILTER ON DARE ACCESS                        
         OC    SELDAR,SELDAR       NO FILER                                     
         BZ    GRDARX              FILTER PASSED MAGNIFICENTLY                  
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTUSAELQ     GET DARE USA ELEMENT                         
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            NONE, CAN'T MATCH W/O THIS ELEM              
*                                                                               
         USING CTUSAD,R3                                                        
         CLI   SELDAR,C'*'         TEST IF ANYTHING IN DARE PARAMETERS          
         BNE   GRDAR10                                                          
         CLI   SELDAR+1,C'*'                                                    
         BE    GRDAR10                                                          
         OC    CTUSADPI,CTUSADPI   WE HAVE A DARE PARTNER?                      
         BZ    *+14                                                             
         CLC   CTUSADPI,GRSPACES                                                
         BNE   GRDARX                                                           
         OC    CTUSADRC,CTUSADRC   OR A DARE ROUTING?                           
         BZ    GETRECRD                                                         
         CLC   CTUSADRC,GRSPACES                                                
         BE    GETRECRD                                                         
         B     GRDARX              YES, PASS THE FILTER                         
*                                                                               
GRDAR10  CLC   SELDAR(3),=C'MQ='      FILTER ON MQID#                           
         BE    GRDAR60                                                          
         CLI   LSTDARH+5-TWAD(R5),1   USER WANTS DARE PARTNER?                  
         BNE   GRDAR20                                                          
         CLC   CTUSADPI,SELDAR                                                  
         BNE   GETRECRD                                                         
         B     GRDARX                                                           
*                                                                               
GRDAR20  OC    SELDAR,GRSPACES                                                  
         CLI   LSTDARH+5-TWAD(R5),3  MATCH ON AGENCY OF ROUTING CODE?           
         BH    GRDAR30             NO, ON WHOLE ROUTING CODE                    
         CLC   CTUSADRC(3),SELDAR                                               
         BNE   GETRECRD                                                         
         B     GRDARX                                                           
*                                                                               
GRDAR30  CLC   SELDAR(3),=C'***'    WILD CARD, LOOKING FOR OFFICE               
         BNE   GRDAR50                                                          
         CLC   CTUSADRC+3(2),SELDAR+3  MATCH ROUTING CODE EXACTLY               
         BNE   GETRECRD                                                         
         B     GRDARX                                                           
*                                                                               
GRDAR50  CLC   CTUSADRC,SELDAR     MATCH ROUTING CODE EXACTLY                   
         BNE   GETRECRD                                                         
         B     GRDARX                                                           
*                                                                               
GRDAR60  EQU   *                                                                
         CLC   SELDARN,CTUSADMQ                                                 
         BNE   GETRECRD                                                         
         B     GRDARX                                                           
*                                                                               
GRDARX   EQU   *                                                                
*                                                                               
GRRTCD   EQU   *                   FILTER ON DARE ROUTING CODE                  
         CLI   SELRTCD,C'Y'                                                     
         BNE   GRRTCDX                                                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTUSAELQ     GET DARE USA ELEMENT                         
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            ELEMENT NOT PRESENT                          
         USING CTUSAD,R3                                                        
         OC    CTUSADRC,CTUSADRC   TEST FOR PRESENCE OF ROUTING CODE            
         BZ    GETRECRD                                                         
         CLC   CTUSADRC,GRSPACES                                                
         BE    GETRECRD                                                         
         B     GRRTCDX                                                          
GRRTCDX  EQU   *                                                                
*&&                                                                             
         B     GETRECOK                                                         
         DROP  R3,R8                                                            
*                                                                               
GETRECN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
         B     GETRECNO                                                         
*                                                                               
GETRECOK SR    RC,RC               RETURN CC EQUAL                              
GETRECNO LTR   RC,RC                                                            
         XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
GRSPACES DC    80C' '                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD A LINE OF USER ID RECORD DATA                                 *         
***********************************************************************         
         SPACE                                                                  
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
LINE     NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING LINE,RB                                                          
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+LIN'    INSERT NAME                                  
*                                                                               
         L     R2,AIOAREA1         POINT TO IOAREA                              
         USING CTIREC,R2                                                        
*                                                                               
         CLI   APACTN,ACTREP       TEST REPORT MODE                             
         BNE   *+12                                                             
         CLI   SELRTCD,C'Y'        YES -- IF ROUTING CODE FILTER IS ON,         
         BE    LNDDT                THEN DON'T PRINT USERID                     
         MVC   LISTID,CTIKID                                                    
*                                                                               
LNDDT    EQU   *                   DESTINATION DETAILS                          
         MVI   APELEM,X'30'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNDDTX                                                           
         USING CTDSTD,R3                                                        
         CLI   APACTN,ACTREP       TEST REPORT MODE                             
         BNE   *+12                                                             
         CLI   SELRTCD,C'Y'        YES -- IF ROUTING CODE FILTER IS ON,         
         BE    LNDDT10              THEN DON'T PRINT CERTAIN FIELDS             
         MVC   LISTLOG1,CTDSTLG1                                                
         MVC   LISTLOG2,CTDSTLG2                                                
         MVC   LISTPOWC,CTDSTPOW                                                
         MVC   LISTLFC,CTDSTLFC                                                 
LNDDT10  MVC   LISTNAME,CTDSTNAM                                                
*                                                                               
         CLI   SELRTCD,C'Y'                                                     
         BE    LNDDTX                                                           
         OC    SELDAR,SELDAR       DARE=Y ON LIST SCREEN?                       
         BZ    LNDDTX                                                           
         MVI   APELEM,CTUSAELQ     X'33'                                        
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNDDTX                                                           
         USING CTUSAD,R3                                                        
         XC    LISTLOG2,LISTLOG2                                                
         MVC   LISTLOG2(1),CTUSADPI   SHOW DARE PARTNER                         
         OC    CTUSADRC,CTUSADRC                                                
         BZ    LNDDTX                                                           
         MVI   LISTLOG2+1,C':'                                                  
         MVC   LISTLOG2+2(L'CTUSADRC),CTUSADRC                                  
LNDDTX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
LNSHI    EQU   *                   SHIPPING INSTRUCTIONS                        
         MVI   APELEM,X'4C'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNSHIX                                                           
         USING CTSHPD,R3                                                        
         CLI   APACTN,ACTREP       TEST REPORT MODE                             
         BNE   *+12                                                             
         CLI   SELRTCD,C'Y'        YES -- IF ROUTING CODE FILTER IS ON,         
         BE    LNSHIX               THEN DON'T PRINT UNIT CODE                  
         MVC   LISTUNIT,CTSHPINS                                                
LNSHIX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
*&&US                                                                           
LNUS     EQU   *                   U.S. SPECIFIC DATA                           
         CLI   APACTN,ACTREP       TEST REPORT MODE                             
         BNE   LNUSX               CAN'T FIT ROUTING CODE ON LIST SCRN          
         MVI   APELEM,CTUSAELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNUSX                                                            
         USING CTUSAD,R3                                                        
         MVC   LISTRTCD,CTUSADRC                                                
LNUSX    EQU   *                                                                
         DROP  R3                                                               
*&&                                                                             
*                                                                               
LINEX    XIT1  ,                                                                
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================                               
* BUILD DOWNLOAD DATA FIELDS                                                    
*================================================                               
                                                                                
         USING DLD,R4              OUTPUT FIELDS                                
*                                                                               
DLLINE   NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DLLINE,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+DLL'    INSERT NAME                                  
*                                                                               
         CLI   SENTCOLS,C'Y'                                                    
         BE    DDLIN02                                                          
*                                                                               
         LARL  RE,COLDEF1                                                       
         MVC   REPP1(COLDEF1X-COLDEF1),0(RE)                                    
         LARL  RE,COLDEF2                                                       
         MVC   REPP2(COLDEF2X-COLDEF2),0(RE)                                    
         LARL  RE,COLDEF3                                                       
         MVC   REPP3(COLDEF3X-COLDEF3),0(RE)                                    
         MVI   REPLINE,1           SUPPRESS PAGING                              
                                                                                
         GOTO1 VREPORT,REPD                                                     
         MVI   SENTCOLS,C'Y'                                                    
*                                                                               
DDLIN02  MVI   HAVEDATA,C'N'                                                    
         L     R2,AIOAREA1         POINT TO IOAREA                              
         USING CTIREC,R2                                                        
*                                                                               
         MVC   DLUSERID,CTIKID                                                  
*                                                                               
         MVI   APELEM,X'06'        AGENCY ALPHA ID                              
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         JZ    EXIT                IF NO ALPHA AGY, SKIP THIS REC               
         USING CTAGYD,R3                                                        
         MVC   DLALPHA,CTAGYID                                                  
         DROP  R3                                                               
*                                                                               
         MVI   APELEM,X'30'        DESTINATION DETAILS                          
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DLLIN10                                                          
         MVI   HAVEDATA,C'Y'                                                    
*                                                                               
         USING CTDSTD,R3                                                        
         MVC   DLLOGO1,CTDSTLG1                                                 
         MVC   DLLOGO2,CTDSTLG2                                                 
         MVC   DLPOWCD,CTDSTPOW                                                 
         MVC   DLDESTNM,CTDSTNAM                                                
         MVC   DLDESTA1,CTDSTADD                                                
         MVC   DLDESTA2,CTDSTAD2                                                
         MVC   DLDESTA3,CTDSTAD3                                                
         DROP  R3                                                               
*                                                                               
DLLIN10  XC    DLSHIP1,DLSHIP1                                                  
         XC    DLSHIP2,DLSHIP2                                                  
*                                                                               
         MVI   APELEM,X'4C'        SHIPPING INSTRUCTIONS                        
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DLLIN12                                                          
         MVI   HAVEDATA,C'Y'                                                    
*                                                                               
         USING CTSHPD,R3                                                        
         LA    RF,6(R3)            POINT TO FIRST DATA CHAR                     
         LLC   RE,CTSHPLEN         GET ELEM LEN                                 
         AHI   RE,-6               GIVES DATA LEN                               
         CHI   RE,60                                                            
         JNH   *+8                                                              
         LA    RE,60                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLSHIP1(0),0(RF)                                                 
*                                                                               
         LLC   RE,CTSHPLEN         GET ELEM LEN                                 
         AHI   RE,-66              GIVES REMAINING DATA LEN                     
         JNP   DLLIN12             ALL HAS BEEN MOVED                           
         LA    RF,60(RF)           BUMP DATA POINTER                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLSHIP2(0),0(RF)                                                 
         DROP  R3                                                               
*                                                                               
DLLIN12  XC    DLORIGNM,DLORIGNM                                                
         XC    DLORIGAD,DLORIGAD                                                
*                                                                               
         MVI   APELEM,X'36'        ORIGIN DETAILS                               
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DLLIN20                                                          
         MVI   HAVEDATA,C'Y'                                                    
*                                                                               
         USING CTORGD,R3                                                        
         MVC   DLORIGNM,CTORGNAM                                                
         MVC   DLORIGAD,CTORGADD                                                
         DROP  R3                                                               
*                                                                               
DLLIN20  XC    DLOUTTYP,DLOUTTYP                                                
*                                                                               
         MVI   APELEM,X'42'        OUTPUT TYPE                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DLLIN30                                                          
         MVI   HAVEDATA,C'Y'                                                    
*                                                                               
         USING CTOCOD,R3                                                        
         MVC   DLOUTTYP,CTOCODE                                                 
         DROP  R3                                                               
*                                                                               
DLLIN30  XC    DLDRDATA,DLDRDATA                                                
*                                                                               
         MVI   APELEM,CTUSAELQ     US AGENCY EXTRA                              
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         JZ    DLLIN40                                                          
         MVI   HAVEDATA,C'Y'                                                    
*                                                                               
         USING CTUSAD,R3                                                        
         MVC   DLDRPTNR,CTUSADPI   DARE PARTNER ID                              
         MVC   DLDRRC,CTUSADRC     DARE ROUTING CODE                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,CTUSADMQ                                                    
         EDIT  (R0),DLDRMQ         DARE MQ SERIES ID NUMBER                     
*                                                                               
         CLI   CTUSALEN,CTUSALN2                                                
         JL    DLLIN32                                                          
         CLI   CTUSAVEN,C' '                                                    
         JL    DLLIN32                                                          
         MVC   DLDRVNDR,CTUSAVEN                                                
*                                                                               
DLLIN32  MVI   DLDRFLAG,C'N'       DARE XML USER? DEFAULT = NO                  
         TM    CTUSADFL,CTUSADFX                                                
         JZ    *+8                                                              
         MVI   DLDRFLAG,C'Y'                                                    
         DROP  R3                                                               
                                                                                
*===============================================================                
* ALL FIELDS NOW IN OUTPUT BUFFERS                                              
* CALL DLFLD FOR EACH DATA FIELD                                                
*===============================================================                
                                                                                
DLLIN40  DS    0H                                                               
         CLI   HAVEDATA,C'Y'                                                    
         JNE   EXIT                                                             
*                                                                               
         MVI   REPHEADI,0          SUPPRESS ALL FORCE PAGES                     
         MVI   REPMIDSI,0                                                       
         MVI   REPFOOTI,0                                                       
*                                                                               
         LA    R3,DOWNTAB                                                       
*                                                                               
DLLIN42  MVI   D.DLCBACT,DLCBPUT     ACTION IS PUT                              
         MVI   D.DLCBTYP,DLCBTXT     TYPE IS TEXT                               
         OI    D.DLCBFLG1,DLCBFXFL   USE EXTENDED FOR TEXT                      
*                                                                               
         LR    RE,R4                 POINT TO DATA BUFFER                       
         AH    RE,0(R3)              ADD DSPL TO FIELD                          
*                                                                               
         LLC   RF,2(R3)              GET DATA LEN                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   D.DLCBFLX(0),0(RE)                                               
*                                                                               
         LARL  RE,TRTAB                                                         
         EX    RF,*+8                                                           
         J     *+10                                                             
         TR    D.DLCBFLX(0),0(RE)    GET RID OF ANY CONTROL CHARS               
*                                                                               
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
         LA    R3,3(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   DLLIN42                                                          
*                                                                               
         MVI   D.DLCBACT,DLCBEOL          END OF LINE                           
         GOTO1 VDLFLD,DLCB                                                      
         J     EXIT                                                             
*                                                                               
DLHOOK   NTR1  BASE=*,LABEL=*                                                   
         MVI   REPLINE,1           SUPPRESS PAGING                              
         GOTO1 VREPORT,REPD                                                     
         J     EXIT                                                             
         EJECT                                                                  
*============================================================                   
* TABLE OF DOWNLOAD FIELDS                                                      
*============================================================                   
                                                                                
DOWNTAB  DS    0D                                                               
         DC    AL2(DLUSERID-DLD),AL1(L'DLUSERID)                                
         DC    AL2(DLALPHA-DLD),AL1(L'DLALPHA)                                  
         DC    AL2(DLDESTNM-DLD),AL1(L'DLDESTNM)                                
         DC    AL2(DLDESTA1-DLD),AL1(L'DLDESTA1)                                
         DC    AL2(DLDESTA2-DLD),AL1(L'DLDESTA2)                                
         DC    AL2(DLDESTA3-DLD),AL1(L'DLDESTA3)                                
         DC    AL2(DLLOGO1-DLD),AL1(L'DLLOGO1)                                  
         DC    AL2(DLLOGO2-DLD),AL1(L'DLLOGO2)                                  
         DC    AL2(DLPOWCD-DLD),AL1(L'DLPOWCD)                                  
         DC    AL2(DLSHIP1-DLD),AL1(L'DLSHIP1)                                  
         DC    AL2(DLSHIP2-DLD),AL1(L'DLSHIP2)                                  
         DC    AL2(DLORIGNM-DLD),AL1(L'DLORIGNM)                                
         DC    AL2(DLORIGAD-DLD),AL1(L'DLORIGAD)                                
         DC    AL2(DLOUTTYP-DLD),AL1(L'DLOUTTYP)                                
*&&US                                                                           
         DC    AL2(DLDRPTNR-DLD),AL1(L'DLDRPTNR)                                
         DC    AL2(DLDRRC-DLD),AL1(L'DLDRRC)                                    
         DC    AL2(DLDRMQ-DLD),AL1(L'DLDRMQ)                                    
         DC    AL2(DLDRFLAG-DLD),AL1(L'DLDRFLAG)                                
         DC    AL2(DLDRVNDR-DLD),AL1(L'DLDRVNDR)                                
*&&                                                                             
         DC    X'FFFF'                                                          
*                                                                               
DLLINX   XIT1  ,                                                                
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK ID RECORD AND ITS PASSIVE WILL NOT BE TOO BIG BEFORE UPDATE   *         
* R2=A(USER ID RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
CHKBIG   NTR1  BASE=*                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CTILEN                                                      
         L     RE,AIOAREA2                                                      
         L     R2,AIOAREA1                                                      
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
         L     R2,AIOAREA2         SUE AIOAREA 2 AS TEMPORARY WORK AREA         
         MVI   APELEM,X'02'        ADD LONGEST PASSIVE POINTER ELEMENT          
         MVI   APELEM+1,X'0C'                                                   
         MVC   APELEM+2(10),=CL10' '                                            
         GOTO1 AADDELN,CTIREC                                                   
         BNE   CBIGNO              HERE IF RECORD WILL BE TOO BIG               
         B     CBIGOK                                                           
*                                                                               
CBIGOK   SR    RC,RC                                                            
CBIGNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
* DO NOT TRANSLATE DOWNLOAD CTL CHARS COLON, SEMICOLON, DOUBLE QUOTE            
TRTAB    DC    X'40404040404040404040404040404040'     00-0F                    
         DC    X'40404040404040404040404040404040'     10-1F                    
         DC    X'40404040404040404040404040404040'     20-2F                    
         DC    X'40404040404040404040404040404040'     30-3F                    
         DC    X'40404040404040404040404B4C4D4E40'     40-4F                    
         DC    X'50404040404040404040405B5C5D405F'     50-5F                    
         DC    X'60614040404040404040406B6C6D6E6F'     60-6F                    
         DC    X'40404040404040404040407B7C7D7E40'     70-7F                    
         DC    X'40818283848586878889404040404040'     80-8F                    
         DC    X'40919293949596979899404040404040'     90-9F                    
         DC    X'4040A2A3A4A5A6A7A8A9404040404040'     A0-AF                    
         DC    X'40404040404040404040404040404040'     B0-BF                    
         DC    X'40C1C2C3C4C5C6C7C8C9404040404040'     C0-CF                    
         DC    X'40D1D2D3D4D5D6D7D8D9404040404040'     D0-DF                    
         DC    X'4040E2E3E4E5E6E7E8E9404040404040'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F9404040404040'     F0-FF                    
*                                                                               
COLDEF1  DC    C'"USERID" "ALPHA" "DEST NAME" "DEST ADDR1"'                     
         DC    C'" DEST ADDR2" "ATTENTION" "LOGO1" "LOGO2"'                     
COLDEF1X EQU   *                                                                
*                                                                               
COLDEF2  DC    C'"POWER CODE" "SHIP INFO 1" "SHIP INFO 2"'                      
         DC    C' "ORIGIN NAME"  "ORIGIN ADDRESS" "OUTPUT TYPE"'                
COLDEF2X EQU   *                                                                
*&&US                                                                           
COLDEF3  DC    C'"DARE PTNR" "DARE ROUTING" "DARE MQID"'                        
         DC    C' "DARE XML" "DARE VENDOR"'                                     
         DC    X'5E'               SEMICOLON EOL                                
COLDEF3X EQU   *                                                                
*&&                                                                             
*&&UK                                                                           
COLDEF3  DC   X'5E'                                                             
COLDEF3X EQU  *                                                                 
*&&                                                                             
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* GEGENEDI                                                                      
       ++INCLUDE GEGENEDI                                                       
         EJECT                                                                  
* CTGENMQDEF                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENMQDEF                                                     
         EJECT                                                                  
         PRINT ON                                                               
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENF9D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGEND9D                                                       
         EJECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENB9D                                                       
         ORG                                                                    
         EJECT                                                                  
*                                  WROKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 LAST SYSTEM DISPLAYED                        
SAVKEY   DS    XL(L'CTIKEY)        SAVE LAST RECORD KEY READ FOR COPY           
*                                                                               
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTID   DS    CL10                                                             
         DS    CL1                                                              
LISTLOG1 DS    CL7                                                              
         DS    CL1                                                              
LISTLOG2 DS    CL7                                                              
         DS    CL1                                                              
LISTPOWC DS    CL4                                                              
         DS    CL1                                                              
LISTLFC  DS    CL2                                                              
         DS    CL1                                                              
LISTUNIT DS    CL2                                                              
         DS    CL1                                                              
LISTNAME DS    CL33                                                             
         DS    CL3                                                              
LISTRTCD DS    CL5                                                              
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
VDLFLD   DS    A                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
RETURN   DS    F                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
WORK     DS    CL(L'APWORK)                                                     
*                                                                               
SELDATA  DS    0XL(SELDATAL)                                                    
SELID    DS    CL8                 USER ID                                      
SELIDSP  DS    CL1                 1ST SPECIAL CHAR                             
SELIDL   DS    CL1                 (L'DATA ENTERED)                             
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELSYS   DS    XL1                 SYSTEM SEOV NUMBER                           
SELPGM   DS    XL1                 PROGRAM NUMBER                               
SELAGY   DS    XL2                 AGENCY ALPHA ID                              
SELPWD   DS    XL1                 PASSWORD REQUIRED                            
SELCID   DS    XL10                COMPATIBLE ID                                
SELLUID  DS    XL8                 PRINTER/SHUTTLE LUID                         
SELPWR   DS    CL4                 POWER CODE                                   
SELLFC   DS    CL2                 LASER FORM CHECKS                            
SELDAR   DS    CL(L'LSTDAR)        DARE FILTER                                  
SELDARN  DS    XL2                 DARE FILTER (MQID#)                          
SELRTCD  DS    CL1                 ROUTING CODE FILTER                          
SELDATAL EQU   *-SELID                                                          
*                                                                               
FLDCNT   DS    XL1                                                              
VPRTFLAG DS    XL1                 VALID PRINTER CONTROL FLAG                   
VPRTFLID EQU   X'80'               PRINTER LUID/LIST ENTRIES                    
VPRTFPNM EQU   X'40'               PRINTER NAME FLAG                            
VPRTFPID EQU   X'20'               PNAME DEFAULT PRINCIPAL ID                   
VPRTFPDI EQU   X'10'               PNAME DEFAULT USERID                         
IDOPTF2  DS    XL1                 SAVE CTIDOFL2                                
PNMDID   DS    XL2                 SAVE PNAME USERID DEFAULT                    
*                                                                               
GETSEQF  DS    XL1                                                              
FLAG     DS    XL1                                                              
SYSTEM   DS    XL1                                                              
PROGRAM  DS    XL1                                                              
WILDCLEN DS    XL1                                                              
HAVEDATA DS    CL1                                                              
SENTCOLS DS    CL1                                                              
*                                                                               
         DS    0D                                                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
*                                                                               
IDNUM    DS    CL2                                                              
IDALP    DS    CL10                                                             
NLINESA  DS    CL1                                                              
NLINESB  DS    CL1                                                              
NLINEST  DS    CL1                                                              
LINES    DS    20CL32                                                           
KEYSAVE  DS    CL(L'IOKEY)                                                      
         DS    0D                                                               
REC      DS    CL1024                                                           
         ORG   REC                                                              
DLCB     DS    XL256                                                            
         ORG                                                                    
LOCALX   EQU   *                                                                
         SPACE 1                                                                
*                                                                               
DLD      DSECT                                                                  
DLUSERID DS    CL10                                                             
DLALPHA  DS    CL2                                                              
DLDESTNM DS    CL33                                                             
DLDESTA1 DS    CL33                                                             
DLDESTA2 DS    CL33                                                             
DLDESTA3 DS    CL33                                                             
DLLOGO1  DS    CL7                                                              
DLLOGO2  DS    CL7                                                              
DLPOWCD  DS    CL4                                                              
DLSHIP1  DS    CL60                                                             
DLSHIP2  DS    CL60                                                             
DLORIGNM DS    CL33                                                             
DLORIGAD DS    CL33                                                             
DLOUTTYP DS    CL10                                                             
*                                                                               
DLDRDATA DS    0XL20                                                            
DLDRPTNR DS    CL1                 DARE PARTNER                                 
DLDRRC   DS    CL5                 ROUTING CODE                                 
DLDRMQ   DS    CL3                 MQ SERIES ID                                 
DLDRFLAG DS    CL1                 DARE FLAG                                    
DLDRVNDR DS    CL10                                                             
       ++INCLUDE DDDLCB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044CTGEN06   08/08/16'                                      
         END                                                                    
