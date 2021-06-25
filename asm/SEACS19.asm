*          DATA SET SEACS19    AT LEVEL 019 AS OF 06/18/15                      
*PHASE TA0D19B                                                                  
         TITLE 'SEACS19 -FILE MAINTENANCE- MEDIA OFFICE RECORDS'                
***********************************************************************         
* THIS VERSION OF TA0D19 IS USED TO POPULATE THE INITIAL TWO CHARACTER          
* MEDIA OFFICE RECORDS.  IT IS NOT INTENDED TO BE LOADED AS A LIVE              
* VERSION OF THE MEDIA OFFICE RECORD MAINTENANCE PROGRAM.  THE MEDIA            
* OFFICE BUILD (GOPOP) MODE WILL ADD ABOUT 40 MEDIA OFFICE RECORDS.             
***********************************************************************         
ACS19    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS19*,RR=RE                                                 
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING MOFRECD,R2          R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     EXIT                05 - APMDELR                                 
         B     EXIT                06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     FSTLST              11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     EXIT                15 - APMVALQ                                 
         B     EXIT                16 - APMREPP                                 
         B     SETSCR              17 - APMSETT                                 
         B     EXIT                18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF OFFICE RECORD                                      
***********************************************************************         
VALKEY   LA    R2,IOKEY                                                         
*                                                                               
* ONE BYTE OFFICE VISIBLE?                                                      
         OI    OFFNUBH+(FVATRB-FVIHDR),FVALOWI                                  
         OI    OFFNUBH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    OFFNUMH+(FVATRB-FVIHDR),FVALOWI+FVAPROT                          
         OI    OFFNUMH+(FVOIND-FVIHDR),FVOXMT                                   
         TM    CUSTAT,CUSDDS             CAN ONLY ENTER ON DDS TERMINAL         
         BZ    VK002                                                            
         NI    OFFNUBH+(FVATRB-FVIHDR),X'FF'-FVALOWI                            
         NI    OFFNUMH+(FVATRB-FVIHDR),X'FF'-FVALOWI-FVAPROT                    
*                                                                               
VK002    TM    CUSTAT,CUSDDS             CAN ONLY ENTER ON DDS TERMINAL         
         BZ    VK010                                                            
         CLI   APACTN,ACTADD             ADD ALL OF DEFAULT OFFICE RECS         
         BNE   VK010                                                            
         CLC   =C'!!',OFFOID                                                    
         BNE   VK010                                                            
         CLC   =C'GOPOP',OFFSYS                                                 
         BNE   VK010                                                            
         BRAS  RE,GOPOP                                                         
         B     SAEPOP                                                           
*                                                                               
* TWO BYTE OFFICE                                                               
VK010    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,OFFOIDH                                                    
         BNE   VALKEYX                                                          
         MVC   TWOOFF,OFFOID                                                    
         OC    TWOOFF,SPACES             MAKE SURE NOT NULL                     
*                                                                               
         CLI   APACTN,ACTADD             CHECK OFFICE INPUT ON ADD              
         BNE   VK030                                                            
*                                                                               
         LA    RE,OFFOID                 VALIDATE ALPHA NUMERIC                 
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         B     VK022                                                            
VK020    CLI   0(RE),C' '                SECOND BYTE CAN BE SPACE               
         BE    VK026                                                            
VK022    CLI   0(RE),C'A'                                                       
         BL    VK024                                                            
         CLI   0(RE),C'Z'                                                       
         BH    VK024                                                            
         B     VK026                                                            
VK024    CLI   0(RE),C'0'                                                       
         BL    SAEIIF                                                           
         CLI   0(RE),C'9'                                                       
         BH    SAEIIF                                                           
VK026    LA    RE,1(RE)                                                         
         BCT   RF,VK020                                                         
*                                                                               
* SYSTEM                                                                        
VK030    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,OFFSYSH                                                    
         BNE   VALKEYX                                                          
         GOTO1 AVALSYS,OFFSYSH                                                  
         BNE   VALKEYX                                                          
         MVC   SYSTEM,APWORK                                                    
*                                                                               
         LA    R1,MOFSYSL                                                       
VK032    CLI   0(R1),0                   MAKE SURE VALID SYSTEM FOR             
         BE    SAESYE                    MEDIA OFFICE RECORDS                   
         CLC   SYSTEM,0(R1)                                                     
         BE    VK038                                                            
         AHI   R1,1                                                             
         B     VK032                                                            
*                                                                               
* CHECK FOR AVAILABLE OFFICE NUMBER                                             
VK038    CLI   APACTN,ACTADD                                                    
         BNE   VK090                                                            
*                                                                               
         MVI   ONEOFF,X'41'              START WITH OFFICE X'41'                
*                                                                               
         TM    CUSTAT,CUSDDS             CAN ONLY ENTER ON DDS TERMINAL         
         BZ    VK040                                                            
         CLI   OFFNUM,C'*'               IF C'*' ENTERED                        
         BNE   VK040                                                            
         MVC   ONEOFF,OFFNUM+1           USE CHARACTER AS STARTING              
*                                                                               
VK040    XC    MOFKEY,MOFKEY             BUILD OFFICE RECORD KEY                
         MVI   MOFKTYP,MOFKTYPQ          C'O' MEDIA OFFICE RECORDS              
         MVI   MOFKSUB,MOFKS1Q           X'01' TWO BYTE OFFICE                  
         MVC   MOFKAGY,CUAALF            AGENCY ALPHA ID                        
         MVC   MOFK1OF,ONEOFF            ONE BYTE OFFICE                        
         MVC   MOFKSYS,SYSTEM            SYSTEM                                 
*                                                                               
         GOTO1 AIO,IORDD+IOGENDIR+IO1                                           
         BE    VK060                                                            
         TM    IOERR,IOEDEL              DON'T USE DELETED NUMBERS              
         BZ    VK090                                                            
*                                                                               
VK060    CLI   OFFNUM,C'*'               IF C'*' ENTERED                        
         BE    SAEIIF                    THEN DON'T TRY FOR NEXT NUMBER         
*                                                                               
VK062    SR    R1,R1                     OFFICE RECORD FOR THIS #               
         IC    R1,ONEOFF                 ALREADY EXISTS, TRY NEXT #             
         AHI   R1,1                                                             
         STC   R1,ONEOFF                                                        
*        -----------------                                                      
         CLI   ONEOFF,X'40'              DO NOT ALLOW NUMBERS < C' '            
         BNH   VK062                                                            
         CLI   ONEOFF,X'5B'              C'$' NOT ALLOWED                       
         BE    VK062                                                            
         CLI   ONEOFF,X'5C'              C'*' NOT ALLOWED                       
         BE    VK062                                                            
         CLI   ONEOFF,X'60'              C'-' NOT ALLOWED                       
         BE    VK062                                                            
         CLI   ONEOFF,X'F0'              C'0' NOT ALLOWED                       
         BE    VK062                                                            
         CLI   ONEOFF,X'FF'              255 - ANYMORE AVAILABLE?               
         BE    SAEIIF                                                           
*        -----------------                                                      
         B     VK040                                                            
*                                                                               
* READ FOR RECORD                                                               
VK090    XC    MOFKEY,MOFKEY             BUILD OFFICE RECORD KEY                
         MVI   MOFKTYP,MOFKTYPQ          C'O' MEDIA OFFICE RECORDS              
         MVI   MOFKSUB,MOFKS2Q           X'02' TWO BYTE CHAR OFFICE             
         MVC   MOFKAGY,CUAALF            AGENCY ALPHA ID                        
         MVC   MOFK2OF,TWOOFF            OFFICE CODE                            
         MVC   MOFKSYS,SYSTEM            SYSTEM                                 
*                                                                               
VK092    MVC   APRECKEY(MOFKLEN),MOFKEY  SAVE KEY                               
         NI    LMOFIND,X'FF'-LMOFPRQ     RESET PASSIVE RESTORE                  
*                                                                               
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX                   I/O ERROR EXIT                         
         BNE   VK094                                                            
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         B     VK100                                                            
VK094    MVI   APINDS,APIOKADD                                                  
         TM    IOERR,IOEDEL              TEST RECORD IS DELETED                 
         BZ    VALKEYY                                                          
         OI    LMOFIND,LMOFPRQ           PASSIVE POINTER RESTORE                
         B     VALKEYY                                                          
*                                                                               
VK100    LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX                   I/O ERROR EXIT                         
         MVC   LMOFDA,IODA               SAVE DISK ADDRESS                      
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN OFFICE RECORD                                     
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
*                                                                               
         CLI   APACTN,ACTADD             ADD ACTION                             
         BNE   VR040                                                            
         XC    MOFKEY(256),MOFKEY                                               
         MVI   MOFKTYP,MOFKTYPQ          C'O' MEDIA OFFICE RECORDS              
         MVI   MOFKSUB,MOFKS1Q           X'01' ONE BYTE OFFICE                  
         MVC   MOFKAGY,CUAALF            AGENCY ALPHA ID                        
         MVC   MOFK1OF,ONEOFF            OFFICE CODE                            
         MVC   MOFKSYS,SYSTEM            SYSTEM                                 
         MVC   MOFFC2OF,TWOOFF                                                  
         LHI   R1,MOFFIRST+1                                                    
         STCM  R1,3,MOFFLEN                                                     
         B     VR060                                                            
*                                                                               
VR040    MVC   ONEOFF,MOFK1OF                                                   
         MVC   OLDOFF,SPACES                                                    
                                                                                
*----------------------------------------                                       
* POSSIBLE 2 BYTE OFF CHANGE                                                    
*----------------------------------------                                       
         MVI   FVMINL,1                                                         
         MVI   FVMAXL,2                                                         
         GOTO1 AFVAL,OFFNOFH             NEW OFFICE CODE?                       
         BE    VR048                                                            
         MVI   FVMAXL,0                                                         
         CLC   FVMSGNO,=AL2(FVFLONG)                                            
         BE    VALRECX                                                          
         B     VR060                                                            
*                                                                               
VR048    LA    RE,OFFNOF                 VALIDATE ALPHA NUMERIC                 
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         B     VR052                                                            
VR050    CLI   0(RE),C' '                SECOND BYTE CAN BE SPACE               
         BE    VR056                                                            
VR052    CLI   0(RE),C'A'                                                       
         BL    VR054                                                            
         CLI   0(RE),C'Z'                                                       
         BH    VR054                                                            
         B     VR056                                                            
VR054    CLI   0(RE),C'0'                                                       
         BL    SAEIIF                                                           
         CLI   0(RE),C'9'                                                       
         BH    SAEIIF                                                           
VR056    LA    RE,1(RE)                                                         
         BCT   RF,VR050                                                         
*                                                                               
         LA    R2,IOKEY                                                         
         XC    MOFKEY,MOFKEY             BUILD OFFICE RECORD KEY                
         MVI   MOFKTYP,MOFKTYPQ          C'O' MEDIA OFFICE RECORDS              
         MVI   MOFKSUB,MOFKS2Q           X'02' TWO BYTE CHAR OFFICE             
         MVC   MOFKAGY,CUAALF            AGENCY ALPHA ID                        
         MVC   MOFK2OF,OFFNOF            NEW OFFICE CODE                        
         OC    MOFK2OF,SPACES                                                   
         MVC   MOFKSYS,SYSTEM            SYSTEM                                 
*                                                                               
         GOTO1 AIO,IORDUPD+IOGENDIR+IO2                                         
         BL    SAEIIO                    I/O ERROR EXIT                         
         BE    SAERAE                                                           
*                                                                               
         MVC   OLDOFF,TWOOFF             SAVE OLD OFFICE                        
         MVC   TWOOFF,OFFNOF             AND GET READY FOR NEW                  
         OC    TWOOFF,SPACES                                                    
*                                                                               
         XC    IOKEY,IOKEY               BUILD OFFICE RECORD KEY                
         MVI   MOFKTYP,MOFKTYPQ          C'O' MEDIA OFFICE RECORDS              
         MVI   MOFKSUB,MOFKS2Q           X'02' TWO BYTE CHAR OFFICE             
         MVC   MOFKAGY,CUAALF            AGENCY ALPHA ID                        
         MVC   MOFK2OF,TWOOFF            NEW OFFICE CODE                        
         MVC   MOFKSYS,SYSTEM            SYSTEM                                 
*                                                                               
         NI    MOFKSTAT,X'FF'-X'80'                                             
         MVC   MOFKC1OF,ONEOFF                                                  
         MVC   MOFKDA,LMOFDA                                                    
*                                                                               
         LA    R1,IOWRITE+IOGENDIR+IO2                                          
         TM    IOERR,IOEDEL              TEST RECORD IS DELETED                 
         BO    *+8                                                              
         LA    R1,IOADD+IOGENDIR+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MOFKEY,APRECKEY                                                  
         GOTO1 AIO,IORDUP+IOGENDIR+IO2                                          
         BE    *+6                       I/O ERROR EXIT                         
         DC    H'0'                                                             
         OI    MOFKSTAT,X'80'                                                   
         GOTO1 AIO,IOWRITE+IOGENDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA1                                                      
         MVC   MOFFC2OF,TWOOFF                                                  
                                                                                
*----------------------------------------                                       
* UPDATE/BUILD ELEMENTS                                                         
*----------------------------------------                                       
VR060    LA    R3,APELEM                                                        
                                                                                
*----------------------------------------                                       
* NAME ELEMENT X'0A'                                                            
*----------------------------------------                                       
         USING MONAMD,R3                                                        
         MVI   MONAMEL,MONAMELQ          REMOVE NAME ELEMENT                    
         MVI   MONAMLN,0                                                        
         GOTO1 ADELELS,MOFRECD                                                  
*                                                                               
         XC    MONAMEL(MONAMLNQ),MONAMEL                                        
         MVI   MONAMEL,MONAMELQ                                                 
         MVI   MONAMLN,MONAMLNQ                                                 
         MVC   MONAMSH,SPACES                                                   
         MVC   MONAMLO,SPACES                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,OFFSNMH             SHORT NAME FIELD                       
         BNE   *+10                                                             
         MVC   MONAMSH,FVIFLD                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,OFFLNMH             LONG NAME FIELD                        
         BNE   *+10                                                             
         MVC   MONAMLO,FVIFLD                                                   
*                                                                               
         GOTO1 AADDELS,MOFRECD                                                  
                                                                                
*----------------------------------------                                       
* PREVIOUS OFFICE ELEMENT X'0B'                                                 
*----------------------------------------                                       
         CLC   OLDOFF,SPACES                                                    
         BNH   VR070                                                            
         USING MOOLDD,R3                                                        
         MVI   MOOLDEL,MOOLDELQ                                                 
         MVI   MOOLDLN,0                                                        
         GOTO1 ADELELS,MOFRECD                                                  
         XC    MOOLDEL(MOOLDLNQ),MOOLDEL                                        
         MVI   MOOLDEL,MOOLDELQ                                                 
         MVI   MOOLDLN,MOOLDLNQ                                                 
         MVC   MOOLDOFF,OLDOFF                                                  
         GOTO1 AADDELS,MOFRECD                                                  
                                                                                
*----------------------------------------                                       
* WEBDAV USER ID AND PASSWORD X'0C'                                             
*----------------------------------------                                       
         USING MOUIPD,R3                                                        
VR070    MVI   MOUIPEL,MOUIPELQ                                                 
         MVI   MOUIPLN,0                                                        
         GOTO1 ADELELS,MOFRECD                                                  
         XC    MOUIPEL(MOUIPLNQ),MOUIPEL                                        
         MVI   MOUIPEL,MOUIPELQ                                                 
         MVI   MOUIPLN,MOUIPLNQ                                                 
         MVC   MOUIPUID,OFFUID                                                  
         MVC   MOUIPASS,OFFPAS                                                  
*                                                                               
         CLC   MOUIPUID,SPACES           ANY WEBDAV USER ID?                    
         BH    VR072                                                            
         CLC   MOUIPASS,SPACES           OR WEBDAV PASSWORD?                    
         BNH   VR074                     . NO, NO NEED FOR ELEMENT              
VR072    GOTO1 AADDELS,MOFRECD                                                  
                                                                                
*----------------------------------------                                       
* SAP INTERFACE CODE ELEMENT X'0D'                                              
*----------------------------------------                                       
         USING MOSAPD,R3                                                        
VR074    CLI   SAPAGY,C'Y'               ONLY IF SAP AGENCY                     
         BNE   VR080                                                            
*                                                                               
         MVI   MOSAPEL,MOSAPELQ                                                 
         MVI   MOSAPLN,0                                                        
         GOTO1 ADELELS,MOFRECD                                                  
         XC    MOSAPEL(MOSAPLNQ),MOSAPEL                                        
         MVI   MOSAPEL,MOSAPELQ                                                 
         MVI   MOSAPLN,MOSAPLNQ                                                 
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,OFFSACH             SAP COMPANY                            
         BNE   SAEMIF                                                           
         MVC   MOSAPCOM,OFFSAC                                                  
         OC    MOSAPCOM,SPACES                                                  
*                                                                               
         GOTO1 AFVAL,OFFSASH             SAP SALES ORG                          
         BNE   SAEMIF                                                           
         MVC   MOSAPORG,OFFSAS                                                  
         OC    MOSAPORG,SPACES                                                  
*                                                                               
         GOTO1 AFVAL,OFFSAPH             SAP PROFIT CENTER                      
         BNE   SAEMIF                                                           
         MVC   MOSAPCTR,OFFSAP                                                  
         OC    MOSAPCTR,SPACES                                                  
*                                                                               
         GOTO1 AADDELS,MOFRECD                                                  
                                                                                
*----------------------------------------                                       
* GENFIL ACTIVITY ELEMENT X'  '                                                 
*----------------------------------------                                       
VR080    GOTO1 ASETACT,MOFRECD           SET ACTIVITY ELEMENT                   
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   LMOFDA,IODA               SAVE DISK ADDRESS                      
*                                                                               
         CLI   APACTN,ACTCHA             ACTION CHANGE?                         
         BNE   VR090                     . NO                                   
         CLC   OLDOFF,SPACES             NEW OFFICE CODE ENTERED?               
         BNH   VR110                     . NO                                   
*                                                                               
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY               BUILD OFFICE RECORD KEY                
         MVI   MOFKTYP,MOFKTYPQ          C'O' MEDIA OFFICE RECORDS              
         MVI   MOFKSUB,MOFKS1Q           X'01' ONE BYTE OFFICE                  
         MVC   MOFKAGY,CUAALF            AGENCY ALPHA ID                        
         MVC   MOFK1OF,ONEOFF            ONE BYTE OFFICE                        
         MVC   MOFKSYS,SYSTEM            SYSTEM                                 
*                                                                               
         GOTO1 AIO,IORDUP+IOGENDIR+IO1                                          
         BE    *+6                       I/O ERROR EXIT                         
         DC    H'0'                                                             
         MVC   MOFKC2OF,TWOOFF                                                  
         GOTO1 AIO,IOWRITE+IOGENDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR090    CLI   APACTN,ACTADD                                                    
         BNE   VR110                                                            
*                                                                               
         LA    R2,IOKEY                  SETUP KEY FOR PASSIVE ADD              
         XC    IOKEY,IOKEY                                                      
         MVI   MOFKTYP,MOFKTYPQ          C'O' MEDIA OFFICE RECORDS              
         MVI   MOFKSUB,MOFKS2Q           X'02' TWO BYTE OFFICE                  
         MVC   MOFKAGY,CUAALF            AGENCY ALPHA ID                        
         MVC   MOFK2OF,TWOOFF            TWO BYTE OFFICE                        
         MVC   MOFKSYS,SYSTEM                                                   
         MVC   MOFKC1OF,ONEOFF                                                  
         MVC   MOFKDA,LMOFDA             AND DISK ADDRESS                       
*                                                                               
         TM    LMOFIND,LMOFPRQ           PASSIVE RECORD TO BE RESTORED?         
         BZ    VR100                     . NO, JUST ADD IT                      
         GOTO1 AIO,IORDUPD+IOGENDIR+IO1                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL              SHOULD BE DELETED                      
         BZ    VALRECX                                                          
*                                                                               
         NI    MOFKSTAT,X'FF'-X'80'      UNDELETE                               
         MVC   MOFKC1OF,ONEOFF           MOVE IN CORRECT 1 BYTE OFFICE          
         MVC   MOFKDA,LMOFDA             AND DISK ADDRESS                       
*                                                                               
         LA    R1,IOWRITE+IOGENDIR+IO1                                          
         B     *+8                                                              
VR100    LA    R1,IOADD+IOGENDIR+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR110    MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF OFFICE RECORD                             *         
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
*                                   OFFICE CODE                                 
         OI    OFFOIDH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   OFFOID,MOFK2OF                                                   
*                                                                               
         GOTO1 ADISSYS,MOFKSYS      SYSTEM                                      
         MVC   OFFSYS,APWORK                                                    
*                                   OFFICE NUMBER                               
         OI    OFFNUMH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   OFFNUM,SPACES                                                    
         GOTO1 VHEXOUT,APPARM,MOFKC1OF,OFFNUM,1                                 
*                                                                               
DISKEYX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY OFFICE RECORD                                    *         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
*                                                                               
         OI    OFFOIDH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   OFFOID,MOFFC2OF      OFFICE CODE                                 
*                                                                               
         GOTO1 ADISSYS,MOFKSYS      SYSTEM                                      
         MVC   OFFSYS,APWORK                                                    
*                                   OFFICE NUMBER                               
         OI    OFFNUMH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   OFFNUM,SPACES                                                    
         GOTO1 VHEXOUT,APPARM,MOFK1OF,OFFNUM,1                                  
*                                                                               
         BAS   RE,DISONE            ORIGINAL ONE CHAR                           
*                                                                               
* DISPLAY RECORD DETAIL                                                         
         TWAXC OFFNOFH                                                          
         TWAXC OFFPREH                                                          
         TWAXC OFFSNMH                                                          
         TWAXC OFFLNMH                                                          
         TWAXC OFFUIDH                                                          
         TWAXC OFFPASH                                                          
         OI    OFFPREH+(FVATRB-FVIHDR),FVALOWI HIDE PREVIOUS UNLESS             
         OI    OFFPRHH+(FVATRB-FVIHDR),FVALOWI ONE EXISTS                       
*                                                                               
         LA    R3,MOFFIRST(R2)                                                  
DR030    CLI   0(R3),0                                                          
         BE    DISRECX                                                          
         CLI   0(R3),MONAMELQ      X'0A' NAME ELEMENT                           
         BE    DR050                                                            
         CLI   0(R3),MOOLDELQ      X'0B' PREVIOUS OFFICE ELEMENT                
         BE    DR055                                                            
         CLI   0(R3),MOUIPELQ      X'0C' WEBDAV USER ID AND PASSWORD            
         BE    DR060                                                            
         CLI   0(R3),MOSAPELQ      X'0D' SAP INTERFACE CODES                    
         BE    DR070                                                            
DR040    SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR030                                                            
*                                                                               
         USING MONAMD,R3                                                        
DR050    MVC   OFFSNM,MONAMSH                                                   
         MVC   OFFLNM,MONAMLO                                                   
         OI    OFFLNMH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    OFFSNMH+(FVOIND-FVIHDR),FVOXMT                                   
         B     DR040                                                            
*                                                                               
         USING MOOLDD,R3                                                        
DR055    MVC   OFFPRE,MOOLDOFF                                                  
         OI    OFFPREH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    OFFPREH+(FVATRB-FVIHDR),X'FF'-FVALOWI                            
         NI    OFFPRHH+(FVATRB-FVIHDR),X'FF'-FVALOWI                            
         OI    OFFPRHH+(FVOIND-FVIHDR),FVOXMT                                   
         B     DR040                                                            
*                                                                               
         USING MOUIPD,R3                                                        
DR060    MVC   OFFUID,MOUIPUID                                                  
         MVC   OFFPAS,MOUIPASS                                                  
         OI    OFFUIDH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    OFFPASH+(FVOIND-FVIHDR),FVOXMT                                   
         B     DR040                                                            
*                                                                               
         USING MOSAPD,R3                                                        
DR070    MVC   OFFSAC,MOSAPCOM                                                  
         MVC   OFFSAS,MOSAPORG                                                  
         MVC   OFFSAP,MOSAPCTR                                                  
         OI    OFFSACH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    OFFSASH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    OFFSAPH+(FVOIND-FVIHDR),FVOXMT                                   
         B     DR040                                                            
         DROP  R3                                                               
*                                                                               
DISRECX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* DISPLAY ORIGINAL ONE CHARACTER OFFICE                                         
*----------------------------------------------------------------------         
DISONE   NTR1                                                                   
         OI    OFFORHH+(FVATRB-FVIHDR),FVALOWI                                  
         OI    OFFORHH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    OFFORGH+(FVATRB-FVIHDR),FVALOWI                                  
         OI    OFFORGH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   OFFORG,SPACES                                                    
*                                                                               
         CLC   CUAALF,=C'SJ'       CONVERTED BEFORE INDICATOR                   
         BE    DISONE2                                                          
         CLC   CUAALF,=C'OO'       CONVERTED BEFORE INDICATOR                   
         BE    DISONE2                                                          
         TM    MOFFSTAT,MOFSCON    CONVERTED FROM ONE CHAR OFFS?                
         BNO   DISONEX                                                          
*                                                                               
DISONE2  LA    R1,DOFFTAB                                                       
         LHI   RF,DOFFTQ                                                        
DISONE4  CLC   MOFK1OF,0(R1)                                                    
         BE    DISONE6                                                          
         LA    R1,1(R1)                                                         
         BCT   RF,DISONE4                                                       
         B     DISONEX                                                          
*                                                                               
DISONE6  MVC   OFFORG,MOFK1OF                                                   
         NI    OFFORHH+(FVATRB-FVIHDR),X'FF'-FVALOWI                            
         NI    OFFORGH+(FVATRB-FVIHDR),X'FF'-FVALOWI                            
DISONEX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)              
***********************************************************************         
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                                         
***********************************************************************         
VALSEL   LA    R2,APRECKEY               BUILD FIRST RECORD KEY                 
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    MOFKEY(MOFKLEN),MOFKEY                                           
         MVI   MOFKTYP,MOFKTYPQ          C'O' MEDIA OFFICE RECORDS              
         MVI   MOFKSUB,MOFKS2Q           X'02' TWO BYTE CHAR OFFICE             
         MVC   MOFKAGY,CUAALF                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTOIDH                                                    
         BNE   VS010                                                            
         MVC   MOFK2OF,LSTOID                                                   
         OC    MOFK2OF,SPACES                                                   
*                                                                               
VS010    MVI   LSYSTEM,0                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTSYSH                                                    
         BNE   VS050                                                            
         GOTO1 AVALSYS,LSTSYSH           VALIDATE SYSTEM                        
         BNE   VALSELX                                                          
         MVC   LSYSTEM,APWORK            HOLD FOR LATER USE                     
*                                                                               
VS050    LA    R0,LSTACT1H               SET ADDRESS OF FIRST LIST LINE         
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H               SET LIST LINE LENGTH                   
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,(LSTFOOTH-LSTACT1H)/(LSTACT2H-LSTACT1H)                 
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                                   
***********************************************************************         
GETSEL   LA    R2,IOKEY                  READ NEXT RECORD                       
         MVC   MOFKEY,APRECKEY           FROM LAST KEY                          
         TM    APINDS,APILNSEQ           FIRST LINE IN LIST SEQUENCE            
         BNZ   GS020                                                            
         OI    APINDS,APILNSEQ                                                  
         LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GS020    LA    R1,IOGENDIR+IOSQ+IO1      ELSE NEXT LIST LINE                    
         GOTO1 AIO                                                              
         BNE   GSEND                                                            
*                                                                               
         MVC   APRECKEY(MOFKLEN),MOFKEY   SAVE LAST RECORD KEY                  
         CLI   MOFKTYP,MOFKTYPQ                                                 
         BNE   GSEND                      ELSE EXIT LIST                        
         CLI   MOFKSUB,MOFKS2Q                                                  
         BNE   GSEND                                                            
         CLC   MOFKAGY,CUAALF                                                   
         BNE   GSEND                                                            
         CLI   LSYSTEM,0                                                        
         BE    GS040                                                            
         CLC   LSYSTEM,MOFKSYS                                                  
         BNE   GS020                                                            
*                                                                               
GS040    GOTO1 AIO,IOGET+IOGENFIL+IO1                                           
         BNE   GSEND                     I/O ERROR EXIT                         
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GSEND    MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
GETSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                                      
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
         MVC   LISTCDE,MOFFC2OF    DISPLAY KEY DATA                             
*                                                                               
         GOTO1 ADISSYS,MOFKSYS     DISPLAY SYSTEM                               
         MVC   LISTSYS,APWORK                                                   
*                                                                               
         TM    CUSTAT,CUSDDS       SHOW ONLY ENTER ON DDS TERMINAL              
         BZ    DS020                                                            
         GOTO1 VHEXOUT,APPARM,MOFK1OF,LISTNUM,1                                 
*                                                                               
DS020    LA    R3,MOFFIRST(R2)     GET ELEMENT DATA                             
DS030    CLI   0(R3),0                                                          
         BE    DISSELX             END OF RECORD                                
         CLI   0(R3),MONAMELQ                                                   
         BE    DS050                                                            
         SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DS030                                                            
*                                                                               
         USING MONAMD,R3                                                        
DS050    MVC   LISTNAM,MONAMLO           OFFICE NAME                            
*                                                                               
DISSELX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)              
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
                                                                                
***********************************************************************         
* NEW SCREEN LOADED - SET SCREEN DATA                                           
***********************************************************************         
SETSCR   GOTO1 AGETAAD,CUAALF                                                   
         MVI   SAPAGY,C'Y'                                                      
         TM    APWORK+5,CTAGSAP          TEST SAP AGENCY                        
         BO    SETSCRX                                                          
         MVI   SAPAGY,C'N'               SET NON-SAP AGENCY                     
         MVC   OFFSACN,SPACES            CLEAR SAP FIELDS AND NAMES             
         MVC   OFFSASN,SPACES                                                   
         MVC   OFFSAPN,SPACES                                                   
         MVC   OFFSAC,SPACES                                                    
         MVC   OFFSAS,SPACES                                                    
         MVC   OFFSAP,SPACES                                                    
         OI    OFFSACH+FHATD,FHATPR      PROTECT AND                            
         OI    OFFSASH+FHATD,FHATPR                                             
         OI    OFFSAPH+FHATD,FHATPR                                             
         OI    OFFSACNH+FHOID,FHOITR     TRANSMIT                               
         OI    OFFSASNH+FHOID,FHOITR                                            
         OI    OFFSAPNH+FHOID,FHOITR                                            
         OI    OFFSACH+FHOID,FHOITR                                             
         OI    OFFSASH+FHOID,FHOITR                                             
         OI    OFFSAPH+FHOID,FHOITR                                             
SETSCRX  B     EXIT                                                             
                                                                                
***********************************************************************         
* POPULATE MEDIA OFFICE RECORDS                                                 
***********************************************************************         
GOPOP    NTR1                                                                   
*                                                                               
         LA    R6,MOFSYSL                VALID MEDIA OFFICE SYSTEM TAB          
         B     *+8                                                              
GP010    LA    R6,1(R6)                                                         
*                                                                               
         LA    R4,DOFFTAB                DEFAULT OFFICE TABLE                   
*                                                                               
GP020    CLI   0(R4),0                   END OF DEFAULT OFFICE TABLE?           
         BE    GP010                     . YES                                  
         CLI   0(R6),0                   END OF VALID SYSTEM TABLE?             
         BE    GOPOPX                    . YES, EXIT                            
*                                                                               
         L     R2,AIOAREA1                                                      
         XC    0(256,R2),0(R2)                                                  
*                                                                               
         USING MOFRECD,R2                                                       
         MVI   MOFKTYP,MOFKTYPQ          OFFICE LIMIT ACCESS RECORD             
         MVI   MOFKSUB,MOFKS1Q           ONE BYTE PRIMARY RECORD                
         MVC   MOFKAGY,CUAALF            AGENCY ALPHA                           
         MVC   MOFK1OF,0(R4)             ONE BYTE OFFICE (PRIMARY)              
         MVC   MOFKSYS,0(R6)             SYSTEM NUMBER                          
         MVC   MOFFC2OF,SPACES                                                  
         MVC   MOFFC2OF(1),0(R4)         TWO BYTE OFFICE (PRIMARY)              
         OI    MOFFSTAT,MOFSCON                                                 
*                                                                               
         LA    R3,MOFFIRST(R2)                                                  
         USING MONAMD,R3                                                        
         MVI   MONAMEL,MONAMELQ          X'0A' ELEMENT CODE                     
         MVI   MONAMLN,MONAMLNQ          ELEMENT LENGTH                         
         MVC   MONAMSH,SPACES                                                   
         MVC   MONAMLO,SPACES                                                   
         MVC   MONAMSH(1),0(R4)          SHORT NAME                             
         MVC   MONAMLO(1),0(R4)          LONG NAME                              
*                                                                               
         LHI   R1,MOFFIRST+MONAMLNQ+1                                           
         STCM  R1,3,MOFFLEN              RECORD LENGTH                          
         DROP  R3                                                               
*                                                                               
         MVC   IOKEY,0(R2)                                                      
*                                                                               
         GOTO1 AIO,IOADD+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LMOFDA,IODA               SAVE DISK ADDRESS                      
*                                                                               
         LA    R2,IOKEY                  SETUP KEY FOR PASSIVE ADD              
         XC    IOKEY,IOKEY                                                      
         MVI   MOFKTYP,MOFKTYPQ          C'O' MEDIA OFFICE RECORDS              
         MVI   MOFKSUB,MOFKS2Q           X'02' TWO BYTE OFFICE                  
         MVC   MOFKAGY,CUAALF            AGENCY ALPHA ID                        
         MVC   MOFK2OF,SPACES            TWO BYTE OFFICE                        
         MVC   MOFK2OF(1),0(R4)          TWO BYTE OFFICE                        
         MVC   MOFKSYS,0(R6)                                                    
         MVC   MOFKC1OF,0(R4)                                                   
         MVC   MOFKDA,LMOFDA             AND DISK ADDRESS                       
*                                                                               
         GOTO1 AIO,IOADD+IOGENDIR+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,1(R4)                  NEXT OFFICE                            
         B     GP020                                                            
*                                                                               
GOPOPX   B     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ERROR EXITS                                                                   
***********************************************************************         
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                INPUT FIELD ERROR                            
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXIT                MISSING FIELD                                
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     EXIT                I/O ERROR                                    
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXIT                RECORD NOT FOUND                             
SAEDIF   MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     EXIT                DUPLICATE                                    
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     EXIT                ALREADY EXISTS                               
SAESYE   MVC   FVMSGNO,=AL2(CE#INCSS)                                           
         B     EXIT                INVALID SYSTEM                               
SAEPOP   MVC   FVMSGNO,=AL2(CE#RDADE)                                           
         B     EXIT                RECORDS ADDED ELSEWHERE                      
*                                                                               
         LTORG                                                                  
FFILL    DC    32X'FF'                                                          
SPACES   DC    32C' '                                                           
*                                                                               
MOFSYSL  DC    X'02'                                                            
         DC    X'04'                                                            
         DC    X'00'         ONLY SPOT, PRINT VALID FOR MOF                     
*                                                                               
DOFFTAB  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ' LETTERS                            
         DC    C'123456789'                  NUMBERS                            
         DC    C'º\;/'                      SPECIAL CHARACTERS                 
         DC    C'!@#%&&()_+{}|:<>'           (SHIFT) SPECIAL CHARACTERS         
DOFFTQ   EQU   *-DOFFTAB       ` ~ ^ . ' " TAKEN OUT OF LIST                    
         DC    X'00'                                                            
*                                                                               
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
*                                                                               
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSAAD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSABD                                                       
*                                                                               
         ORG   SAVOVER                                                          
ONEOFF   DS    X                   ONEOFF                                       
TWOOFF   DS    CL2                 TWOOFF                                       
SYSTEM   DS    X                   SYSTEM NUMBER                                
LSYSTEM  DS    X                   SYSTEM FILTER FOR LIST                       
LMOFIND  DS    X                   LOCAL MEDIA OFFICE INDICATOR                 
LMOFPRQ  EQU   X'80'               . PASSIVE POINTER RESTORE                    
LMOFDA   DS    A                   OFFICE RECORD DISK ADDRESS                   
SAPAGY   DS    C                   SAP AGENCY                                   
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* LIST/SELECT LINE LAYOUT                                                       
***********************************************************************         
LISTD    DSECT                                                                  
LISTSELH DS    XL8                                                              
LISTSEL  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLINX DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTCDE  DS    CL2                 OFFICE CODE                                  
         DS    CL1                                                              
LISTSYS  DS    CL10                OFFICE SYSTEM                                
         DS    CL1                                                              
LISTNAM  DS    CL20                OFFICE NAME                                  
         DS    CL29                                                             
LISTNUM  DS    CL3                 OFFICE NUMBER                                
         DS    CL1                                                              
LISTONE  DS    CL1                 OFFICE NUMBER CHARACTER                      
         ORG   LISTLIN+L'LISTLIN                                                
                                                                                
***********************************************************************         
* DSECT TO COVER LOCAL W/S                                                      
***********************************************************************         
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
OLDOFF   DS    CL2                 SAVED 2 BYTE OLD OFFICE CODE                 
LOCALX   EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SEACS19   06/18/15'                                      
         END                                                                    
