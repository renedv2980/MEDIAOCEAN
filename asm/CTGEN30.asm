*          DATA SET CTGEN30    AT LEVEL 001 AS OF 11/06/17                      
*PHASE TA0B30A                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE NUMVAL                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE PWDVAL                                                                 
         TITLE 'CTGEN30 - FILE MAINTENANCE - AGENCY ACCESS 2'                   
GEN30    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GE30**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         L     R0,=V(SCINKEY)                                                   
         AR    R0,RE                                                            
         ST    R0,VSCINKEY                                                      
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     EXIT      DELREC                                                 
         B     EXIT      RESREC                                                 
         B     EXIT      VALSEL                                                 
         B     EXIT      GETSEL                                                 
         B     EXIT      DISSEL                                                 
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     EXIT      VALREQ                                                 
         B     EXIT      PRTREP                                                 
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
MODEXIT  CLI   APMODE,APMVALK          VALKEY?                                  
         JE    EXIT                    YES: EXIT                                
         CLI   APPFKEY,2               ANY PFKEY PRESSED?                       
         JNE   EXIT                    NO: EXIT                                 
         CLI   APACTN,ACTDIS           ACTION=DISPLAY                           
         JE    MODEXIT1                NO: EXIT                                 
         CLI   APACTN,ACTCHA           ACTION=DISPLAY                           
         JNE   EXIT                    NO: EXIT                                 
MODEXIT1 XC    APCURSOR,APCURSOR       DON'T SET CURSOR                         
         MVI   APMODE,APMSWP           SWAP                                     
         MVI   APPARM,RECACC           SWAP RECORD                              
         MVI   APPARM+1,ACTDIS         SWAP ACTION                              
         MVI   APPFKEY,0               ANY PFKEY PRESSED?                       
*                                                                               
EXIT     XIT1  ,                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF ACCESS TYPE RECORD                       *         
***********************************************************************         
VALKEY   LA    R2,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,INAALPHH                                                   
         BNE   VALKEYX                                                          
*                                                                               
VK012    MVC   CT5KALPH,FVIFLD                                                  
         MVC   APRECKEY(L'CT5KEY),CT5KEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         B     VALKEYY                                                          
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    VALKEYX                                                          
         MVI   APINDS,APIOKDIS                                                  
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     MODEXIT                                                          
                                                                                
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN ACCESS TYPE RECORD                      *         
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
                                                                                
*----------------------------------                                             
* SYSTEM SERVICE TEAM                                                           
*----------------------------------                                             
         GOTO1 AFVAL,INATEAMH      VALIDATE SERVICE TEAM                        
         BNE   VR020                                                            
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAADELQ                                                  
         GOTO1 AGETELS,CT5REC                                                   
         ICM   R3,15,APPARM        GET THE AGENCY ACCESS DETAILS                
         BZ    EIIF                NOT THERE?  THAT'S WEIRD                     
         USING CTAADD,R3                                                        
*                                                                               
         SR    R1,R1               GET INPUT LENGTH                             
         IC    R1,FVXLEN                                                        
         LA    RF,TEAMLST                                                       
         USING TEAMLSTD,RF                                                      
VR014    CLI   TEAMNUM,X'FF'       END OF TEAM LIST                             
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   INATEAM(0),TEAMNAME MATCH ON TEAM NAME                           
         BE    VR016                                                            
         LA    RF,L'TEAMLST(RF)    NEXT NAME                                    
         B     VR014                                                            
*                                                                               
VR016    MVC   CTAACSTN,TEAMNUM    SAVE TEAM NUMBER                             
         MVC   INATEAM,TEAMNAME                                                 
         OI    INATEAMH+6,X'80'                                                 
         DROP  RF                                                               
                                                                                
*----------------------------------                                             
* AGENCY ACCESS DATES                                                           
*----------------------------------                                             
VR020    XC    APELEM,APELEM       REMOVE AGENCY ACCESS DATES                   
         MVI   APELEM,CTACCELQ                                                  
         GOTO1 ADELELS,CT5REC                                                   
*                                                                               
         XC    APELEM,APELEM       PREPARE TO BUILD NEW ONES                    
         LA    R3,APELEM                                                        
         USING CTACCD,R3                                                        
         MVI   CTACCEL,CTACCELQ                                                 
         MVI   CTACCLEN,CTACCLNQ                                                
*                                                                               
         GOTO1 AFVAL,INAANADH                                                   
         BNE   VR040                                                            
         GOTO1 VDATVAL,APPARM,(0,FVIFLD),APWORK                                 
         OC    APPARM(4),APPARM                                                 
         BZ    EIIF                                                             
         GOTO1 VDATCON,APPARM,(0,APWORK),(3,CTACCNAD)                           
*                                                                               
VR040    GOTO1 AFVAL,INAARODH                                                   
         BNE   VR050                                                            
         GOTO1 VDATVAL,APPARM,(0,FVIFLD),APWORK                                 
         OC    APPARM(4),APPARM                                                 
         BZ    EIIF                                                             
         GOTO1 VDATCON,APPARM,(0,APWORK),(3,CTACCROD)                           
*                                                                               
VR050    LR    R4,R3                                                            
SYS      USING CTACCD,R4                                                        
*                                                                               
         LA    R9,INASYS1H                                                      
VR060    LA    RF,INASYSLH                                                      
         CR    R9,RF                   DID WE HIT THE LAST LINE?                
         BH    VR100                                                            
*                                                                               
         GOTO1 AFVAL,(R9)              ANYTHING IN THE FIELD?                   
         BNE   VR090                   NO: WE'RE DONE                           
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AVALSYS,(R9)            VALIDATE SYSTEM NAME                     
         BNE   VALRECX                                                          
         L     RF,APPARM               APPARM = A(SYSLIST ENTRY)                
         USING SYSLSTD,RF                                                       
         MVC   SYS.CTACCSYS,SYSLNUM    SYSTEM NUMBER                            
         OI    FHOID(R9),FHOITR        TRANSMIT                                 
         MVC   FHDAD(L'SYSLNAME,R9),SYSLNAME  DISPLAY FULL SYSTEM NAME          
         DROP  RF                                                               
*                                                                               
         LA    R1,INANAD1H-INASYS1H(,R9)  SYSTEM NO ACCESS DATE                 
         GOTO1 AFVAL,(R1)                                                       
         BNE   VR070                                                            
         GOTO1 VDATVAL,APPARM,(0,FVIFLD),APWORK                                 
         OC    APPARM(4),APPARM                                                 
         BZ    EIIF                                                             
         GOTO1 VDATCON,APPARM,(0,APWORK),(3,SYS.CTACCSNA)                       
*                                                                               
VR070    LA    R1,INAROD1H-INASYS1H(,R9)  SYSTEM READ-ONLY DATE                 
         GOTO1 AFVAL,(R1)                                                       
         BNE   VR080                                                            
         GOTO1 VDATVAL,APPARM,(0,FVIFLD),APWORK                                 
         OC    APPARM(4),APPARM                                                 
         BZ    EIIF                                                             
         GOTO1 VDATCON,APPARM,(0,APWORK),(3,SYS.CTACCSRO)                       
*                                                                               
VR080    LA    R4,CTACCSYL(,R4)        BUMP TO NEXT SPOT IN ELEMENT             
         LLC   RF,CTACCLEN                                                      
         AHI   RF,CTACCSYL                                                      
         STC   RF,CTACCLEN             BUMP UP ELEMENT LENGTH                   
*                                                                               
VR090    LA    R9,INASYS2H-INASYS1H(,R9)                                        
         B     VR060                                                            
         DROP  SYS                                                              
*                                                                               
VR100    OC    CTACCNAD,CTACCNAD       DO WE HAVE A NO ACCESS,                  
         BNZ   VR110                   ...                                      
         OC    CTACCROD,CTACCROD       READ ONLY,                               
         BNZ   VR110                   ...                                      
         CLI   CTACCSYS,0              OR SYSTEM SPECIFIC DATES?                
         BZ    VR120                   NO: WE DON'T NEED THIS ELEMENT           
VR110    GOTO1 AADDELN,CT5REC          YES: ADD ELEMENT TO RECORD               
         BNE   VALRECX                 RECORD TOO BIG                           
*                                                                               
VR120    GOTO1 ASETACT,CT5REC          SET CONTROL FILE ACTIVITY ELEM           
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         JNE   *+2                                                              
VALRECXY MVC   FVMSGNO,=AL2(FVFOK)                                              
VALRECX  B     MODEXIT                                                          
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF ACCESS TYPE RECORD                                  
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         MVC   INAALPH,CT5KALPH                                                 
         OI    INAALPHH+FHOID,FHOITR                                            
*                                                                               
DISKEYX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     MODEXIT                                                          
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY ACCESS TYPE RECORD                                         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
         TWAXC INAUSERH,INAUSNMH,PROT=Y,TRNS=Y                                  
         TWAXC INADTAH,INADTAH,PROT=Y,TRNS=Y                                    
         TWAXC INACTRYH,INACTRYH,PROT=Y,TRNS=Y                                  
         TWAXC INATEAMH,INAFOOTH,TRNS=Y                                         
*                                                                               
         LA    R3,CT5DATA          DISPLAY EACH ELEMENT OF RECORD               
DR010    CLI   0(R3),0             E-O-R                                        
         BE    DISRECX                                                          
         CLI   0(R3),CTDSCELQ      USER-ID                                      
         BE    DR020                                                            
         CLI   0(R3),CTAGDELQ      AGENCY GROUP DETAILS                         
         BE    DR030                                                            
         CLI   0(R3),CTAADELQ      AGENCY ACCESS DETAILS                        
         BE    DR040                                                            
         CLI   0(R3),CTACCELQ      X'1C' AGENCY ACCESS DATES                    
         BE    DR050                                                            
*                                                                               
DR015    LLC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     DR010                                                            
                                                                                
*----------------------------------                                             
* PRINCIPAL ID                                                                  
*----------------------------------                                             
         USING CTDSCD,R3                                                        
DR020    GOTO1 GETID,CTDSC                                                      
         MVC   INAUSER,USERID                                                   
         MVC   INAUSNM,USERNM                                                   
         B     DR015                                                            
                                                                                
*----------------------------------                                             
* AGENCY GROUP DETAILS                                                          
*----------------------------------                                             
         USING CTAGDD,R3                                                        
DR030    MVC   PREAGOPT,CTAGOPTS   SAVE OFF VALUES                              
         BRAS  RE,DISPAGYT         R3 IS SET TO CTAGDD                          
*                                                                               
         CLI   CTAGDLEN,CTAGDL2Q                                                
         BL    DR015                                                            
         L     R1,ACTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         CLC   CTRYCODE,CTAGDCTY                                                
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   INACTRY,CTRYNAM     DISPLAY COUNTRY NAME                         
         B     DR015                                                            
         DROP  R1                                                               
                                                                                
*----------------------------------                                             
* AGENCY ACCESS DETAILS                                                         
*----------------------------------                                             
         USING CTAADD,R3                                                        
DR040    CLI   CTAACSTN,0           CLIENT SERVICE TEAM NAME                    
         BE    DR015                                                            
         LA    RF,TEAMLST                                                       
         USING TEAMLSTD,RF                                                      
DR044    CLI   TEAMNUM,X'FF'        END OF TEAM LIST                            
         BE    DR046                                                            
         CLC   TEAMNUM,CTAACSTN                                                 
         BE    *+12                                                             
         LA    RF,L'TEAMLST(,RF)                                                
         B     DR044                                                            
         MVC   INATEAM,TEAMNAME                                                 
         B     DR015                                                            
DR046    MVC   INATEAM(2),=C'??'                                                
         B     DR015                                                            
         DROP  RF                                                               
                                                                                
*----------------------------------                                             
* AGENCY ACCESS DATES                                                           
*----------------------------------                                             
         USING CTACCD,R3                                                        
DR050    OC    CTACCNAD,CTACCNAD                                                
         BZ    DR051                                                            
         GOTO1 VDATCON,APPARM,(3,CTACCNAD),(21,INAANAD)                         
         OI    INAANADH+FHOID,FHOITR                                            
DR051    OC    CTACCROD,CTACCROD                                                
         BZ    DR060                                                            
         GOTO1 VDATCON,APPARM,(3,CTACCROD),(21,INAAROD)                         
         OI    INAARODH+FHOID,FHOITR                                            
*                                                                               
DR060    LR    R4,R3                                                            
SYS      USING CTACCD,R4                                                        
*                                                                               
         LA    R9,INASYS1H                                                      
DR070    LA    RE,SYS.CTACCSYS+CTACCSYL IS THERE ANOTHER ENTRY                  
         LLC   RF,CTACCLEN             LENGTH OF ELEMENT                        
         AR    RF,R3                   A(NEXT ELEMENT)                          
         CR    RE,RF                   ANYTHING LEFT IN THIS ELEMENT?           
         BH    DR015                   NO: EXIT                                 
                                                                                
         CLI   SYS.CTACCSYS,0          NO SHOWS, NO SHIRT, NO SERVICE           
         BE    DR015                                                            
         GOTO1 ADISSYS,SYS.CTACCSYS    DISPLAY THE SYSTEM                       
         OI    FHOID(R9),FHOITR                                                 
         MVC   FHDAD(L'SYSLNAME,R9),APWORK                                      
*                                                                               
         OC    SYS.CTACCSNA,SYS.CTACCSNA                                        
         BZ    DR080                                                            
         LA    RF,INANAD1H-INASYS1H(,R9) SYSTEM NO ACCESS DATE                  
         OI    FHOID(RF),FHOITR          TRANSMIT                               
         LA    RF,FHDAD(,RF)             FIELD DATA                             
         GOTO1 VDATCON,APPARM,(3,SYS.CTACCSNA),(21,(RF))                        
*                                                                               
DR080    OC    SYS.CTACCSRO,SYS.CTACCSRO                                        
         BZ    DR090                                                            
         LA    RF,INAROD1H-INASYS1H(,R9) SYSTEM READ-ONLY DATE                  
         OI    FHOID(RF),FHOITR          TRANSMIT                               
         LA    RF,FHDAD(,RF)             FIELD DATA                             
         GOTO1 VDATCON,APPARM,(3,SYS.CTACCSRO),(21,(RF))                        
*                                                                               
DR090    LA    R4,CTACCSYL(,R4)        BUMP TO NEXT SPOT IN ELEMENT             
         LA    R9,INASYS2H-INASYS1H(,R9)                                        
         B     DR070                   NO: GET ANOTHER ONE                      
         DROP  SYS                                                              
*                                                                               
DISRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     MODEXIT                                                          
         DROP  R3                                                               
                                                                                
*********************************************************************           
* DISPLAY "DDS AGY TYPE"                                                        
*********************************************************************           
         USING AGYTYPD,RE                                                       
         USING CTAGDD,R3                                                        
DISPAGYT DS    0H                  WHAT TYPE OF COMPANY FILE                    
         STM   RE,RF,SVRERF        SAVE RF SO WE CAN SEE WHENCE WE CAME         
         NI    INADTAH+1,X'FF'-X'08'                                            
         LARL  RE,AGYTYP                                                        
         LA    R1,WORK                                                          
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK   MOVE SPACES IN                           
DISAGY10 CLI   0(RE),X'FF'         END OF TABLE, THEN DONE                      
         BE    DISAGY90            YES                                          
         LLC   RF,AGYOPT           EXTACT OPTION BIT TO TEST                    
         EX    RF,AGYTYTM          TEST BIT                                     
         BZ    DISAGY30            NOT A HIT SO NEXT ENTRY                      
         LA    RF,WORK             BIT MATCHED                                  
         CR    R1,RF               SEE IF FIRST MATCH OF THE MOMENT             
         BE    DISAGY18            IF THE SAME, THEN 1ST ENTRY                  
         MVC   0(1,R1),SCCOMMA     MUST BE 2ND ENTRY SO PUT A COMMA             
         AHI   R1,1                BUMP PAST COMMA                              
*                                                                               
DISAGY18 LLC   RF,AGYMAXL          GET FULL LENGTH                              
         BCTR  RF,0                LESS ONE FOR EXCUTE                          
         EX    RF,AGYTYMVC         MVC INTO WORK                                
         LA    R1,1(R1,RF)         BUMP UP TO END OF DATA MOVED                 
*                                                                               
DISAGY30 LA    RE,AGYTYLNQ(,RE)    KEEP GOING TILL END OF TABLE                 
         B     DISAGY10                                                         
*----------------------------------                                             
* EXCUTE STATEMENTS                                                             
*----------------------------------                                             
AGYTYTM  TM    CTAGOPTS,0                                                       
AGYTYMVC MVC   0(0,R1),AGYTEXT                                                  
*                                                                               
DISAGY90 LA    RF,WORK             DID WE BUILD ANYTHING?                       
         CR    R1,RF               IF THE SAME THEN NO                          
         BNE   DISAGY92            YES WE DID                                   
         LLC   RF,AGYMAXL          MAX LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,AGYTYMVC                                                      
         DROP  RE                                                               
*                                                                               
DISAGY92 MVC   INADTA,WORK         MOVE IN WHAT WE BUILT                        
         OI    INADTAH+FHOID,FHOITR                                             
         L     RE,SVRE             JUST RESTORE RF                              
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
                                                                                
***********************************************************************         
* HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)                         
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     MODEXIT                                                          
                                                                                
***********************************************************************         
* GET ID INFO FROM ID RECORDS                                         *         
* R1 POINTS TO ID NUMBER                                              *         
***********************************************************************         
GETID    NTR1  ,                                                                
         XC    USERVAL(USERVALL),USERVAL                                        
         MVC   USERNO,0(R1)        EXTRACT ID NUMBER                            
         MVC   USERID(3),=C'ID='                                                
         SR    R0,R0                                                            
         ICM   R0,3,USERNO                                                      
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  USERID+3(5),APDUB                                                
         MVI   USERNM,C'?'                                                      
         MVC   USERNM+1(L'USERNM-1),USERNM                                      
*                                                                               
         L     R4,AIOAREA2         READ ID RECORD FROM CONTROL FILE             
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USERNO                                               
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         JL    *+2                                                              
         BH    GETIDX                                                           
*                                                                               
         LA    R1,CTIDATA          EXTRACT INFO FROM ELEMENTS                   
GETID2   CLI   0(R1),0                                                          
         BE    GETIDX                                                           
         CLI   0(R1),CTDSCELQ                                                   
         BNE   *+14                                                             
         MVC   USERID,CTDSC-CTDSCD(R1)                                          
         B     GETID4                                                           
         CLI   0(R1),CTAGYELQ                                                   
         BNE   *+14                                                             
         MVC   USERAL,CTAGYID-CTAGYD(R1)                                        
         B     GETID4                                                           
         CLI   0(R1),CTDSTELQ                                                   
         BNE   *+14                                                             
         MVC   USERNM,CTDSTNAM-CTDSTD(R1)                                       
         B     GETID4                                                           
*                                                                               
GETID4   LLC   R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     GETID2                                                           
*                                                                               
GETIDX   MVC   FVMSGNO,=AL2(FVFOK) IGNORE IO ERROR MESSAGE                      
         J     EXIT                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* GET SYSTEM EXECUTIVE LIST VALUES                                    *         
***********************************************************************         
GETSE    NTR1  ,                                                                
         L     R1,ASYSFACS         GET SYSTEM LIST INFO                         
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SENAME,APWORK                                                    
         BE    GETSEOK                                                          
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)                                            
         LA    R0,1                                                             
         B     GETSEX                                                           
*                                                                               
GETSEOK  MVC   APWORK(2),SESYS                                                  
         SR    R0,R0                                                            
*                                                                               
GETSEX   LTR   R0,R0                                                            
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
***********************************************************************         
* COUNT SYSTEM ENTRIES IN PHASE LIST                                  *         
* SYSTEM # AT 0(R1) COUNT IN APWORK(1)                                *         
***********************************************************************         
CNTSE    NTR1  ,                                                                
         MVI   APWORK,0                                                         
         MVC   APWORK+1(1),0(R1)                                                
         L     R1,ASYSFACS         GET SYSTEM LIST INFO                         
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
CNTSE1   CLC   SEOVSYS(1),APWORK+1                                              
         BNE   CNTSE2                                                           
         ZIC   R4,APWORK                                                        
         LA    R4,1(R4)                                                         
         STC   R4,APWORK                                                        
CNTSE2   BXLE  R1,RE,CNTSE1                                                     
         J     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET COUNTRY TABLE VALUES                                            *         
***********************************************************************         
GETCT    NTR1  ,                                                                
         L     R1,ACTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         ZIC   R4,FVILEN                                                        
         BCTR  R4,0                R4=L'INPUT-1                                 
GETCT1   CLI   FVILEN,L'CTRYSHR                                                 
         BH    GETCT2                                                           
         EX    R4,*+8              MATCH ON SHORT NAME                          
         BE    GETCTOK                                                          
         CLC   CTRYSHR(0),FVIFLD                                                
GETCT2   EX    R4,*+8                                                           
         BE    GETCTOK                                                          
         CLC   CTRYNAM(0),FVIFLD   MATCH ON LONG NAME                           
         BXLE  R1,RE,GETCT1                                                     
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         LA    R0,1                                                             
         B     GETCTX                                                           
*                                                                               
GETCTOK  MVC   CURR,CTRYCURR                                                    
         MVC   APWORK(L'CTRYCODE),CTRYCODE                                      
         MVC   APWORK+L'CTRYCODE(L'CTRYNAM),CTRYNAM                             
         SR    R0,R0                                                            
*                                                                               
GETCTX   LTR   R0,R0                                                            
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
**********************************************************************          
* EXITS                                                                         
**********************************************************************          
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     NO                  INPUT FIELD INVALID                          
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         J     EXIT                                                             
                                                                                
***********************************************************************         
* LITERALS AND CONSTANTS                                                        
***********************************************************************         
         LTORG                                                                  
*                                                                               
       ++INCLUDE DDTEAMLST                                                      
*                                                                               
         DS    0D                                                               
AGYTYP   DC    X'01',AL1(2,3,CTAGTST),CL4'TST'   TEST                           
         DC    X'01',AL1(2,3,CTAGTNG),CL4'TRN'   TRAINING                       
         DC    X'01',AL1(1,3,CTAGUAT),CL4'UAT'   UAT                            
         DC    X'02',AL1(1,3,CTAGSAP),CL4'SAP'   SAP                            
         DC    X'FF',AL1(1,4,0000000),CL4'NONE'  NONE                           
                                                                                
**********************************************************************          
* SHARED DSECTS                                                                 
**********************************************************************          
* DDTEAMLSTD                                                                    
       ++INCLUDE DDTEAMLSTD                                                     
* DDSCANBLKD                                                                    
       ++INCLUDE DDSCANBLKD                                                     
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
* CTGENWEB                                                                      
       ++INCLUDE CTGENWEB                                                       
* DDCTRYEQUS                                                                    
       ++INCLUDE DDCTRYEQUS                                                     
* SEACSFILE                                                                     
       ++INCLUDE SEACSFILE                                                      
*                                                                               
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGEN91D                                                       
         ORG                                                                    
                                                                                
**********************************************************************          
* INTERNAL DSECTS                                                               
**********************************************************************          
AGYTYPD  DSECT                                                                  
AGYCOMBO DS    X                   COMBO TESTING BIT (ALLOWABLE COMBOS)         
AGYMINL  DS    AL1                 MIN LENGTH                                   
AGYMAXL  DS    AL1                 MAX LENGTH                                   
AGYOPT   DS    X                   AGENCY TYPE FLAG OPTION                      
AGYTEXT  DS    CL4                 TEXT TO VALIDATE / DISPLAY                   
AGYTYLNQ EQU   *-AGYTYPD                                                        
*                                                                               
***********************************************************************         
* DSECT TO COVER LOCAL W/S                                            *         
***********************************************************************         
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
SELKEY   DS    0XL10               SELECT KEY PARAMETERS                        
SELCTY   DS    XL1                 COUNTRY                                      
SELGID   DS    CL2                 GROUP ID                                     
SELHID   DS    CL2                 HOLDING ID                                   
SELTYP   DS    CL1                 CLIENT TYPE                                  
SELSYS   DS    CL1                 SYSTEM #                                     
SELSECA  DS    CL2                 SECURITY AGENCY                              
SELSAOQ  EQU   1                   SELECT SECURITY AGENCIES ONLY                
SELCSTN  DS    XL1                 CLIENT SERVICE TEAM NUMBER                   
         ORG   SELKEY+L'SELKEY                                                  
VSCINKEY DS    V                   UNSCAN ROUTINE ADDRESS                       
DMCB     DS    8F                                                               
SVRERF   DS    0A                                                               
SVRE     DS    2A                                                               
BYTE     DS    X                                                                
WORK     DS    XL256                                                            
CTRY     DS    XL1                 COUNTRY CODE                                 
CURR     DS    CL3                 CURRENCY                                     
HOLDID   DS    XL2                 HOLDING COMPANY ID                           
GRUPID   DS    XL2                 AGENCY GROUP ID                              
CLTYPE   DS    CL1                 CLIENT TYPE                                  
DDSACC   DS    XL1                 DDS ACCESS LEVEL FLAG                        
SYSCNT   DS    X                   SYSTEM # FIELD COUNT                         
FLDCNT   DS    X                   SUB-FIELD COUNT                              
SECAFLAG DS    CL1                 SECURITY AGENCY FILTER FLAG                  
ACDFLAG  DS    XL1                 ACCESS DETAIL FLAG                           
ACDPPSQ  EQU   X'80'               PPS=Y FOUND ON ACCESS DETAIL ELEM            
ACDLABQ  EQU   X'40'               AGENCY LABEL FOUND                           
ACDAICQ  EQU   X'20'               AGENCY IN CAG FIELD                          
ACDAADQ  EQU   X'10'               AGENCY ACCESS DETAILS FOUND                  
PRELAB   DS    CL4                 PREVIOUS AGENCY LABEL                        
PREAGOPT DS    X                   PREVIOUS CTAGOPTS                            
USERVAL  DS    0C                  USER ID VALUES                               
USERID   DS    CL10                USER-ID CODE                                 
USERNM   DS    CL33                USER-ID NAME                                 
USERNO   DS    CL2                 USER-ID NUMBER                               
USERAL   DS    CL2                 USER-ID ALPHA-ID                             
USERVALL EQU   *-USERVAL                                                        
ACCSE    DS    CL1                 ACCOUNT SE # SAVE                            
MEDSE    DS    CL1                 MEDIA SE # SAVE                              
MEDAGB   DS    CL1                 MEDIA AGENCY BINARY SAVE                     
ACCFLAG  DS    CL1                 ACCOUNT SE MATCH FLAG                        
MEDFLAG  DS    CL1                 MEDIA SE MATCH FLAG                          
SELSTEAM DS    CL14                SERVICE TEAM                                 
LISTBUFF DS    CL256               SYSTEM INFO DISPLAY BUFFER                   
*                                                                               
OVSYS    DS    XL256                                                            
BLOCKI   DS    20CL32              SCAN BLOCK                                   
         ORG   BLOCKI                                                           
BLOCKO   DS    20CL20              UNSCAN BLOCK                                 
         ORG                                                                    
*                                                                               
LOCALX   EQU   *                   END OF LOCAL WORKING STORAGE                 
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001CTGEN30   11/06/17'                                      
         END                                                                    
