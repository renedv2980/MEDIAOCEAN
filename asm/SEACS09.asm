*          DATA SET SEACS09    AT LEVEL 007 AS OF 01/25/18                      
*PHASE TA0D09A                                                                  
ACS09    TITLE '- SECURITY ACCESS - ACCESS RECORDS'                             
ACS09    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS9**,RA,R4,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         USING SAPGMD,RSAPGMEL                                                  
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    R4,APBASE3                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         LA    RE,SAVOVER                                                       
         LA    RE,ATAB-SAVOVER(RE)                                              
         ST    RE,AATAB            SAVE A(RECORD TABLE)                         
         LA    RE,RTAB-ATAB(RE)                                                 
         ST    RE,ARTAB            SAVE A(ACTION TABLE)                         
         LA    RE,OTAB-RTAB(RE)                                                 
         ST    RE,AOTAB            SAVE A(OPTION TABLE)                         
         LA    RE,FTAB-OTAB(RE)                                                 
         ST    RE,AFTAB            SAVE A(FIELD TABLE)                          
*                                                                               
         XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     DELREC              05 - APMDELR                                 
         B     RESREC              06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     XIT                 10 - APMVALS                                 
         B     FSTLST              11 - APMFLST                                 
         B     XIT                 12 - APMPROC                                 
         B     XIT                 13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     VALREQ              15 - APMVALQ                                 
         B     PRTREP              16 - APMREPP                                 
         B     SETTWA              17 - APMSETT                                 
         B     XIT                 18 - APMPUTK                                 
         B     XIT                 19 - APMNEWK                                 
         B     XIT                 20 - APMFRP                                  
         B     XIT                 21 - APMDISS2                                
*                                                                               
XIT      XIT1  ,                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF ACCESS RECORD                            *         
*  - VALIDATE INPUT FIELDS & INITIALIZE RECORD KEY                    *         
***********************************************************************         
VALKEY   CLI   APACTN,ACTGRD       ACTION  GRIDS?                               
         BNE   *+12                . NO                                         
         MVI   APINDS,APIOKDIS     DISPLAY ALLOWED                              
         B     XIT                                                              
*                                                                               
         MVI   DSPINDS,0           CLEAR DISPLAY INDICATORS                     
*                                                                               
         LA    R2,SAVKEY                                                        
         USING SAASREC,R2          R2=A(ACCESS RECORD KEY)                      
*                                                                               
         CLC   APACTN,SAVAPACT     TEST ACTION HAS CHANGED                      
         BE    VKEY2                                                            
         OI    DSPINDS,DSPIACTC                                                 
         CLI   SAVAPACT,ACTCHA                                                  
         MVC   SAVAPACT,APACTN                                                  
         BNE   *+8                 IF ACTION CHANGED FROM CHANGE                
         OI    RCDINDS,RCDIREAD      RECORD SHOULD BE RE-READ                   
*                                                                               
VKEY2    MVC   WAGY,CUAALF         GET AGENCY EBCIDIC CODE                      
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   WAGY,OPTAGY                                                      
         CLC   WAGY,SAASAGY        TEST CHANGE OF CODE                          
         BNE   VKEY4                                                            
*                                                                               
         TM    DSPSYSH+FHIID,FHIIVA   TEST ALL FIELDS VALIDATED                 
         BZ    VKEY4                                                            
         TM    DSPPGMH+FHIID,FHIIVA                                             
         BZ    VKEY4                                                            
         TM    DSPUIDH+FHIID,FHIIVA                                             
         BZ    VKEY4                                                            
         TM    DSPACGH+FHIID,FHIIVA                                             
         BO    VKEY12                                                           
*                                                                               
VKEY4    XC    DSPPGMD,DSPPGMD     CLEAR & TRANSMIT DESCRIPTIONS                
         XC    DSPUIDD,DSPUIDD                                                  
         XC    DSPACGD,DSPACGD                                                  
         OI    DSPPGMDH+FHOID,FHOITR                                            
         OI    DSPUIDDH+FHOID,FHOITR                                            
         OI    DSPACGDH+FHOID,FHOITR                                            
*                                                                               
         MVC   WORKKEY,SAASKEY     SAVE OLD RECORD KEY                          
         XC    SAASKEY,SAASKEY     BUILD NEW RECORD KEY                         
         MVI   SAASTYP,SAASTYPQ                                                 
         MVI   SAASSUB,SAASSUBQ                                                 
         MVC   SAASAGY,WAGY                                                     
*                                                                               
         GOTO1 AVALOVPG,PARM,DSPSYSH,(1,DSPPGMH),DSPPGMDH                       
         BNE   VALKEYN             VALIDATE SYSTEM/PROGRAM                      
         MVC   SAASOVPG,APHALF                                                  
         MVC   SAVOVPG,APHALF                                                   
         MVC   RSAPGMEL,APELEM     COPY PROGRAM ELEMENT                         
*                                                                               
         CLI   DSPUIDH+FHILD,0     TEST USER-ID ENTERED                         
         BNE   VKEY6                                                            
         LA    R0,DSPUIDH                                                       
         ST    R0,FVADDR                                                        
         GOTO1 ATSTUID,SAASUID     TEST 'ALL' USER-ID VALID                     
         BNE   VALKEYN                                                          
         CLI   DSPACGH+FHILD,0     TEST GROUP NOT ENTERED EITHER                
         BNE   VKEY8                                                            
         TM    CUSTAT,CUSDDS       DDS ONLY MAY SEE                             
         BO    VKEY8                                                            
         MVC   FVMSGNO,=AL2(CE#UGREQ)                                           
         B     VALKEYN                                                          
*                                  USER-ID NOT ALLOWED FOR TEMPO/PRESTO         
VKEY6    CLI   SAVOVPG,X'06'       ACCOUNTING                                   
         BNE   VKEY7                                                            
         CLI   SAVOVPG+1,X'FD'     TEMPO=FD, PRESTO=FE                          
         BL    VKEY7                                                            
         MVC   FVMSGNO,=AL2(CE#UNVPC)                                           
         B     VALKEYN                                                          
*                                                                               
VKEY7    GOTO1 AVALUID,DSPUIDH                                                  
         BNE   VALKEYN                                                          
         MVC   SAASUID,APHALF                                                   
         MVC   DSPUIDD,APWORK                                                   
*                                                                               
VKEY8    CLI   DSPACGH+FHILD,0     TEST ACCESS GROUP ENTERED                    
         BE    VKEY10                                                           
         GOTO1 AVALACG,DSPACGH                                                  
         BNE   VALKEYN                                                          
         MVC   SAASAGN,APHALF                                                   
         MVC   DSPACGD,APWORK                                                   
*                                                                               
VKEY10   CLC   SAASKEY,WORKKEY     TEST CHANGE OF KEY                           
         BE    *+8                                                              
         OI    RCDINDS,RCDIREAD                                                 
*                                                                               
VKEY12   CLI   APACTN,ACTDIS       IF ACTION IS DISPLAY                         
         BE    VKEY16                NO RESTRICTIONS                            
*                                                                               
         OC    SAASAGN,SAASAGN     IF NO ACCESS GROUP                           
         BNZ   VKEY14                MUST BE DDS                                
         TM    CUSTAT,CUSDDS                                                    
         BO    VKEY16                                                           
         LA    RE,DSPUIDH                                                       
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(CE#CCIDL)                                           
         B     VALKEYN                                                          
VKEY14   GOTO1 ATSTGMAN,FVIFLD     TEST FOR GROUP MANAGER                       
         BNE   VALKEYN                                                          
*                                                                               
VKEY16   TM    RCDINDS,RCDIREAD    TEST RECORD TO BE READ                       
         BZ    VALKEYY                                                          
*                                                                               
         BAS   RE,INITRTAB         INITIALIZE RECORD TABLE                      
         BE    VKEY22                                                           
         LA    RE,DSPSYSH                                                       
         ST    RE,FVADDR                                                        
         B     VALKEYN                                                          
                                                                                
***********************************************************************         
*  - READ/SET-UP THE ACCESS RECORD                                    *         
***********************************************************************         
VKEY22   MVC   IOKEY,SAASKEY       READ ACCESS RECORD                           
         L     R2,AIOAREA1                                                      
         GOTO1 AIO,IORDD+IOCONFIL+IO1                                           
         BL    VALKEYN                                                          
         BE    VKEY26                                                           
*                                                                               
         MVI   SAVAPIND,APIOKDIS+APIOKRES                                       
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BO    VKEY28                                                           
*                                                                               
         MVI   SAVAPIND,APIOKADD   RECORD CAN BE ADDED                          
         CLI   APACTN,ACTADD       TEST NOT ADDING                              
         BE    VKEY24                                                           
         XR    RE,RE                                                            
         CLI   DSPUIDH+FHILD,0     TEST USER-ID ENTERED                         
         BE    *+8                                                              
         LA    RE,DSPUIDH                                                       
         CLI   DSPACGH+FHILD,0     TEST ACCESS GROUP ENTERED                    
         BE    *+8                                                              
         LA    RE,DSPACGH                                                       
         ST    RE,APCURSOR         FORCE CURSOR TO BE AT CORRECT FIELD          
         OI    RCDINDS,RCDIREAD                                                 
VKEY24   TM    RCDINDS,RCDISIBL    IF SAME PARENT                               
         BO    VKEY40                CURRENT DISPLAY IS OKAY                    
         B     VKEY30                                                           
*                                                                               
VKEY26   MVI   SAVAPIND,APIOKDIS+APIOKCHA+APIOKDEL                              
         TM    RCDINDS,RCDIACGP    IF ACCESS GROUP DEFINED                      
         BO    VKEY28                RECORD IS NOT A PARENT                     
*                                                                               
         GOTO1 AIO,IOSQ+IOCONFIL+IO2   TEST NEXT RECORD ON FILE IS              
         L     RF,AIOAREA2               CHILD OF ACCESS RECORD                 
         TM    RCDINDS,RCDIAGY                                                  
         BZ    *+14                                                             
         CLC   SAASKEY(SAASUID-SAASKEY),0(RF)                                   
         B     *+10                                                             
         CLC   SAASKEY(SAASAGN-SAASKEY),0(RF)                                   
         BNE   VKEY28                                                           
         OI    RCDINDS,RCDIPRNT    RECORD IS A PARENT                           
         NI    SAVAPIND,FF-APIOKDEL  AND SO CANNOT BE DELETED                   
*                                                                               
VKEY28   GOTOR SAVEACT,SAASREC     SAVE ACTIVITY DATE DETAILS                   
         GOTO1 SAASRTAB,SAASREC    PUT ACCESS RECORD INTO RECORD TABLE          
                                                                                
***********************************************************************         
* - INITIALIZE DISPLAY OF RECORD                                      *         
***********************************************************************         
VKEY30   BAS   RE,DISRCDS          DISPLAY RECORD TYPE TABLE                    
         MVCDD SUBXTRA,CT#SAVIR                                                 
         BAS   RE,SUBMSG                                                        
         OI    DSPINDS,DSPIKEEP    IGNORE INPUT FOR 1ST DISPLAY                 
*                                                                               
VKEY40   TM    DSPINDS,DSPINONE    CAN'T ADD IF NO RECORDS TO LIST              
         BZ    VKEY42                                                           
         CLI   APACTN,ACTADD                                                    
         BNE   VKEY42                                                           
         MVC   FVMSGNO,=AL2(CE#NATLS)                                           
         LA    RE,DSPSYSH                                                       
         ST    RE,APCURSOR                                                      
         B     VALKEYN                                                          
VKEY42   OI    DSPSYSH+FHIID,FHIIVA  SET ALL FIELDS VALIDATED                   
         OI    DSPPGMH+FHIID,FHIIVA                                             
         OI    DSPUIDH+FHIID,FHIIVA                                             
         OI    DSPACGH+FHIID,FHIIVA                                             
*                                                                               
VALKEYY  MVC   APINDS,SAVAPIND                                                  
         MVC   APRECKEY(L'SAVKEY),SAVKEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         B     XIT                                                              
*                                                                               
VALKEYN  XC    SAVKEY,SAVKEY       CLEAR SAVED KEY VALUES                       
         XC    PRNTRKEY,PRNTRKEY                                                
         NI    DSPSYSH+FHIID,FF-FHIIVA   ENSURE A KEY FIELD                     
         B     XIT                         IS UNVALIDATED                       
*                                                                               
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE ACCESS RECORD                                   *         
***********************************************************************         
VALREC   BAS   RE,SUBSINP          VALIDATE INPUT                               
         BE    VREC2                                                            
*                                                                               
         MVI   FVOMTYP,GTMINF      DISPLAY 'ENTER FIELDS' FOR AN ADD            
         MVC   FVMSGNO,=AL2(GI#ENTFD)                                           
         CLI   APACTN,ACTADD                                                    
         BE    VALRECX                                                          
*                                                                               
         MVC   FVXTRA,SAVDATE      OR 'ENTER CHANGES' FOR A CHANGE              
         MVC   FVMSGNO,=AL2(GI#ENTCH)                                           
         B     VALRECX                                                          
*                                                                               
VREC2    L     R2,AIOAREA1                                                      
         USING SAASREC,R2                                                       
         GOTO1 RTABSAAS,SAASREC    CONVERT RECORD TABLE TO ACCESS REC           
         GOTO1 UPDTACT,PARM,AIOAREA2,SAASREC                                    
         BNE   VALRECX                                                          
*                                                                               
         LA    R1,IOPUT+IOCONFIL+IO1                                            
         CLI   APACTN,ACTADD       TEST ACTION ADD                              
         BNE   *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SAVAPIND,APIOKDIS+APIOKCHA+APIOKDEL                              
*                                                                               
VALRECY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALRECX  B     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY ACCESS RECORD                                    *         
***********************************************************************         
DISREC   CLI   APACTN,ACTGRD       ACTION GRIDS?                                
         BNE   *+12                                                             
         BAS   RE,DISGRID                                                       
         B     XIT                                                              
*                                                                               
         BAS   RE,SUBSINP          VALIDATE INPUT                               
         BE    DREC2                                                            
         OI    TWALSCTL,TWALSHLD+TWALSRTN  HOLD ON TO CURRENT SCREEN            
         B     DREC4                                                            
*                                                                               
DREC2    TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BO    DISRECX                                                          
*                                                                               
DREC4    MVC   FVXTRA,SAVDATE      DISPLAY ACTIVITY DATE                        
*                                                                               
DISRECX  L     R2,AIOAREA1                                                      
         USING SAASREC,R2                                                       
         CLC   SAASKEY,APRECKEY                                                 
         BE    DISRECX1                                                         
         MVC   IOKEY,APRECKEY      READ ACCESS RECORD                           
         GOTO1 AIO,IORDD+IOCONFIL+IO1                                           
         BL    XIT                                                              
DISRECX1 TM    SAASSTAT,X'80'      DELETE FLAG IN RECORD                        
         BZ    XIT                                                              
         MVC   FVMSGNO,=AL2(CI#RDDEL)                                           
         MVI   FVOMTYP,GTMINF      SAY DELETED IN MESSAGE                       
         MVI   APMODE,APMFMOK                                                   
         B     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO DELETE RECORD                                                      
***********************************************************************         
DELREC   BAS   RE,SUBSINP          VALIDATE OPTIONS INPUT                       
         BE    DELREC2                                                          
*                                                                               
         MVC   FVXTRA,SAVDATE      DISPLAY ACTIVITY DATE                        
*                                                                               
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI#PETDE)  'PRESS ENTER TO DELETE'                  
         B     DELRECX                                                          
*                                                                               
DELREC2  L     R2,AIOAREA1         UPDATE ACTIVITY ELEMENT                      
         USING SAASREC,R2                                                       
         GOTO1 UPDTACT,PARM,SAASREC,SAASREC                                     
         BNE   DELRECX                                                          
*                                                                               
         OI    SAASSTAT,X'80'      CHANGE DELETE FLAG IN RECORD                 
*                                                                               
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         JNE   *+2                                                              
         MVI   SAVAPIND,APIOKRES+APIOKDIS  RESET APINDS                         
*                                                                               
DELRECX  B     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO RESTORE RECORD                                                     
***********************************************************************         
RESREC   BAS   RE,SUBSINP          VALIDATE OPTIONS INPUT                       
         BE    RESREC2                                                          
*                                                                               
         MVC   FVXTRA,SAVDATE      DISPLAY ACTIVITY DATE                        
*                                                                               
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI#PETRE) 'PRESS ENTER TO RESTORE'                  
         B     RESRECX                                                          
*                                                                               
RESREC2  L     R2,AIOAREA1         UPDATE ACTIVITY ELEMENT                      
         USING SAASREC,R2                                                       
         GOTO1 UPDTACT,PARM,SAASREC,SAASREC                                     
         BNE   RESRECX                                                          
*                                                                               
         NI    SAASSTAT,X'FF'-X'80' CHANGE DELETE FLAG IN RECORD                
*                                                                               
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         JNE   *+2                                                              
         MVI   SAVAPIND,APIOKDIS+APIOKDEL+APIOKCHA                              
*                                                                               
RESRECX  B     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO UPDATE ACTIVITY ELEMENT                                  *         
*                                                                     *         
* NTRY: P1=A(IO AREA TO RE-READ RECORD)                               *         
*       P2=A(IO AREA OF RECORD TO BE UPDATED)                         *         
* EXIT: CC=NOT EQUAL IF ACCESS RECORD HAS CHANGED SINCE LAST READ     *         
***********************************************************************         
UPDTACT  NTR1  ,                                                                
*                                                                               
         LM    R2,R3,0(R1)                                                      
         USING SAASREC,R2                                                       
         LA    R8,ACTEL                                                         
         USING SAACVD,R8           R8=A(ACTIVITY ELEMENT)                       
*                                                                               
UACT2    MVC   IOKEY,SAVKEY        RE-READ CURRENT RECORD                       
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORDUPD+IOCONFIL                                             
         BE    UACT4                                                            
         TM    IOERR,IOEDEL                                                     
         BO    UACT4                                                            
         XC    SAACVSEQ,SAACVSEQ   CLEAR SEQUENCE NUMBER                        
         CLI   APACTN,ACTADD       IF RECORD NOT ON FILE                        
         BE    UACT10                ACTION MUST BE ADD                         
         DC    H'0'                                                             
*                                                                               
UACT4    CLI   APACTN,ACTADD       TEST ACTION IS ADD                           
         BNE   UACT6                                                            
         MVC   FVMSGNO,=AL2(CE#RDADE)  'RECORD ADDED ELSEWHERE'                 
         B     UPDTACTN                                                         
*                                                                               
UACT6    GOTO1 AGETACT,SAASREC     TEST SEQUENCE NUMBER HAS NOT CHANGED         
         CLC   SAVSEQNO,SAACVSEQ                                                
         BE    UACT10                                                           
         MVC   FVMSGNO,=AL2(CE#RDCHE)  'RECORD CHANGED ELSEWHERE'               
         B     UPDTACTN                                                         
*                                                                               
UACT10   GOTO1 ASETACT,(R3)        SET NEW ACTIVITY ELEMENT                     
         GOTOR SAVEACT,(R1)        SAVE NEW ACTIVITY ELEMENT DETAILS            
*                                                                               
UPDTACTY CR    RB,RB               EXIT WITH CC=EQUAL                           
         B     XIT                                                              
UPDTACTN OI    RCDINDS,RCDIREAD    EXIT WITH CC=NOT EQUAL                       
         B     XIT                   & FORCE RE-READING OF RECORD               
         DROP  R2,R8                                                            
                                                                                
***********************************************************************         
* ROUTINE TO VERIFY INPUT FOR SUB-SCREEN                                        
*  EXIT: CC=EQUAL IF DISPLAY IS 'FINISHED'                                      
***********************************************************************         
SUBSINP  NTR1  ,                                                                
*                                                                               
         TM    DSPINDS,DSPINONE    TEST ANYTHING TO DISPLAY                     
         BZ    SINP02                                                           
         OI    DSPINDS,DSPIDONE                                                 
         LA    RE,ACSACTH                                                       
         ST    RE,APCURSOR                                                      
         B     SUBSINPY                                                         
*                                                                               
SINP02   TM    DSPINDS,DSPIKEEP    TEST KEEP CURRENT DISPLAY                    
         BO    SINP20                                                           
*                                                                               
         CLI   APMODE,APMVALR                                                   
         BE    *+8                                                              
         OI    DSPINDS,DSPIDSP     DISPLAY JUST FOR DISPLAY                     
*                                                                               
         CLI   SUBSCRN,SUBSRCDS    TEST RECORD SUB-SCREEN                       
         BNE   SINP04                                                           
         BAS   RE,RCDSINP                                                       
         B     SINP10                                                           
*                                                                               
SINP04   CLI   SUBSCRN,SUBSACTS    TEST ACTION SUB-SCREEN                       
         BNE   SINP06                                                           
         BAS   RE,ACTSINP                                                       
         B     SINP10                                                           
*                                                                               
SINP06   DC    H'0'                                                             
*                                                                               
SINP10   BAS   RE,SUBMSG           DISPLAY SUB-SCREEN MESSAGE                   
*                                                                               
         TM    DSPINDS,DSPIDONE    TEST DISPLAY FINISHED                        
         BZ    SINP20                                                           
         XC    APCURSOR,APCURSOR   RESET CURSOR                                 
*                                                                               
SUBSINPY CR    RB,RB               SET CC=EQUAL                                 
         B     XIT                                                              
*                                                                               
SINP20   OC    APCURSOR,APCURSOR   TEST CURSOR SET                              
         BNZ   SUBSINPN                                                         
         LA    R0,DSAINPH          SET CURSOR                                   
         CLI   SUBSCRN,SUBSRCDS      TO FIRST RECORD/ACTION                     
         BNE   *+8                     INPUT FIELD                              
         LA    R0,DSRINPH                                                       
         ST    R0,APCURSOR                                                      
*                                                                               
SUBSINPN LTR   RB,RB               SET CC=NOT EQUAL                             
         B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO VERIFY INPUT FOR RECORD SUB-SCREEN                       *         
***********************************************************************         
RCDSINP  NTR1  ,                                                                
*                                                                               
         CLI   APPFKEY,10          ONLY 10 PFKEYS ARE DEFINED                   
         BH    PFRUNDF             CHANGE THIS IF MORE ARE ADDED                
         LLC   RF,APPFKEY                                                       
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     PFRENTR             ENTER KEY                                    
         B     PFRHELP             PFK01 = HELP                                 
         B     PFRHELPX            PFK02 = HELP +                               
         B     PFRSEL              PFK03 = SELECT                               
         B     PFRSALL             PFK04 = SELECT +                             
         B     PFRUNDF             PFK05 = UNDEFINED                            
         B     PFRUNDF             PFK06 = UNDEFINED                            
         B     PFRUNDF             PFK07 = UNDEFINED                            
         B     PFRUNDF             PFK08 = UNDEFINED                            
         B     PFRBACK             PFK09 = SCROLL BACK                          
         B     PFRFWRD             PFK10 = SCROLL FORWARD                       
*                                                                               
RCDSSEL  XC    RTABNTRY,RTABNTRY   TEST ANY RECORDS SELECTED                    
         BAS   RE,SELRCD                                                        
         BNE   RSEL2                                                            
         BAS   RE,INITATAB         INITIALIZE AND DISPLAY ACTION TABLE          
         BAS   RE,DISACTS                                                       
         OC    SUBMSGNO,SUBMSGNO                                                
         BNZ   *+10                                                             
         MVCDD SUBXTRA,CT#SAVIA                                                 
         B     RCDSINPX                                                         
*                                                                               
RSEL2    TM    DSPINDS,DSPIINP     TEST ANY INPUT                               
         BO    RCDSINPX                                                         
         CLI   APACTN,ACTCHA       UNLESS ACTION IS CHANGE                      
         BE    *+8                   DISPLAY HAS FINISHED                       
         OI    DSPINDS,DSPIDONE                                                 
*                                                                               
         BRAS  RE,CLRRTAB          CLEAR RECORD TABLE OF SELECTS                
         BAS   RE,DISRCDS                                                       
*                                                                               
RCDSINPX B     XIT                                                              
                                                                                
PFRENTR  BAS   RE,VALRCDS          * ENTER KEY *                                
         BNE   RCDSINPX                                                         
         B     RCDSSEL                                                          
*                                                                               
PFRUNDF  XR    R0,R0               * UNDEFINED PF KEY *                         
         IC    R0,APPFKEY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SUBXTRA(2),DUB                                                   
         OI    SUBXINDS,SUBXIMID                                                
         MVC   SUBMSGNO,=AL2(CE#PFUND)                                          
         B     RCDSINPX                                                         
*                                                                               
PFRHELP  LA    R1,INPHELP          * HELP *                                     
         B     PFRCURS                                                          
*                                                                               
PFRHELPX LA    R1,INPHELPX         * HELP+ *                                    
         B     PFRCURS                                                          
*                                                                               
PFRSEL   LA    R1,INPSEL           * SELECT *                                   
         B     PFRCURS                                                          
*                                                                               
PFRSALL  LA    R1,INPSELX          * SELECT+ *                                  
*                                                                               
PFRCURS  BAS   RE,SETCURS                                                       
         BNE   RCDSINPX                                                         
         BAS   RE,VALRCDS                                                       
         BNE   RCDSINPX                                                         
         B     RCDSSEL                                                          
                                                                                
PFRBACK  BAS   RE,VALRCDS          * SCROLL BACK *                              
         BNE   RCDSINPX                                                         
*                                                                               
         XR    RE,RE                                                            
         IC    RE,RPAGENO                                                       
         BCT   RE,*+14             TEST CAN DO SO                               
         MVCDD SUBXTRA,CT#CTSBK                                                 
         B     RCDSINPX                                                         
*                                                                               
         STC   RE,RPAGENO                                                       
         MVCDD SUBXTRA,CT#SCDBK                                                 
         BAS   RE,DISRCDS                                                       
         XC    APCURSOR,APCURSOR                                                
         B     RCDSINPX                                                         
*                                                                               
PFRFWRD  BAS   RE,VALRCDS          * SCROLL FORWARD *                           
         BNE   RCDSINPX                                                         
*                                                                               
         IC    RE,RPAGENO                                                       
         CLM   RE,1,RNOPAGES       TEST CAN DO SO                               
         BNE   *+14                                                             
         MVCDD SUBXTRA,CT#CTSFD                                                 
         B     RCDSINPX                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         STC   RE,RPAGENO                                                       
         MVCDD SUBXTRA,CT#SCDFD                                                 
         BAS   RE,DISRCDS                                                       
         XC    APCURSOR,APCURSOR                                                
         B     RCDSINPX                                                         
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE RECORD INPUT FIELDS                             *         
*                                                                     *         
* EXIT: CC=EQUAL IF ALL FIELDS HAVE BEEN VALIDATED                    *         
***********************************************************************         
VALRCDS  NTR1  ,                                                                
         XR    R8,R8                                                            
         IC    R8,RPAGENO                                                       
         BCTR  R8,0                                                             
         MH    R8,=Y(RTABPAGE)                                                  
         A     R8,ARTAB                                                         
         USING RTABD,R8            R8=A(RECORD TABLE ENTRY)                     
*                                                                               
         LA    R2,RCDINPLQ         R1=L(RECORD/INPUT FIELDS)                    
         LA    R3,DSRINPLH         R3=A(LAST INPUT FIELD)                       
         LA    R9,DSRINPH                                                       
         USING FHD,R9              R9=A(FIRST INPUT FIELD)                      
*                                                                               
VRCDS2   TM    FHII,FHIIVA         TEST PREVIOUSLY VALIDATED                    
         BO    VRCDS8                                                           
         OI    DSPINDS,DSPIINP                                                  
         ST    R9,APCURSOR                                                      
*                                                                               
         CLC   FHDA(1),INPSELD                                                  
         BE    VRCDS6                                                           
         CLI   FHDA,C' '                                                        
         BNH   VRCDUNSE                                                         
         CLC   FHDA(1),INPSEL                                                   
         BE    VRCDSEL                                                          
         CLC   FHDA(1),INPHELP                                                  
         BE    VRCDHELP                                                         
         B     VRCDUNDF                                                         
*                                                                               
VRCDS6   GOTOR DISRINP,PARM,FHD,RTABD                                           
*                                                                               
VRCDS8   LA    R8,RTABL(R8)        BUMP R8 TO NEXT TABLE ENTRY                  
         CLI   RTABD,RTEOTQ                                                     
         BE    *+8                                                              
         BXLE  R9,R2,VRCDS2        BUMP R9 TO NEXT INPUT                        
*                                                                               
         ICM   R0,3,SUBSTAB        TEST TAB TO NEXT PAGE                        
         BZ    VALRCDSY                                                         
*                                                                               
         IC    RE,RPAGENO          SCROLL FORWARD                               
         LA    RE,1(RE)                                                         
         STC   RE,RPAGENO                                                       
         BAS   RE,DISRCDS                                                       
*                                                                               
         STCM  R0,3,DSRINP         SET FIRST INPUT OF NEW SCREEN TO TAB         
         NI    DSRINPH+FHIID,FF-FHIIVA                                          
         BAS   RE,VALRCDS          VALIDATE NEW SCREEN                          
*                                                                               
VALRCDSN LTR   RB,RB                                                            
         B     XIT                                                              
VALRCDSY CR    RB,RB               CC=EQUAL FOR ALL FIELDS VALIDATED            
         B     XIT                                                              
         SPACE 5                                                                
VRCDUNDF MVC   SUBMSGNO,=AL2(FVFNOTV)  * INVALID INPUT *                        
         MVCDD SUBXTRA,CT#SAVIR                                                 
         B     VALRCDSN                                                         
*                                                                               
VRCDSEL  MVI   RTSEL,RTSIS         * SELECT *                                   
         CLC   INPSELX,FHDA                                                     
         BNE   *+8                                                              
         MVI   RTSEL,RTSALL        * SELECT+ *                                  
         B     VRCDS6                                                           
*                                                                               
VRCDUNSE MVI   RTSEL,RTSNOT        * UNSELECT *                                 
         B     VRCDS6                                                           
*                                                                               
VRCDHELP GOTO1 DISHELP,PARM,RTWRD,RTDSC  * HELP *                               
*                                                                               
         CLC   INPHELPX,FHDA       * HELP+ *                                    
         BNE   VRCDH4                                                           
         CLI   RTABD+RTABL,RTEOTQ  TEST LAST RECORD                             
         BE    VRCDH4                                                           
         CR    R9,R3               TEST LAST FOR SCREEN                         
         BNE   VRCDH2                                                           
         MVC   SUBSTAB,INPHELPX    SET TAB TO NEXT SCREEN                       
         B     VRCDH4                                                           
*                                                                               
VRCDH2   MVC   FHDA+RCDINPLQ(L'INPHELPX),INPHELPX  BUMP 'HELP+' TO              
         NI    FHII+RCDINPLQ,FF-FHIIVA               NEXT INPUT                 
         OI    FHOI+RCDINPLQ,FHOITR                                             
*                                                                               
VRCDH4   GOTOR DISRINP,PARM,FHD,RTABD                                           
         B     VALRCDSN                                                         
*                                                                               
         DROP  R8,R9                                                            
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY RECORD TYPES                                     *         
***********************************************************************         
DISRCDS  NTR1  ,                                                                
         GOTO1 SETSUBS,SUBSRCDS                                                 
*                                                                               
         MVC   DSRPAGE(1),RPAGENO  DISPLAY PAGE NUMBER INFO.                    
         OI    DSRPAGE,C'0'                                                     
         MVC   DSRPAGE+2(1),RNOPAGES                                            
         OI    DSRPAGE+2,C'0'                                                   
         OI    DSRPAGEH+FHOID,FHOITR                                            
*                                                                               
         XR    R8,R8                                                            
         IC    R8,RPAGENO                                                       
         BCTR  R8,0                                                             
         MH    R8,=Y(RTABPAGE)                                                  
         A     R8,ARTAB                                                         
         USING RTABD,R8            R8=A(RECORD TYPE TABLE ENTRY)                
*                                                                               
         LA    R2,RCDINPLQ         R2=L(RECORD/INPUT FIELDS)                    
         LA    R3,DSRWRDLH         R3=A(LAST WORD ON SCREEN)                    
         LA    R9,DSRWRDH                                                       
         USING DSRWRDH,R9          R9=A(FIRST WORD ON SCREEN)                   
*                                                                               
DRCDS2   CLI   RTABD,RTEOTQ        TEST E-O-T                                   
         BE    DISRCDSX                                                         
         NI    DSRINPH+FHATD,FF-FHATPR                                          
         GOTOR DISRINP,PARM,DSRINPH,RTABD                                       
         GOTO1 ADISDIC,PARM,(SAVOVS,L'DSRWRD),(C'L',RTWRD)                      
         MVC   DSRWRD,APWORK                                                    
         GOTOR MVCEX,PARM,DVALID,RTDVALID                                       
         CLI   RTATT,0             TEST RECORD ATTACHED                         
         BE    DRCDS4                                                           
         GOTOR MVCEX,PARM,DAVAIL,RTDAVAIL                                       
         GOTO1 FINDRTAB,RTATT                                                   
         GOTOR NCEX,PARM,DAVAIL,RTDVALID-RTABD(RF)                              
         GOTOR NCEX,(R1),DVALID,DAVAIL  ADJUST WHAT IS VALID                    
         GOTOR XCEX,(R1)                                                        
DRCDS4   GOTOR OCEX,PARM,DVALID,DVALID                                          
         BZ    *+8                 HIGHLIGHT WORD IF ACTIONS VALID              
         OI    DSRWRDH+FHATD,FHATHI                                             
*                                                                               
         LA    R8,RTABL(R8)        BUMP R8 TO NEXT TABLE ENTRY                  
         BXLE  R9,R2,DRCDS2        BUMP TO NEXT WORD/INPUT                      
         DROP  R8,R9                                                            
*                                                                               
DISRCDSX B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO VERIFY INPUT FOR ACTIONS SUB-SCREEN                      *         
***********************************************************************         
ACTSINP  NTR1  ,                                                                
*                                                                               
         CLI   APPFKEY,12          ONLY 12 PFKEYS ARE DEFINED                   
         BH    PFRUNDF             CHANGE THIS IF MORE ARE ADDED                
         LLC   RF,APPFKEY                                                       
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     PFAENTER            ENTER KEY                                    
         B     PFAHELP             PFK01 = HELP                                 
         B     PFAHELPX            PFK02 = HELP+                                
         B     PFAUNDF             PFK03 = UNDEFINED                            
         B     PFAUNDF             PFK04 = UNDEFINED                            
         B     PFAYES              PFK05 = YES                                  
         B     PFAYESX             PFK06 = YES+                                 
         B     PFANO               PFK07 = NO                                   
         B     PFANOX              PFK08 = NO+                                  
         B     PFABACK             PFK09 = SCROLL BACK                          
         B     PFAFRWD             PFK10 = SCROLL FORWARD                       
         B     PFAUNDF             PFK11 = UNDEFINED                            
         B     PFARETN             PFK12 = RETURN                               
*                                                                               
ACTSINPX B     XIT                                                              
*                                                                               
PFAENTER BAS   RE,VALACTS          * ENTER KEY *                                
         BNE   ACTSINPX                                                         
         TM    DSPINDS,DSPIINP     TEST ANY ACTION INPUT                        
         BO    ACTSINPX                                                         
         TM    DSPINDS,DSPIACTC    IF ACTION CHANGED                            
         BO    ACTSINPX              KEEP CURRENT DISPLAY                       
*                                                                               
         BAS   RE,RCDSELD                                                       
         BAS   RE,SELRCD           TEST ANY MORE SELECTIONS                     
         BE    PFAENTE2                                                         
*                                                                               
         BAS   RE,DISRCDS          RE-DISPLAY RECORD LIST                       
         OI    DSPINDS,DSPIDONE                                                 
         B     ACTSINPX                                                         
*                                                                               
PFAENTE2 BAS   RE,INITATAB         DISPLAY NEXT ACTION LIST                     
         BAS   RE,DISACTS                                                       
         OC    SUBMSGNO,SUBMSGNO                                                
         BNZ   *+10                                                             
         MVCDD SUBXTRA,CT#SAVIA                                                 
         B     ACTSINPX                                                         
                                                                                
PFAUNDF  XR    R0,R0               * UNDEFINED PF KEY *                         
         IC    R0,APPFKEY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SUBXTRA(2),DUB                                                   
         OI    SUBXINDS,SUBXIMID                                                
         MVC   SUBMSGNO,=AL2(CE#PFUND)                                          
         B     ACTSINPX                                                         
*                                                                               
PFARETN  L     RF,ARTAB            * RETURN *                                   
         AH    RF,RTABNTRY                                                      
         MVI   RTSEL-RTABD(RF),RTSNOT                                           
         BAS   RE,DISRCDS                                                       
         B     ACTSINPX                                                         
*                                                                               
PFAHELP  LA    R1,INPHELP          * HELP *                                     
         B     PFACURS                                                          
*                                                                               
PFAHELPX LA    R1,INPHELPX         * HELP+ *                                    
         B     PFACURS                                                          
*                                                                               
PFAYESX  LA    R1,INPYESX          * YES+ *                                     
         B     PFACURS                                                          
*                                                                               
PFANOX   LA    R1,INPNOX           * NO+ *                                      
*                                                                               
PFACURS  BAS   RE,SETCURS                                                       
         BNE   ACTSINPX                                                         
         BAS   RE,VALACTS                                                       
         B     ACTSINPX                                                         
*                                                                               
PFAYES   LA    R1,INPYES           * YES *                                      
         B     *+8                                                              
PFANO    LA    R1,INPNO            * NO *                                       
         BAS   RE,SETCURS                                                       
         BNE   ACTSINPX                                                         
*                                                                               
         L     RF,APCURSOR         TEST CURSOR ON LAST INPUT FIELD              
         LA    R0,DSAINPLH                                                      
         CR    RF,R0                                                            
         BE    PFAFRWD             SCROLL FORWARD IF NECCESERY                  
*                                                                               
         BAS   RE,VALACTS                                                       
         BNE   ACTSINPX                                                         
         LA    RF,ACTINPLQ(RF)     TAB CURSOR TO NEXT INPUT FIELD               
         TM    FHATD(RF),FHATPR      (UNLESS LAST ONE)                          
         BO    *+8                                                              
         ST    RF,APCURSOR                                                      
         B     ACTSINPX                                                         
                                                                                
PFABACK  BAS   RE,VALACTS          * SCROLL BACK *                              
         BNE   ACTSINPX                                                         
*                                                                               
         XR    RE,RE                                                            
         IC    RE,APAGENO          TEST CAN DO                                  
         BCT   RE,*+14                                                          
         MVCDD SUBXTRA,CT#CTSBK                                                 
         B     ACTSINPX                                                         
*                                                                               
         STC   RE,APAGENO                                                       
         MVCDD SUBXTRA,CT#SCDBK                                                 
         BAS   RE,DISACTS                                                       
         XC    APCURSOR,APCURSOR                                                
         B     ACTSINPX                                                         
*                                                                               
PFAFRWD  BAS   RE,VALACTS          * SCROLL FORWARD *                           
         BNE   ACTSINPX                                                         
*                                                                               
         IC    RE,APAGENO                                                       
         CLM   RE,1,ANOPAGES       TEST CAN DO                                  
         BL    *+14                                                             
         MVCDD SUBXTRA,CT#CTSFD                                                 
         B     ACTSINPX                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         STC   RE,APAGENO                                                       
         MVCDD SUBXTRA,CT#SCDFD                                                 
         BAS   RE,DISACTS                                                       
         XC    APCURSOR,APCURSOR                                                
         B     ACTSINPX                                                         
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE ACTION INPUT FIELDS                             *         
*                                                                     *         
* EXIT: CC=EQUAL IF ALL FIELDS HAVE BEEN VALIDATED                    *         
***********************************************************************         
VALACTS  NTR1  ,                                                                
*                                                                               
         XR    R8,R8                                                            
         IC    R8,APAGENO                                                       
         BCTR  R8,0                                                             
         MH    R8,=Y(ATABPAGE)                                                  
         A     R8,AATAB                                                         
         USING ATABD,R8            R8=A(ACTION TABLE ENTRY)                     
*                                                                               
         LA    R2,ACTINPLQ         R2=L(ACTION/INPUT FIELDS)                    
         LA    R3,DSAINPLH         R3=A(LAST INPUT FIELD)                       
         LA    R9,DSAINPH                                                       
         USING FHD,R9              R9=A(FIRST INPUT FIELD)                      
*                                                                               
VACTS2   CLI   ATABD,ATEOTQ        TEST E-O-T                                   
         BE    VACTS10                                                          
         TM    FHII,FHIIVA         TEST FIELD PREVIOUSLY VALIDATED              
         BO    VACTS8                                                           
         ST    R9,APCURSOR                                                      
         OI    DSPINDS,DSPIINP     SET ACTION CHANGED                           
*                                                                               
         CLC   FHDA(1),INPYES                                                   
         BE    VACTYES                                                          
         CLC   FHDA(1),INPNO                                                    
         BE    VACTNO                                                           
         CLC   FHDA(1),INPHELP                                                  
         BE    VACTHELP                                                         
         B     VACTUNDF                                                         
*                                                                               
VACTS6   GOTO1 DISAINP,PARM,FHD,ATABD                                           
*                                                                               
VACTS8   LA    R8,ATABL(R8)                                                     
         BXLE  R9,R2,VACTS2                                                     
*                                                                               
VACTS10  ICM   R0,3,SUBSTAB        TEST TAB TO NEXT PAGE                        
         BZ    VALACTSY                                                         
*                                                                               
         IC    RE,APAGENO          SCROLL FORWARD                               
         LA    RE,1(RE)                                                         
         STC   RE,APAGENO                                                       
         BAS   RE,DISACTS                                                       
*                                                                               
         STCM  R0,3,DSAINP         SET FIRST INPUT OF NEW SCREEN TO TAB         
         NI    DSAINPH+FHIID,FF-FHIIVA                                          
         BAS   RE,VALACTS          VALIDATE NEW SCREEN                          
*                                                                               
VALACTSN LTR   RB,RB               CC=NOT EQUAL                                 
         B     XIT                                                              
VALACTSY CR    RB,RB               CC=EQUAL FOR ALL FIELDS VALIDATED            
         B     XIT                                                              
                                                                                
VACTUNDF CLI   FHDA,C' '           * INVALID INPUT *                            
         BH    VAUNDF2             TEST FIELD EMPTY                             
         TM    DSPINDS,DSPIDSP     TEST ACTION IS DISPLAY                       
         BO    VACTS6                                                           
         MVC   SUBMSGNO,=AL2(FVFNONE)                                           
         B     *+10                                                             
VAUNDF2  MVC   SUBMSGNO,=AL2(FVFNOTV)                                           
         MVCDD SUBXTRA,CT#SAVIA                                                 
         B     VALACTSN                                                         
*                                                                               
VACTYES  TM    DSPINDS,DSPIDSP     * YES *                                      
         BO    VACTS6                                                           
         CLC   INPYESX,FHDA                                                     
         BE    VACTYESX                                                         
         GOTO1 SETBIT,PARM,(ATSEQ,VALID)                                        
         B     VACTS6                                                           
*                                                                               
VACTYESX GOTO1 SETBIT,PARM,(ATSEQ,VALID)   * YES+ *                             
         LA    R8,ATABL(R8)                                                     
         CLI   ATABD,ATEOTQ                                                     
         BNE   VACTYESX                                                         
*                                                                               
         BAS   RE,DISACTS                                                       
         B     VALACTSN                                                         
*                                                                               
VACTNO   TM    DSPINDS,DSPIDSP     * NO *                                       
         BO    VACTS6                                                           
         CLC   INPNOX,FHDA                                                      
         BE    VACTNOX                                                          
         GOTO1 RESETBIT,PARM,(ATSEQ,VALID)                                      
         B     VACTS6                                                           
*                                                                               
VACTNOX  GOTO1 RESETBIT,PARM,(ATSEQ,VALID)  * NO+ *                             
         LA    R8,ATABL(R8)                                                     
         CLI   ATABD,ATEOTQ                                                     
         BNE   VACTNOX                                                          
*                                                                               
         BAS   RE,DISACTS                                                       
         B     VALACTSN                                                         
*                                                                               
VACTHELP GOTO1 DISHELP,PARM,ATWRD,ATDSC  * HELP *                               
*                                                                               
         CLC   INPHELPX,FHDA       TEST HELP+                                   
         BNE   VACTH4                                                           
         CLI   ATABD+ATABL,ATEOTQ  TEST LAST ACTION                             
         BE    VACTH4                                                           
         CR    R9,R3               TEST LAST FOR SCREEN                         
         BNE   VACTH2                                                           
         MVC   SUBSTAB,INPHELPX    SET TAB TO NEXT SCREEN                       
         B     VACTH4                                                           
*                                                                               
VACTH2   MVC   FHDA+ACTINPLQ(L'INPHELPX),INPHELPX  BUMP HELP+ TO                
         NI    FHII+ACTINPLQ,FF-FHIIVA               NEXT INPUT                 
         OI    FHOI+ACTINPLQ,FHOITR                                             
*                                                                               
VACTH4   GOTO1 DISAINP,PARM,FHD,ATABD                                           
         B     VALACTSN                                                         
*                                                                               
         DROP  R8,R9                                                            
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY ACTION CODES                                     *         
***********************************************************************         
DISACTS  NTR1  ,                                                                
*                                                                               
         GOTO1 SETSUBS,SUBSACTS                                                 
*                                                                               
         L     R2,ARTAB            DISPLAY RECORD TYPE INFO                     
         AH    R2,RTABNTRY                                                      
         USING RTABD,R2                                                         
         GOTO1 ADISDIC,PARM,(SAVOVS,L'DSARCDW),(C'U',RTWRD)                     
         MVC   DSARCDW,APWORK                                                   
         OI    DSARCDWH+FHOID,FHOITR                                            
         IC    RF,SAVOVS                                                        
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),RTDSC),RTWRD                                  
         MVC   DSARCDD,APWORK                                                   
         OI    DSARCDDH+FHOID,FHOITR                                            
         DROP  R2                                                               
*                                                                               
         MVC   DSAPAGE(1),APAGENO  DISPLAY PAGE NUMBER INFO.                    
         OI    DSAPAGE,C'0'                                                     
         MVC   DSAPAGE+2(1),ANOPAGES                                            
         OI    DSAPAGE+2,C'0'                                                   
         OI    DSAPAGEH+FHOID,FHOITR                                            
*                                                                               
         XR    R8,R8                                                            
         IC    R8,APAGENO                                                       
         BCTR  R8,0                                                             
         MH    R8,=Y(ATABPAGE)                                                  
         A     R8,AATAB                                                         
         USING ATABD,R8            R8=A(ACTION CODE TABLE ENTRY)                
*                                                                               
         LA    R2,ACTINPLQ         R2=L(ACTION/INPUT FIELDS)                    
         LA    R3,DSAWRDLH         R3=A(LAST WORD ON SCREEN)                    
         LA    R9,DSAWRDH                                                       
         USING DSAWRDH,R9          R9=A(FIRST WORD ON SCREEN)                   
*                                                                               
DACTS02  CLI   ATABD,ATEOTQ                                                     
         BE    DISACTSX                                                         
         NI    DSAINPH+FHATD,FF-FHATPR                                          
         GOTO1 ADISDIC,PARM,(SAVOVS,L'DSAWRD),(C'L',ATWRD)                      
         MVC   DSAWRD,APWORK                                                    
         GOTO1 DISAINP,PARM,DSAWRDH,ATABD                                       
*                                                                               
         LA    R8,ATABL(R8)        BUMP R8 TO NEXT TABLE ENTRY                  
         BXLE  R9,R2,DACTS02       BUMP TO NEXT WORD/INPUT                      
         DROP  R8,R9                                                            
*                                                                               
DISACTSX B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY ACTION INPUT                                     *         
*                                                                     *         
* NTRY: P1=A(ACTION WORD/INPUT FIELD)                                 *         
*       P2=A(ACTION TABLE ENTRY)                                      *         
***********************************************************************         
*                                                                               
DISAINP  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
ACT      USING DSAWRDH,R2          R2=A(ACTION FIELD)                           
         USING ATABD,R3            R3=A(ACTION TABLE ENTRY)                     
*                                                                               
         TM    ACT.DSAWRDH+FHATD,FHATPR                                         
         BO    *+8                 ADJUST R2 IF IS THE INPUT FIELD              
         SH    R2,=Y(DSAINPH-DSAWRDH)                                           
*                                                                               
         OI    ACT.DSAWRDH+FHOID,FHOITR  TRANSMIT                               
         OI    ACT.DSAINPH+FHOID,FHOITR                                         
         OI    ACT.DSAINPH+FHIID,FHIIVA  SET INPUT VALIDATED                    
*                                                                               
         GOTO1 TESTBIT,PARM,(ATSEQ,VALID)  TEST ACTION IS VALID                 
         BZ    DAINP2                                                           
         MVC   ACT.DSAINP,INPYES                                                
         OI    ACT.DSAWRDH+FHATD,FHATHI                                         
         OI    ACT.DSAINPH+FHATD,FHATHI                                         
         B     XIT                                                              
*                                                                               
DAINP2   MVC   ACT.DSAINP,INPNO    ACTION IS NOT VALID                          
         NI    ACT.DSAWRDH+FHATD,FF-FHATHI                                      
         NI    ACT.DSAINPH+FHATD,FF-FHATHI                                      
         B     XIT                                                              
*                                                                               
         DROP  ACT,R3                                                           
                                                                                
***********************************************************************         
* ROUTINE TO SET CURSOR INPUT FIELD                                   *         
*                                                                     *         
* NTRY: R1=A(OUTPUT FOR FIELD)                                        *         
* EXIT: CC=EQUAL IF CURSOR VALIDLY WITHIN SUB-SCREEN                  *         
*       APCURSOR=A(INPUT FIELD)                                       *         
***********************************************************************         
*                                                                               
SETCURS  NTR1  ,                                                                
         L     R3,AINP                                                          
         USING TIOBD,R3            TRANSLATOR I/O BLOCK                         
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,3,TIOBCURD                                                    
         LA    R2,TWAD(R2)         R2=A(FIELD OF CURSOR)                        
         USING FHD,R2                                                           
         BNZ   SCURS10                                                          
*                                                                               
         LA    R2,ACSMSGH          CURSOR NOT WITHIN FIELD                      
         XR    RF,RF                                                            
SCURS2   CLC   FHAD,TIOBCURS       FIND NEAREST FIELD TO CURSOR                 
         BH    SCURS4                                                           
         ICM   RF,1,FHLN                                                        
         BZ    *+10                                                             
         BXH   R2,RF,SCURS2                                                     
SCURS4   SR    R2,RF                                                            
         DROP  R3                                                               
*                                                                               
SCURS10  TM    FHAT,FHATPR         TEST FIELD PROTECTED                         
         BZ    SCURS12                                                          
         XR    RF,RF               BUMP R2 TO NEXT FIELD                        
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         TM    FHAT,FHATPR         TEST THIS FIELD PROTECTED                    
         BO    SETCURSN                                                         
*                                                                               
SCURS12  LA    R0,DSPTABH          TEST CURSOR WITHIN SUB-SCREEN                
         CR    R2,R0                                                            
         BL    SETCURSN                                                         
*                                                                               
         OI    FHOI,FHOITR         SET INPUT FIELD                              
         NI    FHII,FF-FHIIVA                                                   
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+1)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FHDA(0),0(R1)                                                    
*                                                                               
SETCURSY ST    R2,APCURSOR         SAVE A(CURSOR INPUT FIELD)                   
         CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
SETCURSN MVC   SUBMSGNO,=AL2(CE#PCWIF)                                          
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY HELP FOR RECORD OR ACTION                        *         
*                                                                     *         
* NTRY: P1=A(DICTIONARY REFERENCE NUMBER)                             *         
*       P2=A(TEXT DESCRIPTION NUMBER)                                 *         
***********************************************************************         
*                                                                               
DISHELP  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         XC    WORK,WORK                                                        
*                                                                               
         GOTO1 ADISDIC,PARM,(SAVOVS,HELPWRDL),(C'U',(R2))                       
         MVC   WORK+1(HELPWRDL),APWORK                                          
         IC    RF,SAVOVS                                                        
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),(R3)),(R2)                                    
         LA    RF,WORK+HELPWRDL    COPY DESCRIPTION TO LEFT OF WORD             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(L'APWORK,RF),APWORK                                            
*                                                                               
         MVC   SUBTEXT,WORK+1                                                   
         B     XIT                                                              
                                                                                
***********************************************************************         
*  ROUTINE TO DISPLAY SUB SCREEN MESSAGE                              *         
***********************************************************************         
*                                                                               
SUBMSG   NTR1  ,                                                                
         LA    R2,DSAMSGH                                                       
         CLI   SUBSCRN,SUBSRCDS                                                 
         BNE   *+8                                                              
         LA    R2,DSRMSGH                                                       
MESS     USING DSAMSGH,R2          R2=A(MESSAGE HEADER)                         
*                                                                               
         OC    SUBTEXT,SUBTEXT     TEST TEXT SET                                
         BZ    SMSG02                                                           
         MVC   MESS.DSAMSG,SUBTEXT                                              
         B     SMSG30                                                           
*                                                                               
SMSG02   LA    R3,GTPARM                                                        
         USING GETTXTD,R3          R3=A(GETTXT PARAMETER LIST)                  
         XC    GTBLOCK,GTBLOCK                                                  
*                                                                               
         OC    SUBMSGNO,SUBMSGNO   TEST MESSAGE NUMBER SET                      
         BNZ   SMSG10                                                           
         TM    DSPINDS,DSPINONE    SET DEFAULT NOTHING TO LIST MESSAGE          
         BZ    *+14                                                             
         MVC   GTMSGNO,=AL2(CE#NATLS)                                           
         B     SMSG20                                                           
*                                                                               
         XC    APWORK,APWORK       USE DEFAULT MESSAGES                         
         MVC   APWORK(L'AC@RECLD),AC@RECLD                                      
         CLI   SUBSCRN,SUBSACTS                                                 
         BNE   *+10                                                             
         MVC   APWORK(L'AC@ACTLD),AC@ACTLD                                      
*                                                                               
         OC    SUBXTRA,SUBXTRA     TEST XTRA FOR DEFAULT                        
         BZ    SMSG04                                                           
         LA    RF,APWORK+L'AC@RECLD-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   2(RF),C'-'                                                       
         MVC   4(L'SUBXTRA,RF),SUBXTRA                                          
SMSG04   MVC   MESS.DSAMSG,APWORK                                               
         B     SMSG30                                                           
*                                                                               
SMSG10   MVC   GTMTYP,SUBMTYP      SET MESSAGE TYPE AND NUMBER                  
         MVC   GTMSGNO,SUBMSGNO                                                 
         CLI   SUBMSGNO,FF         TEST GENERAL SYSTEM MESSAGE                  
         BNE   *+12                                                             
         MVI   GTMSGNO,0                                                        
         MVI   GTMSYS,FF                                                        
*                                                                               
SMSG12   OC    SUBXTRA,SUBXTRA     TEST ANY TEXT TO TAG ON                      
         BZ    SMSG20                                                           
         TM    SUBXINDS,SUBXIMID   TEST TEXT ADDED IN MIDDLE                    
         BZ    SMSG14                                                           
         MVI   GTLTXT,L'SUBXTRA                                                 
         LA    RE,SUBXTRA                                                       
         STCM  RE,7,GTATXT                                                      
         B     SMSG20                                                           
*                                                                               
SMSG14   MVI   WORK,C'-'           TEST IS TO BE APPENDED                       
         MVI   WORK+1,C' '                                                      
         MVC   WORK+2(L'SUBXTRA),SUBXTRA                                        
         MVI   GTLTXT,L'SUBXTRA+2                                               
         LA    RE,WORK                                                          
         STCM  RE,7,GTATXT                                                      
*                                                                               
SMSG20   STCM  R2,7,GTAOUT                                                      
         GOTO1 VGETTXT,GETTXTD                                                  
*                                                                               
SMSG30   OI    MESS.DSAMSGH+FHOID,FHOITR  TRANSMIT MESSAGE                      
*                                                                               
SUBMSGX  B     XIT                                                              
         DROP  R3,MESS                                                          
                                                                                
***********************************************************************         
* ROUTINE TO SET UP SUB-SCREEN                                        *         
*                                                                     *         
* NTRY: R1=SUB-SCREEN NUMBER                                          *         
***********************************************************************         
*                                                                               
SETSUBS  NTR1  ,                                                                
         XC    SUBSTAB,SUBSTAB     CLEAR TAB TO NEXT SCREEN                     
*                                                                               
         CLM   R1,1,SUBSCRN        TEST CHANGE OF SUB-SCREEN                    
         BE    SSUBS2                                                           
         STC   R1,SUBSCRN                                                       
         XC    APCURSOR,APCURSOR                                                
*                                                                               
         MVI   PARM+4,C'R'         SET UP PARAMETER 2 FOR OVERLAY CALL          
         MVC   PARM+5(2),ACSYSPGM                                               
         MVC   PARM+7(1),SUBSCRN                                                
         GOTO1 VCOLY,PARM,DSPTABH,,0                                            
         CLI   4(R1),FF            TEST SCREEN LOADED OKAY                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SUBSCRN,SUBSACTS    TEST ACTION SCREEN                           
         BNE   *+10                YES - OUTPUT RECORD TYPE WORD                
         MVC   DSARECT,AC@RECTY                                                 
*                                                                               
         LA    R1,ACSMSGH          TRANSMIT SCREEN                              
         XR    RF,RF                                                            
         USING FHD,R1                                                           
         OI    FHOI,FHOITR                                                      
         ICM   RF,1,FHLN                                                        
         BZ    SETSUBSX                                                         
         BXH   R1,RF,*-12                                                       
         DROP  R1                                                               
*                                                                               
SSUBS2   LA    RF,DSRWRDH          RF=FIRST FIELD                               
         LA    R1,DSRINPLH         R1=LAST FIELD                                
         CLI   SUBSCRN,SUBSACTS    TEST ACTIONS SUB-SCREEN                      
         BNE   *+12                                                             
         LA    RF,DSAWRDH                                                       
         LA    R1,DSAINPLH                                                      
         USING FHD,RF                                                           
         XR    R0,R0                                                            
*                                                                               
SSUBS4   OI    FHAT,FHATPR         PROTECT                                      
         NI    FHAT,FF-FHATHI      UNHIGHLIGHT                                  
         OI    FHOI,FHOITR         TRANSMIT                                     
         IC    R0,FHLN                                                          
         LR    RE,R0                                                            
         SH    RE,=Y(FHDAD+1)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    FHDA(0),FHDA        CLEAR FIELD                                  
         BXLE  RF,R0,SSUBS4                                                     
         DROP  RF                                                               
*                                                                               
SETSUBSX B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
*                                                                               
VALSEL   LA    R2,APRECKEY         INITIALIZE RECORD KEY                        
         USING SAASREC,R2                                                       
         XC    SAASKEY,SAASKEY                                                  
         MVI   SAASTYP,SAASTYPQ                                                 
         MVI   SAASSUB,SAASSUBQ                                                 
         MVC   SAASAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGY=                             
         BZ    *+10                                                             
         MVC   SAASAGY,OPTAGY                                                   
*                                                                               
VSEL2    GOTO1 AVALOVPG,PARM,(X'80',LSTSYSH),LSTPGMH,0                          
         BNE   VALSELX                                                          
         MVC   SAASOVPG,APHALF                                                  
*                                                                               
VSEL4    CLI   LSTUIDH+FHILD,0     TEST USER-ID ENTERD                          
         BE    VSEL6                                                            
         GOTO1 AVALUID,LSTUIDH                                                  
         BNE   VALSELX                                                          
         MVC   SAASUID,APHALF                                                   
*                                                                               
VSEL6    CLI   LSTACGH+FHILD,0     TEST ACCESS GROUP ENTERED                    
         BE    VSEL8                                                            
         GOTO1 AVALACG,LSTACGH                                                  
         BNE   VALSELX                                                          
         MVC   SAASAGN,APHALF                                                   
*                                                                               
VSEL8    MVC   SELOVS,SAASOVS                                                   
         MVC   SELPGM,SAASPGM                                                   
         MVC   SELUID,SAASUID                                                   
         MVC   SELAGN,SAASAGN                                                   
*                                                                               
         LA    R0,LSTACTH          SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM                                                        
         MVI   APPARM+4,LSTLINEN   SET NO. OF LIST LINES                        
         LA    R0,LSTLINEL         SET LIST LINE LENGTH                         
         STCM  R0,3,APPARM+6                                                    
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO  SET SCREEN TO MODIFIED              
*                                                                               
VALSELX  B     XIT                                                              
                                                                                
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
*                                                                               
GETSEL   L     R2,AIOAREA2                                                      
         USING SAASREC,R2                                                       
         MVC   IOKEY(L'SAASKEY),APRECKEY                                        
*                                                                               
         TM    APINDS,APILRERD     TEST SEQUENTIAL READ BROKEN                  
         BZ    GSEL2                                                            
         GOTO1 AIO,IOCONFIL+IORD+IO2                                            
         BNE   GETSELN                                                          
         B     GSEL4                                                            
*                                                                               
GSEL2    TM    APINDS,APILNSEQ     TEST START OF NEW SCREEN                     
         BO    GSEL4                                                            
         LA    R1,IOCONFIL+IOHI+IO2                                             
         B     *+8                                                              
GSEL4    LA    R1,IOCONFIL+IOSQ+IO2                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
*                                                                               
         CLC   SAASKEY(SAASOVS-SAASKEY),IOKEYSAV                                
         BNE   GETSELN             TEST RECORD TYPE/AGENCY                      
*                                                                               
         CLI   SELOVS,0            MATCH ON SYSTEM/PROGRAM                      
         BE    GSEL6                                                            
         CLC   SAASOVS,SELOVS                                                   
         BNE   GETSELN                                                          
         CLI   SELPGM,0                                                         
         BE    GSEL6                                                            
         CLC   SAASPGM,SELPGM                                                   
         BNE   GETSELN                                                          
GSEL6    GOTO1 ATSTSYS,SAASOVS     TEST USER CAN CONNECT TO SYSTEM              
         BNE   GSEL4                                                            
*                                                                               
         OC    SELUID,SELUID       MATCH ON USER-ID                             
         BZ    GSEL8                                                            
         CLC   SAASUID,SELUID                                                   
         BNE   GSEL4                                                            
GSEL8    GOTO1 ATSTUID,SAASUID     TEST USER CAN CONNECT TO USER-ID             
         BNE   GSEL4                                                            
*                                                                               
         OC    SELAGN,SELAGN       MATCH ON ACCESS GROUP                        
         BZ    GSEL10                                                           
         CLC   SAASAGN,SELAGN                                                   
         BNE   GSEL4                                                            
*                                                                               
GSEL10   OC    SAASUID(L'SAASUID+L'SAASAGN),SAASUID                             
         BNZ   *+12                TEST MAIN AGENCY RECORD                      
         TM    CUSTAT,CUSDDS       TEST DDS TERMINAL                            
         BZ    GSEL4                                                            
         MVC   APRECKEY(L'SAASKEY),SAASKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     XIT                                                              
                                                                                
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
*                                                                               
DISSEL   L     R2,AIOAREA2                                                      
         USING SAASREC,R2                                                       
*                                                                               
         L     R3,APPARM                                                        
         USING LSTACTH,R3          R3=A(LIST/SELECT LINE)                       
*                                                                               
         GOTO1 ADISSYS,SAASOVS                                                  
         MVC   LSTLSYS,APWORK      SYSTEM NAME                                  
*                                                                               
         GOTO1 ADISPGM,PARM,(SAASOVS,SAASPGM)                                   
         MVC   LSTLPGM,APWORK      PROGRAM NAME                                 
*                                                                               
         OC    SAASUID,SAASUID                                                  
         BZ    DSEL2                                                            
         GOTO1 ADISUID,SAASUID                                                  
         MVC   LSTLUID,APWORK      USER-ID                                      
*                                                                               
DSEL2    OC    SAASAGN,SAASAGN                                                  
         BZ    DISSELX                                                          
         GOTO1 ADISACG,SAASAGN                                                  
         MVC   LSTLACG,APWORK      ACCESS GROUP                                 
*                                                                               
DISSELX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF ACCESS RECORD                             *         
***********************************************************************         
*                                                                               
DISKEY   LA    R2,APRECKEY                                                      
         USING SAASREC,R2                                                       
*                                                                               
         GOTO1 ADISSYS,SAASOVS                                                  
         MVC   DSPSYS,APWORK       SYSTEM NAME                                  
*                                                                               
         GOTO1 ADISPGM,PARM,(SAASOVS,SAASPGM)                                   
         MVC   DSPPGM,APWORK       PROGRAM NAME                                 
*                                                                               
         OC    SAASUID,SAASUID     USER-ID                                      
         BZ    DKEY2                                                            
         GOTO1 ADISUID,SAASUID                                                  
         MVC   DSPUID,APWORK                                                    
         MVI   DSPUIDH+FHILD,L'DSPUID                                           
*                                                                               
DKEY2    OC    SAASAGN,SAASAGN     ACCESS GROUP                                 
         BZ    DISKEYX                                                          
         GOTO1 ADISACG,SAASAGN                                                  
         MVC   DSPACG,APWORK                                                    
         MVI   DSPACGH+FHILD,L'DSPACG                                           
*                                                                               
DISKEYX  B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR LIST (SET SCREEN TO MODIFIED)           *         
***********************************************************************         
*                                                                               
FSTLST   OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         B     XIT                                                              
*                                                                               
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
*                                                                               
LSTSCR   MVI   APMODE,APMPFKS                                                   
         B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE TWA                                           *         
***********************************************************************         
SETTWA   CLI   APACTN,ACTGRD       ACTION  GRIDS?                               
         BE    XIT                                                              
         XC    SAVOVER(SAVCLRL),SAVOVER                                         
         B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
*                                                                               
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         XC    APRECKEY,APRECKEY                                                
*                                                                               
         MVI   FVMINL,1            VALIDATE REQUESTOR                           
         GOTO1 AFVAL,REPREQH                                                    
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
VREQ1    GOTOR VALSEC              VALIDATE REQUEST PID/PIN                     
         BNE   VALREQX                                                          
*                                                                               
VREQ2    GOTO1 AVALOVPG,PARM,(X'80',REPSYSH),REPPGMH,0                          
         BNE   VALREQX                                                          
         MVC   SELOVPG,APHALF                                                   
*                                                                               
VREQ4    CLI   REPUIDH+FHILD,0     TEST USER-ID ENTERD                          
         BE    VREQ6                                                            
         GOTO1 AVALUID,REPUIDH                                                  
         BNE   VALREQX                                                          
         MVC   SELUID,APHALF                                                    
*                                                                               
VREQ6    CLI   REPACGH+FHILD,0     TEST ACCESS GROUP ENTERED                    
         BE    VREQ8                                                            
         GOTO1 AVALACG,REPACGH                                                  
         BNE   VALREQX                                                          
         MVC   SELAGN,APHALF                                                    
*                                                                               
VREQ8    CLI   REPVALH+FHILD,0     TEST VALIDATION FILTER ENTERED               
         BE    VREQ9                                                            
         MVC   SELVAL,REPVAL                                                    
         CLC   REPVAL,INPYES                                                    
         BE    VREQ9                                                            
         CLC   REPVAL,INPNO                                                     
         BE    VREQ9                                                            
         LA    R0,REPVALH                                                       
         ST    R0,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALREQX                                                          
*                                                                               
VREQ9    CLI   ASONOFF,ASON        TEST IF ONLINE                               
         BE    VREQ9X                                                           
         SR    R1,R1               TEST IF PID/PIN WAS INPUT                    
         ICM   R1,1,REPREQPH+5                                                  
         BZ    VREQ9X                                                           
         MVI   REPREQP,C'*'        OVERWRITE PID/PIN WITH ASTERISKS             
         AHI   R1,-2                                                            
         BNP   VREQ9X                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REPREQP+1(0),REPREQP                                             
VREQ9X   EQU   *                                                                
*                                                                               
VREQ10   MVC   APRECKEY,SELKEY                                                  
*                                                                               
         MVCDD REPDESC,CT#ACLST    SET REPORT DESCRIPTION                       
         GOTO1 VDICTAT,PARM,C'SL  ',REPDESC                                     
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         OI    REPIND2,REPILOW                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO GENERATE REPORT                                          *         
***********************************************************************         
*                                                                               
PRTREP   MVC   SELKEY,APRECKEY                                                  
         L     R9,AREP                                                          
         USING REPD,R9                                                          
*                                                                               
         ICM   R8,15,REPABOX       TEST FOR BOXES DSECT                         
         USING BOXD,R8             R8=A(BOXD)                                   
         BZ    PREP2                                                            
         MVI   BOXYORN,YES         INITIALIZE BOXES                             
         MVI   BOXOFF,NO                                                        
         MVI   BOXBLANK,NO                                                      
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         OI    BOXDDCTL,BOXDDUL+BOXDDLC                                         
*                                                                               
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+99,C'B'                                                  
         OI    REPHEADI,REPHOPEN                                                
         OI    REPMIDSI,REPMMLIN                                                
*                                                                               
         MVC   BOXCOLS,SPACES      SET COLUMNS                                  
         LA    RF,BOXCOLS                                                       
         USING REPP1+1,RF                                                       
         MVI   REPPSYS,C'L'                                                     
         MVI   REPPPGM,C'C'                                                     
         MVI   REPPUID,C'C'                                                     
         MVI   REPPACG,C'C'                                                     
         MVI   REPPRCD,C'C'                                                     
         MVI   REPPACTS,C'C'                                                    
         MVI   REPPEND,C'R'                                                     
         DROP  RF                                                               
*                                                                               
PREP2    LA    R2,IOKEY            SET UP INITIAL KEY                           
         USING SAASREC,R2          R2=A(ACCESS RECORD KEY)                      
         XC    SAASKEY,SAASKEY                                                  
         MVI   SAASTYP,SAASTYPQ                                                 
         MVI   SAASSUB,SAASSUBQ                                                 
         MVC   SAASAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGY=                             
         BZ    *+10                                                             
         MVC   SAASAGY,OPTAGY                                                   
         MVC   SAASOVS,SELOVS                                                   
         MVC   SAASPGM,SELPGM                                                   
         MVC   SAASUID,SELUID                                                   
         MVC   SAASAGN,SELAGN                                                   
         L     R2,AIOAREA3                                                      
*                                                                               
         XC    SAVOVPG,SAVOVPG                                                  
         LA    R1,IOCONFIL+IOHI+IO3                                             
         B     *+8                                                              
PREP10   LA    R1,IOCONFIL+IOSQ+IO3                                             
         GOTO1 AIO                                                              
         BNE   PREP30                                                           
         CLC   SAASKEY(SAASOVS-SAASKEY),IOKEYSAV                                
         BNE   PREP30              TEST RECORD TYPE/AGENCY                      
*                                                                               
         OC    SAASUID(L'SAASUID+L'SAASAGN),SAASUID                             
         BZ    PREP10              IGNORE AGENCY RECORDS                        
*                                                                               
         CLI   SELOVS,0            MATCH ON SYSTEM/PROGRAM                      
         BE    PREP12                                                           
         CLC   SAASOVS,SELOVS                                                   
         BNE   PREP30                                                           
         CLI   SELPGM,0                                                         
         BE    PREP12                                                           
         CLC   SAASPGM,SELPGM                                                   
         BNE   PREP30                                                           
PREP12   GOTO1 ATSTSYS,SAASOVS     TEST USER CAN CONNECT TO SYSTEM              
         BNE   PREP10                                                           
*                                                                               
         OC    SELUID,SELUID       MATCH ON USER-ID                             
         BZ    PREP14                                                           
         CLC   SAASUID,SELUID                                                   
         BNE   PREP10                                                           
PREP14   GOTO1 ATSTUID,SAASUID     TEST USER CAN CONNECT TO USER-ID             
         BNE   PREP10                                                           
*                                                                               
         OC    SELAGN,SELAGN       MATCH ON ACCESS GROUP                        
         BZ    PREP20                                                           
         CLC   SAASAGN,SELAGN                                                   
         BNE   PREP10                                                           
*                                                                               
PREP20   TM    REPIND1,REPIPUT     TEST FIRST TIME                              
         BZ    PREP26                                                           
         LTR   R8,R8               PRINT SPACE/LINE                             
         BZ    PREP24                                                           
         IC    RE,REPLINE                                                       
         LA    RE,2(RE)                                                         
         CLM   RE,1,REPMAXL                                                     
         BL    PREP22                                                           
         BNE   PREP26              IF NEAR END OF PAGE                          
         MVI   BOXREQ,C'C'           CLOSE BOX OR DO NOTHING                    
         B     PREP24                                                           
PREP22   MVI   BOXREQ,C'B'                                                      
PREP24   GOTO1 VREPORT,REPD                                                     
*                                                                               
PREP26   BAS   RE,PRTRCD           PRINT RECORD                                 
*                                                                               
         MVC   IOKEY,SAVKEY                                                     
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         B     PREP10                                                           
*                                                                               
PREP30   LTR   R8,R8               TEST FOR BOXES                               
         BZ    PRTREPX                                                          
         IC    RE,REPLINE          CLOSE BOX                                    
         LA    RE,1(RE)              UNLESS AT END OF PAGE                      
         CLM   RE,1,REPMAXL                                                     
         BNL   PRTREPX                                                          
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VREPORT,REPD                                                     
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R8,R2                                                            
                                                                                
***********************************************************************         
* ROUTINE TO PRINT ACCESS RECORD DETAILS                              *         
***********************************************************************         
*                                                                               
PRTRCD   NTR1  ,                                                                
         L     R2,AIOAREA3                                                      
         USING SAASREC,R2                                                       
         MVC   SAVKEY,SAASKEY                                                   
*                                                                               
         GOTO1 ADISSYS,SAASOVS     PRINT SYSTEM/PROGRAM                         
         MVC   REPPSYS,APWORK                                                   
         GOTO1 ADISPGM,PARM,(SAASOVS,SAASPGM)                                   
         MVC   REPPPGM,APWORK                                                   
*                                                                               
         OC    SAASUID,SAASUID     PRINT USER-ID                                
         BZ    PRCD2                                                            
         GOTO1 ADISUID,SAASUID                                                  
         MVC   REPPUID,APWORK                                                   
*                                                                               
PRCD2    OC    SAASAGN,SAASAGN     PRINT ACCESS GROUP                           
         BZ    PRCD4                                                            
         GOTO1 ADISACG,SAASAGN                                                  
         MVC   REPPACG,APWORK                                                   
*                                                                               
PRCD4    CLC   SAVOVPG,SAASOVPG    TEST CHANGE OF PROGRAM                       
         BE    PRCD8                                                            
         MVC   SAVOVPG,SAASOVPG                                                 
         LA    R1,IOKEY            READ PROGRAM RECORD                          
         USING SAPGREC,R1            TO EXTRACT ACTION CODE LIST                
         XC    SAPGKEY,SAPGKEY                                                  
         MVI   SAPGTYP,SAPGTYPQ                                                 
         MVI   SAPGSUB,SAPGSUBQ                                                 
         MVC   SAPGOVPG,SAVOVPG                                                 
         DROP  R1                                                               
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         XC    RSAPGMEL,RSAPGMEL                                                
         GOTOR FINDEL,SAPGMELQ                                                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RSAPGMEL(0),0(R3)   PUT PROGRAM ELEMENT IN SAVED STORAGE         
         L     R1,AIOAREA1                                                      
         GOTO1 ASUBDIC                                                          
         MVC   REPM1,SPACES                                                     
         LA    R3,REPM1            SET UP MIDLINES                              
         USING REPP1,R3                                                         
         MVCDD REPPSYS,CT#SYS,L                                                 
         MVCDD REPPPGM,CT#PROG,L                                                
         MVCDD REPPUID,CT#USRID,L                                               
         MVCDD REPPACG,CT#GROUP,L                                               
         MVCDD REPPRCD,CT#REC,L                                                 
         MVCDD REPPACTS,CT#AA,L                                                 
         CLC   SELVAL,INPYES                                                    
         BNE   *+10                                                             
         MVCDD REPPACTS,CT#VALAC,L                                              
         CLC   SELVAL,INPNO                                                     
         BNE   *+10                                                             
         MVCDD REPPACTS,CT#INVAC                                                
         MVC   REPP2,REPP1         SET MIDLINE2 TO MIDLINE1 UNDERLINED          
         MVI   REPPSYS+L'REPP1,CT#ESUL                                          
         MVI   REPPPGM+L'REPP1,CT#ESUL                                          
         MVI   REPPUID+L'REPP1,CT#ESUL                                          
         MVI   REPPACG+L'REPP1,CT#ESUL                                          
         MVI   REPPRCD+L'REPP1,CT#ESUL                                          
         MVI   REPPACTS+L'REPP1,CT#ESUL                                         
         TM    DICINDS,DICIRECO    TEST RECORD WORD OVERRIDEN                   
         BZ    PRCD6                                                            
         MVC   REPPRCD,AC8RECTY                                                 
         GOTOR UNDERIZE,PARM,(L'REPPRCD,REPPRCD),REPPRCD+L'REPP1                
PRCD6    TM    DICINDS,DICIACTO    TEST ACTION WORD OVERRIDEN                   
         BZ    PRCD8                                                            
         MVC   REPPACTS(L'AC@ACT),AC@ACT                                        
         GOTOR UNDERIZE,PARM,(L'AC@ACT,REPPACTS),REPPACTS+L'REPP1               
         DROP  R3                                                               
*                                                                               
PRCD8    BAS   RE,INITRTAB         INITIALIZE RECORD TABLE                      
         GOTO1 SAASRTAB,SAASREC                                                 
*                                                                               
         L     RF,ARTAB            SELECT+ FIRST RECORD                         
         MVI   RTSEL-RTABD(RF),RTSALL                                           
         XC    RTABNTRY,RTABNTRY                                                
         NI    INDICS,FF-INDIPRCD  RESET PRINTING DONE FOR RECORD               
*                                                                               
PRCD10   BAS   RE,SELRCD           GET SELECTED RECORD                          
         BNE   PRCD20                                                           
*                                                                               
         L     RF,ARTAB                                                         
         AH    RF,RTABNTRY                                                      
         GOTO1 ADISDIC,PARM,(SAVOVS,L'REPPRCD),(C'L',RTWRD-RTABD(RF))           
         MVC   REPPRCD,APWORK                                                   
         BAS   RE,PRTACTS          PRINT ACTIONS                                
         BNZ   PRCD12                                                           
         MVC   REPPRCD,SPACES                                                   
         B     PRCD14                                                           
*                                                                               
PRCD12   OI    INDICS,INDIPRCD     SET PRINTING DONE FOR RECORD                 
         IC    RE,REPLINE          PRINT A SPACE                                
         LA    RE,1(RE)              UNLESS AT END OF PAGE                      
         CLM   RE,1,REPMAXL                                                     
         BNL   PRCD14                                                           
         GOTO1 VREPORT,REPD                                                     
*                                                                               
PRCD14   BAS   RE,RCDSELD          BUMP SELECT UP RECORD TABLE                  
         B     PRCD10                                                           
*                                                                               
PRCD20   TM    INDICS,INDIPRCD     TEST PRINTING DONE FOR RECORD                
         BO    PRTRCDX                                                          
         MVI   REPPRCD,C'*'                                                     
         MVCDD REPPRCD+1(L'REPPRCD-2),CT#NONE,C                                 
         MVI   REPPRCD+L'REPPRCD-1,C'*'                                         
         GOTO1 VREPORT,REPD                                                     
*                                                                               
PRTRCDX  B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO PRINT ACTION TABLE                                       *         
***********************************************************************         
*                                                                               
PRTACTS  NTR1  ,                                                                
         BAS   RE,INITATAB                                                      
*                                                                               
         L     R8,AATAB                                                         
         USING ATABD,R8            R8=A(ACTION TABLE ENTRY)                     
         XR    R2,R2                                                            
*                                                                               
PACTS2   CLI   ATABD,ATEOTQ                                                     
         BE    PACTS20                                                          
*                                                                               
         MVC   PRTVAL,INPNO        SET INVALID/VALID                            
         GOTO1 TESTBIT,PARM,(ATSEQ,VALID)                                       
         BZ    *+10                                                             
         MVC   PRTVAL,INPYES                                                    
*                                                                               
         CLI   SELVAL,0            TEST VALIDATION FILTER                       
         BE    PACTS4                                                           
         CLC   SELVAL,PRTVAL                                                    
         BNE   PACTS10                                                          
*                                                                               
PACTS4   GOTO1 ADISDIC,PARM,(SAVOVS,L'REPPAWRD),(C'L',ATWRD)                    
*                                                                               
         LTR   R2,R2               TEST FIRST ACTION PRINTED                    
         BZ    PACTS6                                                           
         LA    R0,L'REPPACT        BUMP R2 TO NEXT COLUMN                       
         LA    R1,REPPACTS+L'REPPACTS-1                                         
         BXLE  R2,R0,PACTS8                                                     
*                                                                               
         GOTO1 VREPORT,REPD        PRINT IF ROW IS FULL                         
*                                                                               
PACTS6   LA    R2,REPPACTS         RESET R2 TO FIRST COLUMN                     
*                                                                               
         USING REPPACT,R2                                                       
PACTS8   MVC   REPPAWRD,APWORK     PRINT ACTION                                 
         MVC   REPPAVAL,PRTVAL                                                  
         DROP  R2                                                               
*                                                                               
PACTS10  LA    R8,ATABL(R8)                                                     
         B     PACTS2                                                           
         DROP  R8                                                               
*                                                                               
PACTS20  LTR   R2,R2               PRINT LAST LINE IF NECCESERY                 
         BZ    PRTACTSX                                                         
         GOTO1 VREPORT,REPD                                                     
*                                                                               
PRTACTSX LTR   R2,R2               SET CC FOR CALLER                            
         B     XIT                                                              
                                                                                
***********************************************************************         
* INITIALIZE RECORD TYPES TABLE                                       *         
*                                                                     *         
* NTRY: SAVKEY=CURRENT ACCESS RECORD KEY                              *         
* EXIT: RCDINDS, PRNTRKEY INITIALIZED                                 *         
*       RECORD TABLE SET UP                                           *         
***********************************************************************         
INITRTAB NTR1                                                                   
*                                                                               
         MVI   RCDINDS,0           RESET RECORD INDICATORS                      
         MVC   WORKKEY,SAVKEY      SET UP PARENT KEY IN WORKKEY                 
         LA    R2,WORKKEY                                                       
         USING SAASREC,R2                                                       
*----------------------------------------                                       
* SET UP ACCESS RECORD PARENT KEY                                               
*----------------------------------------                                       
         OC    SAASAGN,SAASAGN     TEST FOR ACCESS GROUP                        
         BZ    IRTAB2                                                           
         OI    RCDINDS,RCDIACGP                                                 
         XC    SAASAGN,SAASAGN                                                  
         B     IRTAB6                                                           
                                                                                
IRTAB2   OC    SAASUID,SAASUID     TEST FOR USER-ID                             
         BZ    IRTAB4                                                           
         XC    SAASUID,SAASUID                                                  
         B     IRTAB6                                                           
                                                                                
IRTAB4   OI    RCDINDS,RCDIAGY     MAIN AGENCY RECORD                           
         XC    SAASAGY,SAASAGY                                                  
                                                                                
IRTAB6   OC    SAASUID,SAASUID     TEST PARENT IS FOR USER-ID                   
         BNZ   *+8                                                              
         OI    RCDINDS,RCDIPAGY      OR FOR AGENCY                              
                                                                                
         CLC   PRNTRKEY,SAASKEY    TEST CHANGE OF PARENT                        
         BNE   IRTAB8                                                           
         OI    RCDINDS,RCDISIBL    IF SAME PARENT                               
         BRAS  RE,CLRRTAB            JUST CLEAR CURRENT RECORD TABLE            
         B     IRTABYES                                                         
                                                                                
IRTAB8   MVC   PRNTRKEY,SAASKEY                                                 
*                                                                               
*----------------------------------------                                       
* INITIALIZE RECORD TABLE FROM RECORD RECORDS                                   
*----------------------------------------                                       
         LA    RF,SAPGMACT+L'SAPGMACT-1                                         
         CLI   0(RF),0                                                          
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         LA    RE,SAPGMACT                                                      
         SR    RF,RE               RF=NUMBER OF CODES - 1                       
         SRL   RF,3                /8                                           
         STC   RF,RBITXLEN         EXECUTABLE LENGTH OF BIT TABLES              
         LA    RF,1(RF)                                                         
         STH   RF,RBITLEN          ACTUAL LENGTH OF BIT TABLES                  
         XC    RBITEND,RBITEND                                                  
                                                                                
         LA    R2,IOKEY                                                         
         USING SARCREC,R2          R2=A(RECORD RECORD KEY)                      
         XC    SARCKEY,SARCKEY                                                  
         MVI   SARCTYP,SARCTYPQ                                                 
         MVI   SARCSUB,SARCSUBQ                                                 
         MVC   SARCOVPG,SAVOVPG                                                 
                                                                                
         L     R8,ARTAB                                                         
         USING RTABD,R8            R8=A(RECORD TABLE)                           
         L     R2,AIOAREA1         READ THROUGH RECORD RECORDS                  
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
IRTAB12  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   IRTAB20                                                          
         CLC   SARCKEY(SARCRCD-SARCKEY),IOKEYSAV                                
         BNE   IRTAB20                                                          
                                                                                
         MVC   RTTYPE,SARCRCD      BUILD TABLE ENTRY                            
         GOTOR FINDEL,SARCDELQ                                                  
         USING SARCDD,R3           R3=A(RECORD ELEMENT)                         
         MVC   RTWRD,SARCDWRD                                                   
         MVC   RTDSC,SARCDDSC                                                   
         MVC   RTATT,SARCDATT                                                   
         DROP  R3                                                               
         MVI   RTSEL,RTSNOT                                                     
         XR    RF,RF                                                            
         ICM   RF,3,RBITEND                                                     
         BNZ   *+8                                                              
         LHI   RF,RBITS-TWAD                                                    
         STCM  RF,3,RTDAVAIL       SAVE DISP. TO AVAILABLE BIT TABLE            
         AH    RF,RBITLEN                                                       
         STCM  RF,3,RTDVALID       SAVE DISP. TO VALID BIT TABLE                
         AH    RF,RBITLEN                                                       
         STH   RF,RBITEND          SAVE NEW END DISP.                           
         CLC   RBITEND,=AL2(RBITSX-TWAD)                                        
         BL    *+6                                                              
         DC    H'0'                 BIT TABLES ARE TOO MANY & TOO BIG           
                                                                                
         CLI   RTATT,0             TEST RECORD IS ATTACHED                      
         BNE   IRTAB14                                                          
         GOTOR SETACTS,PARM,SARCREC,SARCRCD                                     
         GOTOR MVCEX,(R1),RTDAVAIL,DBITTABL  SET AVAILABLE                      
         GOTOR MVCEX,(R1),RTDVALID                & VALID                       
         B     IRTAB18                                                          
                                                                                
IRTAB14  GOTO1 FINDRTAB,RTATT                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR MVCEX,PARM,RTDAVAIL,RTDVALID-RTABD(RF)                           
         GOTOR SETACTS,(R1),SARCREC,SARCRCD SET AVAILABLE                       
         GOTOR NCEX,(R1),DBITTABL,RTDAVAIL                                      
         GOTOR XCEX,(R1),RTDAVAIL,DBITTABL                                      
         GOTOR XCEX,(R1),RTDVALID,RTDVALID  VALID=INVALID (IF ATTACHED)         
*                                                                               
IRTAB18  LA    R8,RTABL(R8)                                                     
         B     IRTAB12                                                          
*                                                                               
IRTAB20  MVI   RTABD,RTEOTQ        SET E-O-T                                    
         ST    R8,ARTABX                                                        
         DROP  R2,R8                                                            
*                                                                               
*----------------------------------------                                       
* MERGE PARENT ACCESS RECORDS INTO RECORD TABLE                                 
*----------------------------------------                                       
         TM    RCDINDS,RCDIAGY     TEST MAIN AGENCY RECORD                      
         BO    IRTAB30                                                          
         LA    R2,IOKEY              NO - MERGE IN MAIN AGENCY RECORD           
         USING SAASREC,R2                                                       
         MVC   SAASKEY,PRNTRKEY                                                 
         XC    SAASUID,SAASUID                                                  
*                                                                               
         BAS   RE,PRNTRTAB                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#DRANF)                                           
         B     IRTABNO                                                          
*                                                                               
         TM    RCDINDS,RCDIPAGY    TEST PARENT IS MAIN AGENCY RECORD            
         BO    IRTAB30                                                          
         MVC   SAASKEY,PRNTRKEY      NO - MERGE IN USER-ID RECORD               
         BAS   RE,PRNTRTAB                                                      
         DROP  R2                                                               
*                                                                               
IRTAB30  MVI   RPAGENO,1           SET INITIAL PAGE INFO                        
         L     RE,ARTABX                                                        
         LA    RE,RTABPAGE-RTABL(RE)                                            
         S     RE,ARTAB                                                         
         SRDL  RE,32                                                            
         LA    R0,RTABPAGE                                                      
         DR    RE,R0                                                            
         STC   RF,RNOPAGES                                                      
         LTR   RF,RF               TEST TABLE IS EMPTY                          
         BNZ   IRTABYES                                                         
         OI    DSPINDS,DSPINONE                                                 
         OI    RCDINDS,RCDIREAD                                                 
         XC    PRNTRKEY,PRNTRKEY                                                
*                                                                               
IRTABYES CR    RB,RB                                                            
         B     XIT                                                              
IRTABNO  LTR   RB,RB                                                            
         B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO MERGE PARENT ACCESS RECORD INTO RECORD TABLE             *         
*                                                                     *         
* NTRY: IOKEY=KEY OF PARENT RECORD                                    *         
***********************************************************************         
PRNTRTAB NTR1                                                                   
*                                                                               
         L     R2,AIOAREA1         READ PARENT RECORD                           
         USING SAASREC,R2                                                       
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   PRTABN                                                           
*                                                                               
         L     R8,ARTAB                                                         
         USING RTABD,R8            R8=A(RECORD TABLE)                           
*                                                                               
PRTAB2   CLI   RTABD,RTEOTQ                                                     
         BE    PRTABY                                                           
*                                                                               
         CLI   RTATT,0             TEST RECORD ATTACHED                         
         BE    PRTAB4                                                           
         GOTO1 FINDRTAB,RTATT      AVAIL=AVAIL & AVAIL(ATTACHED)                
         BNE   PRTAB6                                                           
         GOTOR NCEX,PARM,RTDAVAIL,RTDAVAIL-RTABD(RF)                            
         BZ    PRTAB6                                                           
         GOTOR SETACTS,PARM,SAASREC,RTTYPE                                      
         GOTOR NCEX,PARM,DBITTABL,RTDAVAIL  AVAIL=AVAIL-BIT TABLE               
         GOTOR XCEX,(R1),RTDAVAIL,DBITTABL                                      
         BZ    PRTAB6                                                           
         B     PRTAB8                                                           
*                                                                               
PRTAB4   GOTOR SETACTS,PARM,SAASREC,RTTYPE                                      
         GOTOR NCEX,PARM,RTDAVAIL,DBITTABL                                      
         BZ    PRTAB6                                                           
         GOTOR MVCEX,(R1),RTDVALID,RTDAVAIL                                     
         B     PRTAB8                                                           
*                                                                               
PRTAB6   LR    RE,R8               NO AVAILABLE ACTIONS                         
         LA    R0,RTABL(RE)          SO DELETE TABLE ENTRY                      
         L     RF,ARTABX                                                        
         LA    RF,1(RF)                                                         
         SR    RF,R0                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         BCTR  RE,0                                                             
         ST    RE,ARTABX                                                        
         B     PRTAB2                                                           
*                                                                               
PRTAB8   LA    R8,RTABL(R8)        BUMP TO NEXT TABLE ENTRY                     
         B     PRTAB2                                                           
*                                                                               
PRTABY   CR    RB,RB                                                            
         B     XIT                                                              
PRTABN   LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R8                                                            
                                                                                
***********************************************************************         
* INITIALIZE OPTION TABLE                                             *         
*                                                                     *         
* NTRY: SAVKEY=CURRENT OCONTROL RECORD KEY                            *         
* EXIT: OCRINDS, PRNTOKEY INITIALIZED                                 *         
*       OPTIONS TABLE SET UP                                          *         
***********************************************************************         
INITOTAB NTR1                                                                   
*                                                                               
         MVI   OCRINDS,0           RESET OCONTROL RECORD INDICATORS             
         MVC   WORKKEY,SAVKEY      SET UP PARENT KEY IN WORK KEY                
         LA    R2,WORKKEY                                                       
         USING SAOCREC,R2                                                       
         MVI   SAOCSUB,SAOCSUBQ    OPTION CONTROL RECORD                        
*----------------------------------------                                       
* SET UP OCONTROL RECORD PARENT KEY                                             
*----------------------------------------                                       
         OC    SAOCAGN,SAOCAGN     TEST FOR ACCESS GROUP                        
         BZ    IOTAB2                                                           
         OI    OCRINDS,OCRIACGP                                                 
         XC    SAOCAGN,SAOCAGN                                                  
         B     IOTAB6                                                           
*                                                                               
IOTAB2   OC    SAOCUID,SAOCUID     TEST FOR USER-ID                             
         BZ    IOTAB4                                                           
         XC    SAOCUID,SAOCUID                                                  
         B     IOTAB6                                                           
*                                                                               
IOTAB4   OI    OCRINDS,OCRIAGY     MAIN AGENCY RECORD                           
         XC    SAOCAGY,SAOCAGY                                                  
*                                                                               
IOTAB6   OC    SAOCUID,SAOCUID     TEST PARENT IS FOR USER-ID                   
         BNZ   *+8                                                              
         OI    OCRINDS,OCRIPAGY      OR FOR AGENCY                              
*                                                                               
         CLC   PRNTOKEY,SAOCKEY    TEST CHANGE OF PARENT                        
         BNE   IOTAB8                                                           
         OI    OCRINDS,OCRISIBL                                                 
         B     IOTABYES                                                         
*                                                                               
IOTAB8   MVC   PRNTOKEY,SAOCKEY                                                 
*                                                                               
*----------------------------------------                                       
* SET UP OPTIONS TABLE                                                          
*----------------------------------------                                       
         MVI   PRNTVAL,FF          SET EVERYTHING VALID FOR PARENT              
         MVC   PRNTVAL+1(L'PRNTVAL-1),PRNTVAL                                   
*                                                                               
         TM    OCRINDS,OCRIAGY     TEST MAIN AGENCY RECORD                      
         BO    IOTAB10                                                          
         LA    R2,IOKEY              NO - MERGE IN MAIN AGENCY RECORD           
         USING SAOCREC,R2                                                       
         MVC   SAOCKEY,PRNTOKEY                                                 
         XC    SAOCUID,SAOCUID                                                  
         BAS   RE,MRGOPRNT                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#DRANF)                                           
         B     IOTABNO                                                          
*                                                                               
         TM    OCRINDS,OCRIPAGY    TEST PARENT IS MAIN AGENCY RECORD            
         BO    IOTAB10                                                          
         MVC   SAOCKEY,PRNTOKEY      NO - MERGE IN USER-ID RECORD               
         BAS   RE,MRGOPRNT                                                      
         DROP  R2                                                               
*                                                                               
IOTAB10  LA    R2,IOKEY                                                         
         USING SAOPREC,R2          R2=A(OPTION RECORD KEY)                      
         XC    SAOPKEY,SAOPKEY                                                  
         MVI   SAOPTYP,SAOPTYPQ                                                 
         MVI   SAOPSUB,SAOPSUBQ                                                 
         MVC   SAOPOVPG,SAVOVPG                                                 
         L     R2,AIOAREA1                                                      
         L     R8,AOTAB                                                         
         USING OTABD,R8            R8=A(OPTION TABLE ENTRY)                     
         XC    WORKBITT,WORKBITT                                                
*                                                                               
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
IOTAB12  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   IOTAB20             TEST OPTION RECORD FOR PROGRAM               
         CLC   SAOPKEY(SAOPOCD-SAOPKEY),IOKEYSAV                                
         BNE   IOTAB20                                                          
*                                                                               
         GOTO1 TESTBIT,PARM,(SAOPOCD,PRNTVAL)  TEST OPTION VALID                
         BZ    IOTAB12                                                          
*                                                                               
         GOTO1 SETBIT,PARM,(SAOPOCD,WORKBITT)                                   
*                                                                               
         MVC   OTCODE,SAOPOCD      ADD OPTION TABLE ENTRY                       
         GOTOR FINDEL,SAOPTELQ                                                  
         MVC   OTWRD,SAOPTWRD-SAOPTD(R3)                                        
         MVC   OTDSC,SAOPTDSC-SAOPTD(R3)                                        
*                                                                               
         LA    R8,OTABL(R8)                                                     
         B     IOTAB12                                                          
*                                                                               
IOTAB20  MVI   OTABD,EOT           SET END OF TABLE                             
         NC    PRNTVAL,WORKBITT    ENSURE NO EXTRA BITS SET ON                  
         DROP  R8,R2                                                            
*                                                                               
         C     R8,AOTAB                                                         
         BNE   IOTABYES                                                         
         OI    OCRINDS,OCRIREAD                                                 
         XC    PRNTOKEY,PRNTOKEY                                                
*                                                                               
IOTABYES CR    RB,RB                                                            
         B     XIT                                                              
IOTABNO  LTR   RB,RB                                                            
         B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO MERGE IN PARENT OCONTROL RECORD DETAILS                  *         
*                                                                     *         
* NTRY: IOKEY=KEY OF PARENT RECORD                                    *         
***********************************************************************         
MRGOPRNT NTR1                                                                   
*                                                                               
         L     R2,AIOAREA1         READ PARENT RECORD                           
         USING SAOCREC,R2                                                       
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   MOPRNTN                                                          
*                                                                               
         GOTOR FINDEL,SAOCTELQ     AND IN PARENT BIT TABLE                      
         GOTO1 ABLDBITT,PARM,(R3),WORKBITT                                      
         NC    PRNTVAL,WORKBITT                                                 
*                                                                               
MOPRNTY  CR    RB,RB                                                            
         B     XIT                                                              
MOPRNTN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
* INITIALIZE FIELD TABLE                                                        
*                                                                               
* NTRY: SAVKEY=CURRENT FCONTROL RECORD KEY                                      
* EXIT: FCRINDS, PRNTFKEY INITIALIZED                                           
*       FIELDS TABLE SET UP                                                     
***********************************************************************         
INITFTAB NTR1                                                                   
*                                                                               
         MVI   FCRINDS,0                 RESET FCONTROL RECORD INDS             
         MVC   WORKKEY,SAVKEY            SET UP PARENT KEY IN WORKKEY           
         LA    R2,WORKKEY                                                       
         USING SAFCREC,R2                                                       
         MVI   SAFCSUB,SAFCSUBQ          FIELD CONTROL RECORD                   
*----------------------------------------                                       
* SET UP FCONTROL RECORD PARENT KEY                                             
*----------------------------------------                                       
         OC    SAFCAGN,SAFCAGN           TEST FOR ACCESS GROUP                  
         BZ    IFTAB2                                                           
         OI    FCRINDS,FCRIACGP                                                 
         XC    SAFCAGN,SAFCAGN                                                  
         B     IFTAB6                                                           
*                                                                               
IFTAB2   OC    SAFCUID,SAFCUID           TEST FOR USER-ID                       
         BZ    IFTAB4                                                           
         XC    SAFCUID,SAFCUID                                                  
         B     IFTAB6                                                           
*                                                                               
IFTAB4   OI    FCRINDS,FCRIAGY           MAIN AGENCY RECORD                     
         XC    SAFCAGY,SAFCAGY                                                  
*                                                                               
IFTAB6   OC    SAFCUID,SAFCUID           TEST PARENT IS FOR USER-ID             
         BNZ   *+8                                                              
         OI    FCRINDS,FCRIPAGY          OR FOR AGENCY                          
*                                                                               
         CLC   PRNTFKEY,SAFCKEY          TEST CHANGE OF PARENT                  
         BNE   IFTAB8                                                           
         OI    FCRINDS,FCRISIBL                                                 
         B     IFTABYES                                                         
*                                                                               
IFTAB8   MVC   PRNTFKEY,SAFCKEY                                                 
*----------------------------------------                                       
*  - SET UP FIELDS TABLE                                                        
*----------------------------------------                                       
         MVI   AVAILWRT,FF               SET EVERYTHING AVAILABLE               
         MVC   AVAILWRT+1(L'AVAILWRT-1),AVAILWRT                                
         MVC   AVAILRD,AVAILWRT                                                 
*                                                                               
         TM    FCRINDS,FCRIAGY           TEST MAIN AGENCY RECORD                
         BO    IFTAB10                                                          
         LA    R2,IOKEY                  NO, MERGE IN MAIN AGENCY REC           
         USING SAFCREC,R2                                                       
         MVC   SAFCKEY,PRNTFKEY                                                 
         XC    SAFCUID,SAFCUID                                                  
         BAS   RE,MRGFPRNT                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#DRANF)                                           
         B     IFTABNO                                                          
*                                                                               
         TM    FCRINDS,FCRIPAGY          TEST PARENT IS MAIN AGENCY REC         
         BO    IFTAB10                                                          
         MVC   SAFCKEY,PRNTFKEY          NO, MERGE IN USER-ID REC               
         BAS   RE,MRGFPRNT                                                      
         DROP  R2                                                               
*                                                                               
IFTAB10  LA    R2,IOKEY                                                         
         USING SAFDREC,R2                R2=A(FIELD RECORD KEY)                 
         XC    SAFDKEY,SAFDKEY                                                  
         MVI   SAFDTYP,SAFDTYPQ                                                 
         MVI   SAFDSUB,SAFDSUBQ                                                 
         MVC   SAFDOVPG,SAVOVPG                                                 
         L     R2,AIOAREA1                                                      
         L     R8,AFTAB                                                         
         USING FTABD,R8                  R8=A(FIELD TABLE ENTRY)                
         XC    WORKBITT,WORKBITT                                                
*                                                                               
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
IFTAB12  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   IFTAB20                   TEST FIELD RECORD FOR PROGRAM          
         CLC   SAFDKEY(SAFDFCD-SAFDKEY),IOKEYSAV                                
         BNE   IFTAB20                                                          
*                                                                               
         GOTO1 TESTBIT,PARM,(SAFDFCD,AVAILRD) TEST CAN AT LEAST READ            
         BZ    IFTAB12                                                          
*                                                                               
         GOTO1 SETBIT,PARM,(SAFDFCD,WORKBITT)                                   
         MVC   FTCODE,SAFDFCD            ADD FIELD TABLE ENTRY                  
         GOTOR FINDEL,SAFLDELQ                                                  
         MVC   FTWRD,SAFLDWRD-SAFLDD(R3)                                        
         MVC   FTDSC,SAFLDDSC-SAFLDD(R3)                                        
*                                                                               
         LA    R8,FTABL(R8)                                                     
         B     IFTAB12                                                          
*                                                                               
IFTAB20  MVI   FTABD,EOT                 SET END OF TABLE                       
         NC    AVAILRD,WORKBITT          ENSURE NO EXTRA BITS ARE ON            
         NC    AVAILWRT,WORKBITT                                                
         DROP  R8,R2                                                            
*                                                                               
         C     R8,AFTAB                                                         
         BNE   IFTABYES                                                         
         OI    FCRINDS,FCRIREAD                                                 
         XC    PRNTFKEY,PRNTFKEY                                                
*                                                                               
IFTABYES CR    RB,RB                                                            
         B     XIT                                                              
IFTABNO  LTR   RB,RB                                                            
         B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO MERGE IN PARENT FCONTROL RECORD DETAILS                            
*                                                                               
* NTRY: IOKEY=KEY OF PARENT RECORD                                              
***********************************************************************         
MRGFPRNT NTR1                                                                   
*                                                                               
         L     R2,AIOAREA1                                                      
         USING SAFCREC,R2                READ PARENT RECORD                     
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   MFPRNTN                                                          
*                                                                               
         GOTOR FINDEL,SAFCWELQ           AND IN PARENT BIT TABLES               
         GOTO1 ABLDBITT,PARM,(R3),WORKBITT                                      
         NC    AVAILWRT,WORKBITT                                                
         GOTOR FINDEL,SAFCRELQ                                                  
         GOTO1 ABLDBITT,PARM,(R3),WORKBITT                                      
         NC    AVAILRD,WORKBITT                                                 
         OC    AVAILRD,AVAILWRT                                                 
*                                                                               
MFPRNTY  CR    RB,RB                                                            
         B     XIT                                                              
MFPRNTN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO MERGE ACCESS RECORD INTO RECORD TABLE                    *         
*                                                                     *         
* NTRY: R1=A(ACCESS RECORD)                                           *         
***********************************************************************         
SAASRTAB NTR1  ,                                                                
*                                                                               
         LR    R2,R1                                                            
         USING SAASREC,R2          R2=A(ACCESS RECORD)                          
         L     R8,ARTAB                                                         
         USING RTABD,R8            R8=A(RECORD TABLE ENTRY)                     
*                                                                               
SRTAB2   CLI   RTABD,RTEOTQ                                                     
         BE    SRTABXIT                                                         
*                                                                               
         GOTOR SETACTS,PARM,SAASREC,RTTYPE                                      
         GOTOR MVCEX,(R1),RTDVALID,RTDAVAIL  SET VALID/INVALID ACTIONS          
         GOTOR NCEX,(R1),,DBITTABL                                              
*                                                                               
         LA    R8,RTABL(R8)                                                     
         B     SRTAB2                                                           
*                                                                               
SRTABXIT B     XIT                                                              
         DROP  R2,R8                                                            
                                                                                
***********************************************************************         
* ROUTINE TO CONVERT RECORD TABLE INTO ACCESS RECORD                  *         
*                                                                     *         
* NTRY: R1=A(ACCESS RECORD)                                           *         
***********************************************************************         
RTABSAAS NTR1  ,                                                                
*                                                                               
         LR    R2,R1                                                            
         USING SAASREC,R2          R2=A(ACCESS RECORD)                          
*                                                                               
         MVC   SAASKEY,SAVKEY      INITIALIZE ELEMENTLESS RECORD                
         LA    R0,SAASDATA-SAASREC+1                                            
         STCM  R0,3,SAASLEN                                                     
         MVI   SAASSTAT,0                                                       
         MVI   SAASDATA,0                                                       
*                                                                               
         LA    R3,APELEM                                                        
         USING SAMIXD,R3           R3=A(RECORD/ACTIONS ELEMENT)                 
         XC    SAMIXD(SAMIXLNQ),SAMIXD                                          
         MVI   SAMIXEL,SAMIXELQ                                                 
*                                                                               
         L     R8,ARTAB                                                         
         USING RTABD,R8            R8=A(RECORD TABLE)                           
*                                                                               
RSAAS2   CLI   RTABD,RTEOTQ                                                     
         BE    RSAAS10                                                          
*                                                                               
         GOTOR OCEX,PARM,RTDVALID,RTDVALID  TEST ANY ACTIONS SET                
         BZ    RSAAS8                                                           
*                                                                               
         MVC   SAMIXRCD,RTTYPE     ADD ELEMENT                                  
         MVC   SAMIXATT,RTATT                                                   
         LH    R0,RBITLEN                                                       
         XR    RF,RF                                                            
         ICM   RF,3,RTDVALID                                                    
         GOTO1 ATAGBITT,PARM,SAMIXD,((R0),TWAD(RF))                             
         GOTO1 AADDELS,SAASREC                                                  
*                                                                               
RSAAS8   LA    R8,RTABL(R8)                                                     
         B     RSAAS2                                                           
*                                                                               
RSAAS10  B     XIT                                                              
*                                                                               
         DROP  R2,R3,R8                                                         
                                                                                
***********************************************************************         
* ROUTINE TO SELECT NEXT RECORD TYPE                                  *         
*                                                                     *         
* NTRY: RTABNTRY=FIRST ENTRY TO LOOK AT                               *         
* EXIT: CC=NOT EQUAL IF NO RECORD IS SELECTED                         *         
*       RTABNTRY=NEXT SELECTED RECORD TABLE ENTRY                     *         
*       AVAIL=BIT TABLE OF AVAILABLE ACTIONS                          *         
*       VALID=BIT TABLE OF VALID ACTIONS                              *         
***********************************************************************         
SELRCD   NTR1  ,                                                                
*                                                                               
         L     R8,ARTAB                                                         
         AH    R8,RTABNTRY                                                      
         USING RTABD,R8            R8=A(RECORD TABLE ENTRY)                     
*                                                                               
SRCD2    CLI   RTABD,RTEOTQ        TEST E-O-T                                   
         BE    SELRCDN                                                          
         CLI   RTSEL,RTSALL        TEST RECORD IS SELECTED(+)                   
         BE    SRCD4                                                            
         CLI   RTSEL,RTSIS                                                      
         BE    SRCD4                                                            
         LA    R8,RTABL(R8)                                                     
         B     SRCD2                                                            
*                                                                               
SRCD4    GOTOR MVCEX,PARM,DAVAIL,RTDAVAIL  COPY AVAILABLE/VALID ACTIONS         
         GOTOR MVCEX,(R1),DVALID,RTDVALID                                       
*                                                                               
         CLI   RTATT,0             TEST RECORD ATTACHED                         
         BE    SRCD6                                                            
         GOTO1 FINDRTAB,RTATT      ADJUST WHAT IS AVAILABLE                     
         GOTOR NCEX,PARM,DAVAIL,RTDVALID-RTABD(RF)                              
         GOTOR NCEX,(R1),DVALID,DAVAIL   ADJUST WHAT IS VALID                   
         GOTOR XCEX,(R1)                                                        
*                                                                               
SRCD6    S     R8,ARTAB            SAVE DISPLACEMENT TO SELECTED RECORD         
         STH   R8,RTABNTRY                                                      
SELRCDY  CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
SELRCDN  LTR   RB,RB               NO RECORD SELECTED                           
         B     XIT                                                              
         DROP R8                                                                
                                                                                
***********************************************************************         
* ROUTINE FOR AFTER RECORD HAS BEEN SELECTED                          *         
*                                                                     *         
* NTRY: RTABNTRY=SELECTED RECORD                                      *         
* EXIT: ACCESS RECORD IS UPDATED                                      *         
***********************************************************************         
RCDSELD  NTR1  ,                                                                
*                                                                               
         L     R8,ARTAB                                                         
         AH    R8,RTABNTRY                                                      
         USING RTABD,R8            R8=A(RECORD TABLE ENTRY)                     
*                                                                               
         CLI   RTSEL,RTSALL                                                     
         BNE   *+8                                                              
         MVI   RTSEL+RTABL,RTSALL  BUMP S+ UP THE TABLE                         
         MVI   RTSEL,RTSWAS        SET RECORD HAS BEEN SELECTED                 
*                                                                               
         CLI   APMODE,APMVALR      TEST RECORD BEING CHANGED                    
         BNE   RCDSELDX                                                         
*                                                                               
         GOTOR MVCEX,PARM,RTDVALID,DVALID  COPY NEW VALID ACTIONS               
*                                                                               
         CLI   RTATT,0             TEST RECORD ATTACHED                         
         BE    RCDSELDX                                                         
         MVC   BITTABLE,EFFS                                                    
         GOTOR XCEX,PARM,RTDVALID,DBITTABL  REVERSE BIT TABLE TO 'NO'S          
         GOTOR NCEX,(R1),,DAVAIL                                                
*                                                                               
RCDSELDX B     XIT                                                              
*                                                                               
         DROP  R8                                                               
***********************************************************************         
* ROUTINE TO FIND A RECORD TABLE ENTRY                                *         
*                                                                     *         
* NTRY: R1=A(RECORD TYPE)                                             *         
* EXIT: IF FOUND:      CC=EQUAL,   RF=A(RTAB ENTRY).                  *         
*       IF NOT FOUND:  CC=NOT EQUAL.                                  *         
***********************************************************************         
FINDRTAB L     RF,ARTAB                                                         
         USING RTABD,RF            RF=A(RECORD TABLE)                           
*                                                                               
FRTAB2   CLI   RTABD,RTEOTQ        TEST E-O-T                                   
         BE    FRTABNO                                                          
*                                                                               
         CLC   RTTYPE,0(R1)        MATCH ON RECORD TYPE                         
         BER   RE                                                               
         BH    FRTABNO                                                          
*                                                                               
         LA    RF,RTABL(RF)        BUMP RF TO NEXT ENTRY                        
         B     FRTAB2                                                           
*                                                                               
FRTABNO  LTR   RE,RE               SET CC NOT EQUAL                             
         BR    RE                                                               
*                                                                               
         DROP RF                                                                
                                                                                
***********************************************************************         
* DISPLAY GRIDS                                                                 
***********************************************************************         
DISGRID  NTR1                                                                   
*                                                                               
         L     R2,=A(GDDCLIST)                                                  
         A     R2,APRELO                                                        
         GOTO1 VDICTAT,ACPARM,C'LU  ',(R2),GDDSLIST                             
*                                                                               
         BRAS  RE,CHKFMFL                CHECK FORMAT FIELD                     
         BNE   XIT                                                              
*                                                                               
         L     RF,=A(GCTBL)              GRIDS COLUMN TABLE                     
         A     RF,APRELO                                                        
         ST    RF,AGCTBL                                                        
*                                                                               
         LHI   R3,PCDRIVEN-TWAD                                                 
         A     R3,ATWA                                                          
         USING PCDRIVEN,R3                                                      
*                                                                               
         TM    PCGRIDS,PCGPAPQ           GRIDS IN PROCESS FOR APP?              
         BO    DG010                     . YES                                  
         OI    PCGRIDS,PCGPAPQ           IT IS NOW                              
         GOTO1 AGRIDS,APPARM,(C'C',AGCTBL),GCOSEL  CLEAR GRIDS SCREEN           
         XC    SAVSEL,SAVSEL                                                    
         MVC   SAVKEY,SPACES                                                    
         MVC   FVMSGNO,=AL2(FVFEKEY)     ENTER KEY                              
         B     DG070                     CHANCE TO ENTER FILTERS                
*                                                                               
DG010    BRAS  RE,VALGSEL                VALIDATE SELECT FIELDS                 
         BNE   DGX                                                              
*                                                                               
DG030    BRAS  RE,GETGSEL                GET THE RECORD                         
         BNE   DG050                                                            
*                                                                               
DG040    BAS   RE,PREGRID                 PRE-GRID PREPARATION                  
         BL    DG030                                                            
         BH    DG042                                                            
*                                                                               
         GOTO1 AGRIDS,APPARM,AGCTBL,GCOSEL                                      
         BNE   DG060                                                            
*                                                                               
DG042    TM    RRFLAG,RRLOOPQ            REDISPLAYING RECORD                    
         BNO   DG030                     . NO, GET NEXT                         
*                                                                               
         TM    RRFLAG,RRLRDQ             FINISHED WITH RECORDS                  
         BO    DG046                     . YES, MOVE TO OPTIONS                 
         TM    RRFLAG,RRLADQ             FINISHED WITH ACTIONS                  
         BO    DG044                     . YES, NEXT RECORD                     
         LH    R1,ATABNTRY               . NO, BUMP TO NEXT RECORD              
         AHI   R1,ATABL                                                         
         STH   R1,ATABNTRY                                                      
         B     DG040                                                            
DG044    LH    R1,RTABNTRY               . NO, BUMP TO NEXT RECORD              
         AHI   R1,RTABL                                                         
         STH   R1,RTABNTRY                                                      
         NI    RRFLAG,X'FF'-RRLADQ       RESTART ACTIONS                        
         XC    ATABNTRY,ATABNTRY                                                
         B     DG040                                                            
*                                                                               
DG046    TM    RRFLAG,RRLODQ             FINISHED DISPLAYING OPTIONS?           
         BO    DG048                     . YES, MOVE TO FIELDS                  
         LH    R1,OTABNTRY               . NO, BUMP UP TO NEXT OPTION           
         AHI   R1,OTABL                                                         
         STH   R1,OTABNTRY                                                      
         B     DG040                                                            
*                                                                               
DG048    LH    R1,FTABNTRY               BUMP UP TO NEXT FIELD                  
         AHI   R1,FTABL                                                         
         STH   R1,FTABNTRY                                                      
         B     DG040                                                            
*                                                                               
DG050    TM    PCGRIDS,PCGCOFQ           OUTPUT COLUMN HEADINGS?                
         BO    DG054                                                            
         MVC   FVMSGNO,=AL2(CE#NATLS)    NOTHING AVAILABLE TO LIST              
         B     DG070                                                            
DG054    GOTO1 AGRIDS,APPARM,(C'E',AGCTBL),GCOSEL END GRIDS DISPLAY             
*                                                                               
DG060    MVC   ACSMSG,FVOMSG                                                    
         MVC   FVMSGNO,=AL2(FVFSET)                                             
DG070    LA    R1,GRDSYSH                SET CURSOR                             
         ST    R1,FVADDR                                                        
DGX      B     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
* PRE-GRIDS PROCESSING                                                          
***********************************************************************         
PREGRID  NTR1                                                                   
*                                                                               
         LA    RE,GVALS2                 INIT NON-REPEATING VALUES              
         LHI   RF,GVLN2Q                                                        
         SR    R0,R0                                                            
         LHI   R1,X'40'                                                         
         SLL   R1,24                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,AIOAREA2                                                      
         USING SAASREC,R2                                                       
         CLC   GVSYS,SPACES              NO NEED TO RE-DISP IF PRESENT          
         BH    PG020                                                            
*                                                                               
         GOTO1 ADISSYS,SAASOVS                                                  
         MVC   GVSYS,APWORK              SYSTEM NAME                            
*                                                                               
         GOTO1 ADISPGM,PARM,(SAASOVS,SAASPGM)                                   
         MVC   GVPGM,APWORK              PROGRAM NAME                           
*                                                                               
         OC    SAASUID,SAASUID                                                  
         BZ    PG010                                                            
         GOTO1 ADISUID,SAASUID                                                  
         MVC   GVUID,APWORK              USER-ID                                
*                                                                               
PG010    OC    SAASAGN,SAASAGN                                                  
         BZ    PG020                                                            
         GOTO1 ADISACG,SAASAGN                                                  
         MVC   GVSEG,APWORK              ACCESS GROUP                           
         MVC   GVSEGN,APWORK+L'GVSEG                                            
*                                                                               
*----------------------------------------                                       
* CHECK FILTERS AND SET GRID COL SELETOR                                        
*----------------------------------------                                       
PG020    CLI   SELACTL,0                 RECORD/ACTION FILTER                   
         BNE   *+12                                                             
         CLI   SELRECL,0                                                        
         BE    *+12                                                             
         MVI   GCOSEL,C'R'                                                      
         B     PG022                                                            
*                                                                               
         OC    SELAGN,SELAGN             SECURITY GROUP FILTER                  
         BNZ   *+12                                                             
         CLI   SELPGM,0                  PROGRAM FILTER                         
         BE    *+12                                                             
         MVI   GCOSEL,C'P'                                                      
         B     PG022                                                            
         MVI   GCOSEL,0                                                         
         MVI   RRFLAG,0                                                         
         B     PGOKX                                                            
*                                                                               
PG022    TM    RRFLAG,RRLOOPQ                                                   
         BO    PG050                                                            
         OI    RRFLAG,RRLOOPQ                                                   
*----------------------------------------                                       
* BUILD ACCESS TABLES                                                           
*----------------------------------------                                       
         XC    RTABNTRY,RTABNTRY                                                
         XC    OTABNTRY,OTABNTRY                                                
         XC    FTABNTRY,FTABNTRY                                                
         OI    APINDS,APILRERD           SEQUENTIAL READ BROKEN                 
*----------------------------------------                                       
* RECORDS / ACTIONS ACCESS                                                      
*----------------------------------------                                       
         LA    R1,IOKEY                  READ PROGRAM RECORD                    
         USING SAPGREC,R1                TO EXTRACT ACTION CODE LIST            
         XC    SAPGKEY,SAPGKEY                                                  
         MVI   SAPGTYP,SAPGTYPQ                                                 
         MVI   SAPGSUB,SAPGSUBQ                                                 
         MVC   SAPGOVPG,SAVOVPG                                                 
         DROP  R1                                                               
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         XC    RSAPGMEL,RSAPGMEL                                                
         GOTOR FINDEL,SAPGMELQ                                                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RSAPGMEL(0),0(R3)         SAVE PROGRAM ELEMENT                   
*                                                                               
         BAS   RE,INITRTAB               INITIALIZE RECORD TABLE                
         GOTO1 SAASRTAB,SAASREC                                                 
*----------------------------------------                                       
* OPTION CONTROL                                                                
*----------------------------------------                                       
         CLI   GCOSEL,C'R'               RECORD/ACTION FILTER                   
         BE    PG050                     . YES, NO NEED FOR OPTS/FLDS           
*                                                                               
         LA    R2,IOKEY                                                         
         USING SAOCREC,R2                OCONTROL RECORD KEY                    
         MVC   IOKEY,SAVKEY              READ CURRENT RECORD                    
         MVI   SAOCSUB,SAOCSUBQ          OPTION CONTROL RECORD                  
*                                                                               
         OI    OCRINDS,OCRIREAD                                                 
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   PG040                                                            
         NI    OCRINDS,X'FF'-OCRIREAD                                           
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTOR FINDEL,SAOCTELQ                                                  
         GOTO1 ABLDBITT,PARM,(R3),VALIDTAB                                      
         BAS   RE,INITOTAB               INITIALIZE OPTION TABLE                
         NC    VALIDTAB,PRNTVAL                                                 
*----------------------------------------                                       
* FIELD CONTROL                                                                 
*----------------------------------------                                       
PG040    LA    R2,IOKEY                                                         
         USING SAFCREC,R2                FCONTROL RECORD KEY                    
         MVC   IOKEY,SAVKEY              READ CURRENT RECORD                    
         MVI   SAFCSUB,SAFCSUBQ          FIELD CONTROL RECORD                   
*                                                                               
         OI    FCRINDS,FCRIREAD                                                 
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   PG050                                                            
         NI    FCRINDS,X'FF'-FCRIREAD                                           
*                                                                               
         L     R2,AIOAREA1               BUILD FIELD ACCESS BIT TABLES          
         GOTOR FINDEL,SAFCWELQ                                                  
         GOTO1 ABLDBITT,PARM,(R3),WRITETAB                                      
         GOTOR FINDEL,SAFCRELQ                                                  
         GOTO1 ABLDBITT,PARM,(R3),READTAB                                       
         BAS   RE,INITFTAB                                                      
         NC    WRITETAB,AVAILWRT                                                
         NC    READTAB,AVAILRD                                                  
*----------------------------------------                                       
* DISPLAY RECORDS                                                               
*----------------------------------------                                       
PG050    TM    RRFLAG,RRLRDQ             TEST IF RECORD DISPLAY IS DONE         
         BO    PG080                     . YES, MOVE ON TO OPTIONS              
         TM    RCDINDS,RCDIREAD          ANY VALID RECORD/ACTIONS?              
         BO    PG052                     . NO                                   
*                                                                               
         L     R8,ARTAB                                                         
         USING RTABD,R8                                                         
         AH    R8,RTABNTRY                                                      
*                                                                               
         MVI   RTSEL,RTSALL                                                     
         BAS   RE,SELRCD                 GET SELECTED RECORD                    
         BE    PG054                                                            
PG052    OI    RRFLAG,RRLRDQ                                                    
         B     PG080                                                            
*                                                                               
PG054    GOTO1 ADISDIC,PARM,(SAVOVS,L'REPPRCD),(C'L',RTWRD)                     
         MVC   GVREC,APWORK                                                     
*                                                                               
         OC    APWORK,SPACES                                                    
         CLC   SELREC,SPACES             CHECK FOR RECORD FILTER                
         BNH   PG060                                                            
         SR    R1,R1                                                            
         IC    R1,SELRECL                RECORD LENGTH                          
         SHI   R1,1                                                             
         BM    PG060                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELREC(0),APWORK          SAME RECORD?                           
         BE    PG060                                                            
         OI    RRFLAG,RRLADQ             FINISHED WITH ACTIONS                  
         B     PGHIX                                                            
*----------------------------------------                                       
* DISPLAY ACTIONS                                                               
*----------------------------------------                                       
PG060    L     R9,AATAB                                                         
         USING ATABD,R9                                                         
         AH    R9,ATABNTRY                                                      
         LA    R2,GVACTS                                                        
*                                                                               
         TM    RRFLAG,RRACTSQ            ACTIONS DISPLAY STARTED?               
         BO    PG064                     . YES                                  
         BRAS  RE,INITATAB                                                      
         OI    APINDS,APILRERD                                                  
         OI    RRFLAG,RRACTSQ            ACTIONS DISPLAY STARTED                
         NI    RRFLAG,X'FF'-RRFACTQ      INIT FILTERED ACTION FOUND             
*                                                                               
PG064    CLI   0(R9),ATEOTQ              ANY ACTION REMAINING?                  
         BNE   PG070                     . YES                                  
         NI    RRFLAG,X'FF'-RRACTSQ                                             
         OI    RRFLAG,RRLADQ             ACTIONS DISPLAY FINISHED               
         CLI   SELFOR,C'E'               EXPAND OPTION                          
         BE    PGHIX                                                            
         TM    RRFLAG,RRFACTQ            FILTERED ACTION FOUND?                 
         BNO   PGHIX                                                            
         SHI   R2,1                                                             
         CLI   0(R2),C','                REMOVE COMMA IF NECESSARY              
         BNE   PGOKX                                                            
         MVI   0(R2),C' '                                                       
         B     PGOKX                                                            
*                                                                               
PG070    GOTO1 ADISDIC,PARM,(SAVOVS,10),(C'L',ATWRD)                            
         MVC   0(10,R2),APWORK                                                  
*                                                                               
         OC    APWORK,SPACES                                                    
         CLC   SELACT,SPACES             CHECK FOR RECORD FILTER                
         BNH   PG072                                                            
         SR    R1,R1                                                            
         IC    R1,SELACTL                RECORD LENGTH                          
         SHI   R1,1                                                             
         BM    PG072                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELACT(0),APWORK                                                 
         BE    PG072                                                            
         CLI   SELFOR,C'E'               EXPAND OPTION                          
         BE    PGHIX                     SKIP DISPLAY LINE                      
         MVC   0(10,R2),SPACES                                                  
         B     PG079                                                            
*                                                                               
PG072    OI    RRFLAG,RRFACTQ            FILTERED ACTION FOUND                  
         LA    R1,9(R2)                                                         
PG073    CR    R1,R2                                                            
         BE    PG076                                                            
         CLI   0(R1),C' '                                                       
         BH    PG076                                                            
         AHI   R1,-1                                                            
         B     PG073                                                            
PG076    MVI   1(R1),C'='                                                       
         LA    R2,2(R1)                                                         
*                                                                               
         MVC   0(1,R2),INPNO                                                    
         GOTO1 TESTBIT,PARM,(ATSEQ,VALID)                                       
         BZ    PG078                                                            
         MVC   0(1,R2),INPYES                                                   
*                                                                               
PG078    CLI   SELFOR,C'E'               EXPAND OPTION                          
         BE    PGOKX                                                            
         MVI   1(R2),C','                SEPARATE ACTIONS WITH COMMA            
         LA    R2,2(R2)                                                         
PG079    LA    R9,ATABL(R9)              NEXT ACTION ENTRY                      
         B     PG064                                                            
*----------------------------------------                                       
* OPTIONS                                                                       
*----------------------------------------                                       
PG080    CLI   GCOSEL,C'R'               RECORD/ACTION FILTERS                  
         BE    PG102                     . YES, NO NEED FOR OPTS/FLDS           
*                                                                               
         TM    RRFLAG,RRLODQ             TEST IF OPTION DISPLAY IS DONE         
         BO    PG100                     . YES, MOVE ON TO FIELDS               
         TM    OCRINDS,OCRIREAD          ANY OPTIONS?                           
         BO    PG082                     . NO                                   
*                                                                               
         L     R8,AOTAB                                                         
         USING OTABD,R8                                                         
         AH    R8,OTABNTRY                                                      
*                                                                               
         CLI   0(R8),EOT                                                        
         BNE   PG090                                                            
PG082    OI    RRFLAG,RRLODQ             OPTIONS DONE                           
         B     PG100                                                            
                                                                                
PG090    GOTO1 ADISDIC,PARM,(SAVOVS,L'GVOPT-4),(C'L',OTWRD)                     
         MVC   GVOPT(L'GVOPT-4),APWORK                                          
*                                                                               
         LA    R2,GVOPT                                                         
         LA    R1,L'GVOPT-1(R2)                                                 
PG092    CR    R1,R2                                                            
         BE    PG094                                                            
         CLI   0(R1),C' '                                                       
         BH    PG094                                                            
         AHI   R1,-1                                                            
         B     PG092                                                            
PG094    MVI   1(R1),C'='                                                       
         LA    R2,2(R1)                                                         
*                                                                               
         MVC   0(L'INPNO,R2),INPNO                                              
         GOTO1 TESTBIT,PARM,(OTCODE,VALIDTAB)                                   
         BZ    *+10                                                             
         MVC   0(L'INPYES,R2),INPYES                                            
         B     PGOKX                                                            
*----------------------------------------                                       
* FIELDS                                                                        
*----------------------------------------                                       
PG100    TM    FCRINDS,FCRIREAD          ANY FIELDS?                            
         BO    PG102                     . NO                                   
         L     R8,AFTAB                                                         
         USING FTABD,R8                                                         
         AH    R8,FTABNTRY                                                      
*                                                                               
         CLI   0(R8),EOT                                                        
         BNE   PG110                                                            
PG102    MVI   RRFLAG,0                  FINISHED DISPLAYING ALL                
         B     PGLOX                                                            
                                                                                
PG110    GOTO1 ADISDIC,PARM,(SAVOVS,L'GVFLD-4),(C'L',FTWRD)                     
         MVC   GVFLD(L'GVFLD-4),APWORK                                          
*                                                                               
         LA    R2,GVFLD                                                         
         LA    R1,L'GVFLD-1(R2)                                                 
PG112    CR    R1,R2                                                            
         BE    PG114                                                            
         CLI   0(R1),C' '                                                       
         BH    PG114                                                            
         AHI   R1,-1                                                            
         B     PG112                                                            
PG114    MVI   1(R1),C'='                                                       
         LA    R2,2(R1)                                                         
*                                                                               
         MVC   0(L'INPNO,R2),INPNO        SET NO/WRITE/READ                     
         GOTOR TESTBIT,PARM,(FTCODE,WRITETAB)                                   
         BZ    *+14                                                             
         MVC   0(L'INPWRT,R2),INPWRT                                            
         B     PGOKX                                                            
         GOTOR TESTBIT,PARM,(FTCODE,READTAB)                                    
         BZ    *+10                                                             
         MVC   0(L'INPREAD,R2),INPREAD                                          
         B     PGOKX                                                            
*                                                                               
PGLOX    MVI   BYTE,0                    LOW   - FINISHED                       
         B     PGX                                                              
PGHIX    MVI   BYTE,2                    HIGH  - SKIP                           
         B     PGX                                                              
PGOKX    MVI   BYTE,1                    EQUAL - CONTIUE                        
PGX      CLI   BYTE,1                                                           
         B     XIT                                                              
         DROP  R2,R9                                                            
                                                                                
***********************************************************************         
* ROUTINES TO TEST/SET/RESET A BIT OF A BIT TABLE                     *         
*                                                                     *         
* NTRY: P1=(BIT CODE,A(BIT TABLE))                                    *         
***********************************************************************         
TESTBIT  XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         L     R0,0(R1)                                                         
         LA    R1,X'07'                                                         
         NR    R1,RF                                                            
         IC    R1,MASKS(R1)        R1=MASK                                      
         SRL   RF,3                                                             
         AR    RF,R0               RF=A(BYTE OF BIT)                            
         EX    R1,TESTBITM         EX A TM                                      
         BR    RE                                                               
TESTBITM TM    0(RF),0                                                          
*                                                                               
SETBIT   XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         L     R0,0(R1)                                                         
         LA    R1,X'07'                                                         
         NR    R1,RF                                                            
         LA    R1,MASKS(R1)        R1=A(MASK)                                   
         SRL   RF,3                                                             
         AR    RF,R0               RF=A(BYTE OF BIT)                            
         OC    0(1,RF),0(R1)                                                    
         BR    RE                                                               
*                                                                               
RESETBIT XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         L     R0,0(R1)                                                         
         LA    R1,X'07'                                                         
         NR    R1,RF                                                            
         LA    R1,UNMASKS(R1)      R1=A(UNMASK)                                 
         SRL   RF,3                                                             
         AR    RF,R0               RF=A(BYTE OF BIT)                            
         NC    0(1,RF),0(R1)                                                    
         BR    RE                                                               
*                                                                               
MASKS    DC    X'8040201008040201'                                              
UNMASKS  DC    X'7FBFDFEFF7FBFDFE'                                              
                                                                                
GI#ENTFD EQU   X'FF00'+02          ENTER FIELDS AS REQUIRED                     
GI#ENTCH EQU   X'FF00'+04          RECORD DISPLAYED - ENTER CHANGES             
GI#PETDE EQU   X'FF00'+24          PRESS ENTER TO DELETE                        
GI#PETRE EQU   X'FF00'+25          PRESS ENTER TO RESTORE                       
*                                                                               
RCDINPLQ EQU   DSRWRD2H-DSRWRDH    LENGTH OF RECORD/INPUT FIELDS                
RCDINPPQ EQU   DSRBBARH-DSRWRDH    LENGTH OF 1 PAGE OF RECORD/INPUTS            
RCDSDISQ EQU   RCDINPPQ/RCDINPLQ   NUMBER OF RECORDS ON A PAGE                  
RTABPAGE EQU   RTABL*RCDSDISQ      LENGTH OF 1 PAGE OF RECORD TABLE             
*                                                                               
ACTINPLQ EQU   DSAWRD2H-DSAWRDH    LENGTH OF ACTION/INPUT FIELDS                
ACTINPPQ EQU   DSABBARH-DSAWRDH    LENGTH OF 1 PAGE OF ACTION/INPUTS            
ACTSDISQ EQU   ACTINPPQ/ACTINPLQ   NUMBER OF ACTIONS ON A PAGE                  
ATABPAGE EQU   ATABL*ACTSDISQ      LENGTH OF 1 PAGE OF ACTION TABLE             
*                                                                               
HELPWRDL EQU   8                   LENGTH OF WORD FOR 'DISHELP' ROUTINE         
*                                                                               
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
                                                                                
DBITTABL DC    AL2(BITTABLE-TWAD)                                               
DVALID   DC    AL2(VALID-TWAD)                                                  
DAVAIL   DC    AL2(AVAIL-TWAD)                                                  
*                                                                               
         LTORG                                                                  
SPACES   DC    CL(L'REPP1)' '                                                   
EFFS     DC    (L'BITTABLE)X'FF'                                                
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,50,CT#ACLST,32,C                                              
         SPEC  H2,50,CT#ACLST,32,CU                                             
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,126,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
*                                                                               
GDDCLIST DS    0X                                                               
         DCDDL CT#CLPSE,L'CT@CLPSE                                              
         DCDDL CT#EXPND,L'CT@EXPND                                              
         DC    AL1(0)                                                           
                                                                                
***********************************************************************         
* ROUTINE TO FIND AN ELEMENT                                          *         
*                                                                     *         
* NTRY: RECORD IN IOAREA1, R1=ELEMENT CODE                            *         
* EXIT: R3=A(ELEMENT), RF=L(ELEMENT)                                  *         
***********************************************************************         
FINDEL   L     R3,AIOAREA1         SET R3 TO 1ST ELEMENT                        
         LA    R3,SAASDATA-SAASREC(R3)                                          
         XR    RF,RF                                                            
*                                                                               
FEL2     CLI   0(R3),0             TEST E-O-R                                   
         JE    FELOVER                                                          
*                                                                               
         IC    RF,1(R3)            RF=L(ELEMENT)                                
         CLM   R1,1,0(R3)          MATCH ON ELEMENT CODE                        
         BER   RE                                                               
         JXH   R3,RF,FEL2          BUMP R3 TO NEXT ELEMENT                      
*                                                                               
FELOVER  SR    RE,RB                                                            
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* INITIALIZE ACTION CODES TABLE                                                 
***********************************************************************         
INITATAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,IOKEY                                                         
         USING SAACREC,R2          R2=A(ACTION RECORD KEY)                      
         XC    SAACKEY,SAACKEY                                                  
         MVI   SAACTYP,SAACTYPQ                                                 
         MVI   SAACSUB,SAACSUBQ                                                 
         MVC   SAACOVPG,SAVOVPG                                                 
         L     R2,AIOAREA1                                                      
         L     R8,AATAB                                                         
         USING ATABD,R8            R8=A(ACTION TABLE)                           
*                                                                               
         LA    R1,IOHI+IOCONFIL+IO1                                             
         B     *+8                                                              
IATAB2   LA    R1,IOSQ+IOCONFIL+IO1                                             
         GOTO1 AIO                 READ ACTION RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SAACKEY(SAACACT-SAACKEY),IOKEYSAV                                
         BNE   IATAB10                                                          
*                                                                               
         LA    RF,SAPGMACT         SEARCH ACTION CODE LIST FOR CODE             
         LR    R1,RF                                                            
         LA    R0,L'SAPGMACT                                                    
IATAB4   CLC   SAACACT,0(RF)                                                    
         BE    IATAB6                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,IATAB4                                                        
         DC    H'0'                                                             
IATAB6   SR    RF,R1                                                            
         STC   RF,ATSEQ            SAVE SEQUENCE NUMBER                         
*                                                                               
         GOTO1 TESTBIT,PARM,(ATSEQ,AVAIL) TEST ACTION AVAILABLE FOR REC         
         BZ    IATAB2                                                           
         GOTOR FINDEL,SAACTELQ     SET UP TABLE ENTRY FOR ACTION                
*                                                                               
         CLI   SAACOVS,X'04'       PRINT - SPECIAL CODE FOR PREBUY              
         BNE   IATAB8                                                           
         CLI   SAACPGM,X'FA'       MYIDESK/PRISMA                               
         BNE   IATAB8                                                           
         CLI   SAACACT,X'07'       PREBUY                                       
         BNE   IATAB8                                                           
         TM    CUSTAT,CUSDDS       DDS TERMINAL                                 
         BZ    IATAB2              DDS ONLY, GET NEXT                           
*                                                                               
IATAB8   DS    0H                                                               
*                                                                               
         MVC   ATWRD,SAACTWRD-SAACTD(R3)                                        
         MVC   ATDSC,SAACTDSC-SAACTD(R3)                                        
*                                                                               
         LA    R8,ATABL(R8)                                                     
         B     IATAB2                                                           
*                                                                               
IATAB10  MVI   ATABD,ATEOTQ        SET E-O-T                                    
         DROP  R2,R8                                                            
*                                                                               
         CLI   APACTN,ACTGRD       ACTION  GRIDS?                               
         BE    IATABX              . YES                                        
*                                                                               
         MVI   APAGENO,1           SET INITAL PAGE INFO.                        
         LA    R8,ATABPAGE-ATABL(R8)                                            
         S     R8,AATAB                                                         
         SRDL  R8,32                                                            
         LA    R0,ATABPAGE                                                      
         DR    R8,R0                                                            
         STC   R9,ANOPAGES                                                      
         LTR   R9,R9               TEST SOME ACTIONS ARE AVAILABLE              
         BNZ   *+10                                                             
         MVC   SUBMSGNO,=AL2(CE#NATLS)                                          
*                                                                               
IATABX   J     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO CLEAR RECORD TYPES TABLE                                 *         
***********************************************************************         
CLRRTAB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ARTAB            UNSELECT EACH RECORD                         
         USING RTABD,RF                                                         
CLRRTAB2 CLI   RTABD,RTEOTQ                                                     
         BE    CLRRTABX                                                         
         MVI   RTSEL,RTSNOT                                                     
         LA    RF,RTABL(RF)                                                     
         B     CLRRTAB2                                                         
*                                                                               
CLRRTABX J     XIT                                                              
         DROP  RF                                                               
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY SELECT STATUS OF RECORD TYPE                     *         
*                                                                     *         
* NTRY: P1=A(RECORD INPUT FIELD)                                      *         
*       P2=A(RECORD TABLE ENTRY)                                      *         
***********************************************************************         
DISRINP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LM    R2,R3,0(R1)                                                      
REC      USING DSRINPH,R2          R2=A(RECORD INPUT FIELD)                     
         USING RTABD,R3            R3=A(RECORD TABLE ENTRY)                     
*                                                                               
         OI    REC.DSRINPH+FHOID,FHOITR                                         
         OI    REC.DSRINPH+FHIID,FHIIVA                                         
         NI    REC.DSRINPH+FHATD,FF-FHATHI                                      
*                                                                               
         XC    REC.DSRINP,REC.DSRINP NOT SELECTED                               
         CLI   RTSEL,RTSNOT                                                     
         JE    XIT                                                              
*                                                                               
         OI    REC.DSRINPH+FHATD,FHATHI HIGHLIGHT INPUT                         
*                                                                               
         MVC   REC.DSRINP,INPSELD  WAS SELECTED                                 
         CLI   RTSEL,RTSWAS                                                     
         JE    XIT                                                              
         MVC   REC.DSRINP,INPSEL   IS SELECTED                                  
         CLI   RTSEL,RTSIS                                                      
         JE    XIT                                                              
         MVC   REC.DSRINP,INPSELX  IS SELECTED+                                 
         CLI   RTSEL,RTSALL                                                     
         JE    XIT                                                              
         DC    H'0'                                                             
         DROP  REC,R3                                                           
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO MANIPULATE ACTION BIT TABLE                                        
*                                                                               
* NTRY: P1=A(DISP. IN TWA OF 1ST BIT TABLE)                                     
*       P2=A(DISP. IN TWA OF 2ND BIT TABLE)                                     
***********************************************************************         
MVCEX    NTR1  BASE=*                                                           
*                                                                               
         LM    RE,RF,0(R1)                                                      
         XR    R1,R1                                                            
         ICM   R1,3,0(RE)                                                       
         LA    R1,TWAD(R1)                                                      
         XR    R2,R2                                                            
         ICM   R2,3,0(RF)                                                       
         LA    R2,TWAD(R2)                                                      
         IC    RE,RBITXLEN                                                      
         EX    RE,*+8                                                           
         J     XIT                                                              
         MVC   0(0,R1),0(R2)                                                    
*                                                                               
XCEX     NTR1  BASE=*                                                           
         LM    RE,RF,0(R1)                                                      
         XR    R1,R1                                                            
         ICM   R1,3,0(RE)                                                       
         LA    R1,TWAD(R1)                                                      
         XR    R2,R2                                                            
         ICM   R2,3,0(RF)                                                       
         LA    R2,TWAD(R2)                                                      
         IC    RE,RBITXLEN                                                      
         EX    RE,*+8                                                           
         J     XIT                                                              
         XC    0(0,R1),0(R2)                                                    
*                                                                               
NCEX     NTR1  BASE=*                                                           
         LM    RE,RF,0(R1)                                                      
         XR    R1,R1                                                            
         ICM   R1,3,0(RE)                                                       
         LA    R1,TWAD(R1)                                                      
         XR    R2,R2                                                            
         ICM   R2,3,0(RF)                                                       
         LA    R2,TWAD(R2)                                                      
         IC    RE,RBITXLEN                                                      
         EX    RE,*+8                                                           
         J     XIT                                                              
         NC    0(0,R1),0(R2)                                                    
*                                                                               
OCEX     NTR1  BASE=*                                                           
         LM    RE,RF,0(R1)                                                      
         XR    R1,R1                                                            
         ICM   R1,3,0(RE)                                                       
         LA    R1,TWAD(R1)                                                      
         XR    R2,R2                                                            
         ICM   R2,3,0(RF)                                                       
         LA    R2,TWAD(R2)                                                      
         IC    RE,RBITXLEN                                                      
         EX    RE,*+8                                                           
         J     XIT                                                              
         OC    0(0,R1),0(R2)                                                    
                                                                                
***********************************************************************         
* ROUTINE TO CHECK THE FORMAT FIELD ON THE GRID SCREEN                          
***********************************************************************         
CHKFMFL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OI    GRDFORH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 AFVAL,GRDFORH                                                    
         BNE   CFF010                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,GRDFORH+(FVILEN-FVIHDR)                                       
         SHI   R1,1                                                             
         BM    CFF010                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   GRDFOR(0),CT@CLPSE                                               
         BE    CFF010                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   GRDFOR(0),CT@EXPND                                               
         BE    CFF020                                                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     CFFERX                                                           
*                                                                               
CFF010   CLI   OPTEXP,C'Y'                                                      
         BE    CFF020                                                           
         MVC   GRDFOR,CT@CLPSE                                                  
         MVI   SELFOR,C'C'                                                      
         B     CFFOKX                                                           
                                                                                
CFF020   MVC   GRDFOR,SPACES                                                    
         MVC   GRDFOR,CT@EXPND                                                  
         MVI   SELFOR,C'E'                                                      
*                                                                               
CFFOKX   CR    RB,RB                                                            
         J     XIT                                                              
CFFERX   LTR   RB,RB                                                            
         J     XIT                                                              
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO SAVE ACTIVITY ELEMENT DETAILS                            *         
*                                                                     *         
* NTRY: R1=A(ACCESS RECORD)                                           *         
* EXIT: SAVSEQNO=SEQUENCE NUMBER, SAVDATE=TEXT FOR DATE               *         
***********************************************************************         
SAVEACT  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ADISACT,(R1)                                                     
         MVC   SAVSEQNO,ACTEL+(SAACVSEQ-SAACVD)                                 
         MVC   SAVDATE,FVXTRA                                                   
         XC    FVXTRA,FVXTRA                                                    
         J     XIT                                                              
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO UNDERLINE A LEFT ALIGNED WORD(S)                         *         
*                                                                     *         
* NTRY: P1 BYTE 0 = L(INPUT)                                          *         
*             1-3 = A(INPUT)                                          *         
*       P2        = A(OUTPUT)                                         *         
***********************************************************************         
UNDERIZE NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         AR    R2,RF                                                            
         AR    R3,RF                                                            
UNDER02  BCTR  R2,0                                                             
         BCTR  R3,0                                                             
         CLI   0(R2),C' '                                                       
         BH    UNDER04                                                          
         BCT   RF,UNDER02                                                       
         J     XIT                                                              
*                                                                               
UNDER04  MVI   0(R3),C'-'                                                       
         BCTR  R3,0                                                             
         BCT   RF,UNDER04                                                       
         J     XIT                                                              
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO SET VALID ACTIONS FOR RECORD TYPE                        *         
*                                                                     *         
* NTRY: P1=A(ACCES RECORD)                                            *         
*       P2=A(RECORD TYPE)                                             *         
* EXIT: CC=EQUAL IF FOUND                                             *         
*       BITTABLE IS SET FOR VALID ACTIONS                             *         
***********************************************************************         
SETACTS  NTR1  BASE=*,LABEL=*                                                   
         XC    BITTABLE,BITTABLE                                                
*                                                                               
         LM    R2,R3,0(R1)                                                      
         LA    R2,SAASDATA-SAASREC(R2)                                          
         USING SAMIXD,R2           R2=A(RECORD/ACTIONS ELEMENT)                 
         XR    RF,RF                                                            
*                                                                               
SACTS2   CLI   SAMIXEL,0           TEST E-O-R                                   
         BE    SETACTSN                                                         
         CLI   SAMIXEL,SAMIXELQ    TEST ELEMENT TYPE                            
         BNE   SACTS4                                                           
         CLC   SAMIXRCD,0(R3)      TEST RECORD CODE                             
         BE    SETACTSY                                                         
SACTS4   IC    RF,SAMIXLN                                                       
         BXH   R2,RF,SACTS2                                                     
*                                                                               
SETACTSY GOTO1 ABLDBITT,PARM,SAMIXD,(L'BITTABLE,BITTABLE)                       
         CR    RB,RB                                                            
         J     XIT                                                              
SETACTSN LTR   RB,RB                                                            
         J     XIT                                                              
         DROP  R2                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE AND PROCESS PQ PID/PIN SECURITY                 *         
***********************************************************************         
VALSEC   NTR1  BASE=*,LABEL=*                                                   
         L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VSEC1    GOTO1 AFVAL,REPREQPH      VALIDATE REQUEST PID/PIN                     
         BNE   VSEC1X                                                           
         OI    REPREQPH+6,X'80'                                                 
         ICM   RF,15,=V(FAPQSEC)   TEST IF NEW PID/PIN VALIDATION               
         BZ    *+12                                                             
         A     RF,APRELO           RELOCATE IF FAPQSEC INCLUDED                 
         B     VSEC1A                                                           
         L     RF,ACOM                                                          
         ICM   RF,15,CFAPQSEC-COMFACSD(RF)                                      
         BNZ   VSEC1A                                                           
         MVC   REPPSWD,FVIFLD      SET 4-CHR PIN                                
         MVI   REPSECF1,REPSPIN    SET PIN PROTECTED                            
         MVI   REPSECF2,0                                                       
         B     VSEC1X                                                           
VSEC1A   GOTO1 (RF),APPARM,(X'80',REPREQPH),0,ACOM,APDUB                        
         CLI   12(R1),0                                                         
         BE    VSEC1B                                                           
         TM    12(R1),X'81'        TEST INVALID PID                             
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(106)                                                
         B     VSEC1X                                                           
         TM    12(R1),X'82'        TEST INVALID PIN                             
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(105)                                                
         B     VSEC1X                                                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VSEC1X                                                           
VSEC1B   CLI   APDUB,0             TEST NO PID OR PIN INPUT                     
         BE    VSEC1X                                                           
         MVC   REPPSWD,APDUB+1     SET PID OR PIN VALUE                         
         MVI   REPSECF2,0                                                       
         CLI   APDUB,2                                                          
         BNE   *+12                                                             
         MVI   REPSECF1,REPSPIN    SET PIN PROTECTED                            
         B     VSEC1X                                                           
         CLI   APDUB,3                                                          
         BNE   *+12                                                             
         MVI   REPSECF1,REPSPID    SET PID PROTECTED                            
         B     VSEC1X                                                           
*                                                                               
VSEC1X   CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         J     XIT                                                              
         DROP  R9                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS FOR GRIDS                               
***********************************************************************         
VALGSEL  NTR1  BASE=*,LABEL=*                                                   
         NI    APINDS,X'FF'-APILNSEQ                                            
*                                                                               
         LA    R2,APRECKEY         INITIALIZE RECORD KEY                        
         USING SAASREC,R2                                                       
         XC    SAASKEY,SAASKEY                                                  
         MVI   SAASTYP,SAASTYPQ                                                 
         MVI   SAASSUB,SAASSUBQ                                                 
         MVC   SAASAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGY=                             
         BZ    *+10                                                             
         MVC   SAASAGY,OPTAGY                                                   
*                                                                               
         GOTO1 AVALOVPG,PARM,(X'80',GRDSYSH),GRDPGMH,0                          
         BNE   VGSX                                                             
         MVC   SAASOVPG,APHALF                                                  
*                                                                               
         CLI   GRDUIDH+FHILD,0     TEST USER-ID ENTERD                          
         BE    VGS010                                                           
         GOTO1 AVALUID,GRDUIDH                                                  
         BNE   VGSX                                                             
         MVC   SAASUID,APHALF                                                   
*                                                                               
VGS010   CLI   GRDACGH+FHILD,0     TEST ACCESS GROUP ENTERED                    
         BE    VGS020                                                           
         GOTO1 AVALACG,GRDACGH                                                  
         BNE   VGSX                                                             
         MVC   SAASAGN,APHALF                                                   
*                                                                               
VGS020   MVC   SELOVS,SAASOVS                                                   
         MVC   SELPGM,SAASPGM                                                   
         MVC   SELUID,SAASUID                                                   
         MVC   SELAGN,SAASAGN                                                   
*                                                                               
         CLI   GRDACGH+FHILD,0           SEC GROUP OR                           
         BNE   VGS022                                                           
         CLI   GRDPGMH+FHILD,0           PROGRAM REQUIRED                       
         BNE   VGS022                                                           
         CLI   GRDRECH+FHILD,0           FOR RECORD OR                          
         BNE   *+12                                                             
         CLI   GRDACTH+FHILD,0           ACTION FILTER                          
         BE    VGS022                                                           
         LA    RE,GRDPGMH                                                       
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFREQD)                                            
         B     VGSX                                                             
*                                                                               
VGS022   MVC   SELREC,GRDREC                                                    
         MVC   SELRECL,GRDRECH+FHILD                                            
         MVC   SELACT,GRDACT                                                    
         MVC   SELACTL,GRDACTH+FHILD                                            
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO  SET SCREEN TO MODIFIED              
*                                                                               
         LHI   R3,PCDRIVEN-TWAD                                                 
         A     R3,ATWA                                                          
         USING PCDRIVEN,R3                                                      
                                                                                
         TM    PCGRIDS,PCGBEGQ                                                  
         BNO   VGS032                                                           
         CLC   SAVSEL,SELKEY                                                    
         BE    VGS040                                                           
*                                                                               
VGS032   NI    PCGRIDS,X'FF'-PCGBEGQ-PCGCOFQ                                    
         MVC   SAVSEL,SELKEY                                                    
         MVI   RRFLAG,0                                                         
         B     VGS042                                                           
*                                                                               
VGS040   CLC   SAVKEY,SPACES                                                    
         BH    VGS080                                                           
VGS042   MVC   SAVKEY,APRECKEY                                                  
         XC    PRNTRKEY,PRNTRKEY                                                
         XC    PRNTOKEY,PRNTOKEY                                                
         XC    PRNTFKEY,PRNTFKEY                                                
         NI    APINDS,X'FF'-APILRERD     TEST SEQUENTIAL READ BROKEN            
         DROP  R3                                                               
*                                                                               
VGS080   CR    RB,RB                                                            
VGSX     J     XIT                                                              
         LTORG                                                                  
                                                                                
***********************************************************************         
* GET NEXT GRIDS RECORD                                                         
***********************************************************************         
GETGSEL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,GVALS                  CLEAR GVALS                            
         LHI   RF,GVLNQ                                                         
         SR    R0,R0                                                            
         LHI   R1,X'40'                                                         
         SLL   R1,24                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,AIOAREA2                                                      
         USING SAASREC,R2                                                       
         MVC   IOKEY(L'SAASKEY),SAVKEY                                          
*                                                                               
         TM    APINDS,APILRERD     TEST SEQUENTIAL READ BROKEN                  
         BZ    GGS020                                                           
         GOTO1 AIO,IOCONFIL+IORD+IO2                                            
         BNE   GGSXN                                                            
*                                                                               
GGS020   TM    APINDS,APILNSEQ     TEST START OF NEW SCREEN                     
         BO    GGS040                                                           
GGS022   LA    R1,IOCONFIL+IOHI+IO2                                             
         B     *+8                                                              
GGS040   LA    R1,IOCONFIL+IOSQ+IO2                                             
         GOTO1 AIO                                                              
         MVC   SAVKEY(L'SAASKEY),SAASKEY                                        
         BNE   GGSXN                                                            
*                                                                               
         CLC   SAASKEY(SAASOVS-SAASKEY),IOKEYSAV                                
         BNE   GGSXN               TEST RECORD TYPE/AGENCY                      
*                                                                               
         CLI   SELOVS,0            MATCH ON SYSTEM/PROGRAM                      
         BE    GGS060                                                           
         CLC   SAASOVS,SELOVS                                                   
         BNE   GGSXN                                                            
         CLI   SELPGM,0                                                         
         BE    GGS060                                                           
         CLC   SAASPGM,SELPGM                                                   
         BNE   GGSXN                                                            
GGS060   GOTO1 ATSTSYS,SAASOVS     TEST USER CAN CONNECT TO SYSTEM              
         BNE   GGS040                                                           
*                                                                               
         OC    SELUID,SELUID       MATCH ON USER-ID                             
         BZ    GGS070                                                           
         CLC   SAASUID,SELUID                                                   
         BNE   GGS040                                                           
GGS070   GOTO1 ATSTUID,SAASUID     TEST USER CAN CONNECT TO USER-ID             
         BNE   GGS040                                                           
*                                                                               
         OC    SELAGN,SELAGN       MATCH ON ACCESS GROUP                        
         BZ    GGS080                                                           
         CLC   SAASAGN,SELAGN                                                   
         BNE   GGS040                                                           
*                                                                               
GGS080   OC    SAASUID(L'SAASUID+L'SAASAGN),SAASUID                             
         BNZ   *+12                TEST MAIN AGENCY RECORD                      
         TM    CUSTAT,CUSDDS       TEST DDS TERMINAL                            
         BZ    GGS040                                                           
*                                                                               
         MVC   SAVOVPG,SAASOVPG                                                 
*                                                                               
GGS100   OI    APINDS,APILNSEQ                                                  
         CR    RB,RB                                                            
         J     XIT                                                              
GGSXN    LTR   RB,RB               SET NO MORE RECORDS TO COME                  
         J     XIT                                                              
         LTORG                                                                  
                                                                                
***********************************************************************         
* GRIDS COLUMN TABLE - COVERED BY GRIDFD AND GRIDCD                             
***********************************************************************         
GCTBL    DC    C'090'                         GRIDS FORMAT ID                   
         DC    AL1(8)                         N/D                               
         DC    AL2(GRDLIN1-TWAD)              DISP TO FIRST LINE                
         DC    AL2(GRDLINL-TWAD)              DISP TO LAST LINE                 
         DC    AL1(L'GRDLIN1)                 LENGTH OF LINE                    
         DC    AL1(GRDLIN2-GRDLIN1)           DISP BETWEEN LINES                
         DC    AL1(0)                         # OF GRIDS DESC LINES             
         DC    C'4'                           FIXED COLUMNS                     
                                                                                
*---------------------------------------------                                  
* GRIDS DESCRIPTION DATA                                                        
*---------------------------------------------                                  
         DC    AL1(GRLDQ)     * SYSTEM *      GRIDS DECRIPTION DATA             
         DC    AL1(0)                         GENERAL INDICATOR                 
         DC    AL1(L'GRDSYSN)                 DESCR. NAME FIELD LENGTH          
         DC    AL1(L'GRDSYS)                  DESCR. NAME DATA LENGTH           
         DC    AL2(GRDSYSN-TWAD)              DISPL. TO NAME (IN TWA)           
         DC    AL2(GRDSYS-TWAD)               DISPL. TO DATA (IN TWA)           
         DC    AL2(0)                         N/D                               
*                                                                               
         DC    AL1(GRLDQ),AL1(0)              PROGRAM                           
         DC    AL1(L'GRDPGMN,L'GRDPGM)                                          
         DC    AL2(GRDPGMN-TWAD,GRDPGM-TWAD)                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(GRLDQ),AL1(0)              USER-ID                           
         DC    AL1(L'GRDUIDN,L'GRDUID)                                          
         DC    AL2(GRDUIDN-TWAD,GRDUID-TWAD)                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(GRLDQ),AL1(0)              SECURITY GROUP                    
         DC    AL1(L'GRDACGN,L'GRDACG)                                          
         DC    AL2(GRDACGN-TWAD,GRDACG-TWAD)                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(GRLDQ),AL1(0)              RECORD                            
         DC    AL1(L'GRDRECN,L'GRDREC)                                          
         DC    AL2(GRDRECN-TWAD,GRDREC-TWAD)                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(GRLDQ),AL1(0)              ACTION                            
         DC    AL1(L'GRDACTN,L'GRDACT)                                          
         DC    AL2(GRDACTN-TWAD,GRDACT-TWAD)                                    
         DC    AL2(0)                                                           
                                                                                
*---------------------------------------------                                  
* GRIDS COLUMNS                                                                 
*---------------------------------------------                                  
         DC    CL3'SY'         * SYSTEM *     COLUMN ID           +00           
         DC    AL1(GRCTTXT)                   DATA TYPE           +02           
         DC    AL1(0)                         FORMAT OPTIONS      +03           
         DCDDL CT#SYS,16                      COLUMN NAME         +04           
         DC    AL2(GVSYS-WORKD)               DISP TO DATA        +08           
         DC    AL1(L'GVSYS)                   LENGTH OF DATA      +10           
         DC    AL1(GRCDWS)                    DATA INDICATOR      +11           
         DC    AL1(0)                         ELEMENT CODE        +12           
         DC    AL1(0)                         ELEMENT SUB-CODE    +13           
         DC    AL1(0)                         GENERAL INDICATOR   +14           
         DC    AL1(0)                         SPLIT POSITION      +15           
         DC    AL1(0)                         COLUMN SELECTOR     +16           
         DC    AL1(0)                         COLUMN INDICATOR    +17           
*                                                                               
         DC    CL3'PG'                        * PROGRAM *                       
         DC    AL1(GRCTTXT,0)                                                   
         DCDDL CT#PROG,16                                                       
         DC    AL2(GVPGM-WORKD)                                                 
         DC    AL1(L'GVPGM,GRCDWS)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
*                                                                               
         DC    CL3'US'                        * USER-ID *                       
         DC    AL1(GRCTTXT,0)                                                   
         DCDDL CT#USRID,16                                                      
         DC    AL2(GVUID-WORKD)                                                 
         DC    AL1(L'GVUID,GRCDWS)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
*                                                                               
         DC    CL3'AG'                        * SECURITY GROUP *                
         DC    AL1(GRCTTXT,0)                                                   
         DCDDL CT#SECGR,16                                                      
         DC    AL2(GVSEG-WORKD)                                                 
         DC    AL1(L'GVSEG,GRCDWS)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
*                                                                               
         DC    CL3'SN'                        * SECURITY GROUP NAME *           
         DC    AL1(GRCTTXT,GRCFHID)                                             
         DCDDL CT#SECGN,20                                                      
         DC    AL2(GVSEGN-WORKD)                                                
         DC    AL1(L'GVSEGN,GRCDWS)                                             
         DC    AL1(0,0,0,0,0,0)                                                 
*        -------------------------------------FOR RECORD/ACTION FILTER          
         DC    CL3'REC'                       * RECORD *                        
         DC    AL1(GRCTTXT,0)                                                   
         DCDDL CT#REC,18                                                        
         DC    AL2(GVREC-WORKD)                                                 
         DC    AL1(L'GVREC,GRCDWS)                                              
         DC    AL1(0,0,0,0)                                                     
         DC    C'R',AL1(0)                                                      
*                                                                               
         DC    CL3'ACT'                       * ACTION *                        
         DC    AL1(GRCTTXT,GRCFSIZ)                                             
         DCDDL CT#ACT,18                                                        
         DC    AL2(GVACTS-WORKD)                                                
         DC    AL1(L'GVACTS,GRCDWS)                                             
         DC    AL1(0,0,0,0)                                                     
         DC    C'R',AL1(0)                                                      
*        -------------------------------------FOR PROGRAM/SEC GRP FILT          
         DC    CL3'REC'                       * RECORD *                        
         DC    AL1(GRCTTXT,0)                                                   
         DCDDL CT#REC,18                                                        
         DC    AL2(GVREC-WORKD)                                                 
         DC    AL1(L'GVREC,GRCDWS)                                              
         DC    AL1(0,0,0,0)                                                     
         DC    C'P',AL1(0)                                                      
*                                                                               
         DC    CL3'ACT'                       * ACTION *                        
         DC    AL1(GRCTTXT,GRCFSIZ)                                             
         DCDDL CT#ACT,18                                                        
         DC    AL2(GVACTS-WORKD)                                                
         DC    AL1(L'GVACTS,GRCDWS)                                             
         DC    AL1(0,0,0,0)                                                     
         DC    C'P',AL1(0)                                                      
*                                                                               
         DC    CL3'OPT'                       * OPTIONS CONTROL *               
         DC    AL1(GRCTTXT,0)                                                   
         DCDDL CT#OCON,20                                                       
         DC    AL2(GVOPT-WORKD)                                                 
         DC    AL1(L'GVOPT,GRCDWS)                                              
         DC    AL1(0,0,0,0)                                                     
         DC    C'P',AL1(0)                                                      
*                                                                               
         DC    CL3'FLD'                       * FIELD CONTROL *                 
         DC    AL1(GRCTTXT,0)                                                   
         DCDDL CT#FCON,20                                                       
         DC    AL2(GVFLD-WORKD)                                                 
         DC    AL1(L'GVFLD,GRCDWS)                                              
         DC    AL1(0,0,0,0)                                                     
         DC    C'P',AL1(0)                                                      
*        -------------------------------------                                  
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* SEACSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSWRK                                                       
         PRINT ON                                                               
                                                                                
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSF6D                                                       
         ORG   DSPTABH                                                          
       ++INCLUDE SEACSE1D                                                       
         ORG   DSPTABH                                                          
       ++INCLUDE SEACSE2D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSC6D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSD6D                                                       
*                                                                               
         ORG   LSTLINE             * LIST LINE LAYOUT *                         
LSTLSYS  DS    CL7                 SYSTEM                                       
         DS    CL2                                                              
LSTLPGM  DS    CL7                 PROGRAM                                      
         DS    CL2                                                              
LSTLUID  DS    CL8                 USER-ID                                      
         DS    CL2                                                              
LSTLACG  DS    CL46                ACCESS GROUP                                 
         ORG                                                                    
*                                                                               
LSTLINEL EQU   LSTACT2H-LSTACTH    LIST LINE LENGTH                             
LSTLINES EQU   LSTFOOTH-LSTACTH    LIST LINES LENGTH                            
LSTLINEN EQU   LSTLINES/LSTLINEL   NO. OF LIST LINES                            
*                                                                               
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSB6D                                                       
                                                                                
***********************************************************************         
* PRINT LINE LAYOUT                                                   *         
***********************************************************************         
REPD     DSECT                                                                  
         ORG   REPP1                                                            
         DS    CL1                                                              
REPPSYS  DS    CL7                 SYSTEM                                       
         DS    CL1                                                              
REPPPGM  DS    CL7                 PROGRAM                                      
         DS    CL1                                                              
REPPUID  DS    CL8                 USER-ID                                      
         DS    CL1                                                              
REPPACG  DS    CL8                 ACCESS GROUP                                 
         DS    CL1                                                              
REPPRCD  DS    CL8                 RECORD TYPE                                  
         DS    CL1                                                              
REPPACTS DS    0CL87               ACTIONS                                      
REPPACT  DS    0CL15                                                            
REPPAWRD DS    CL8                 ACTION WORD                                  
         DS    CL1                                                              
REPPAVAL DS    CL1                 VALID/INVALID                                
         ORG   REPPACTS+L'REPPACTS                                              
         DS    CL1                                                              
REPPEND  DS    0C                  END OF LINE                                  
                                                                                
***********************************************************************         
* RECORD TYPE TABLE                                                             
***********************************************************************         
RTABD    DSECT                                                                  
RTEOTQ   EQU   0                   END-OF-TABLE INDICATOR                       
RTTYPE   DS    XL1                 RECORD TYPE                                  
RTWRD    DS    XL2                 DICTIONARY REFERENCE NUMBER                  
RTDSC    DS    XL2                 TEXT DESCRIPTION NUMBER                      
RTSEL    DS    XL1                 SELECT STATUS                                
RTSIS    EQU   C'S'                RECORD IS SELECTED                           
RTSWAS   EQU   C'*'                RECORD WAS SELECTED                          
RTSALL   EQU   C'+'                ALL FOLLOWING RECORDS ARE SELECTED           
RTSNOT   EQU   C' '                RECORD IS NOT SELECTED AT ALL                
RTATT    DS    XL1                 RECORD TYPE ATTACHED TO (OR 0)               
RTDAVAIL DS    XL2                 DISP. TO AVAILABLE ACTIONS BIT TABLE         
RTDVALID DS    XL2                 DISP. TO VALID ACTIONS BIT TABLE             
RTABL    EQU   *-RTABD                                                          
                                                                                
                                                                                
***********************************************************************         
* ACTION CODE TABLE                                                             
***********************************************************************         
ATABD    DSECT                                                                  
ATEOTQ   EQU   FF                  END-OF-TABLE INDICATOR                       
ATSEQ    DS    XL1                 ACTION SEQUENCE NUMBER                       
ATWRD    DS    XL2                 DICTIONARY REFERENCE NUMBER                  
ATDSC    DS    XL2                 TEXT DESCRIPTION REFERENCE NUMBER            
ATABL    EQU   *-ATABD                                                          
                                                                                
***********************************************************************         
* DSECT FOR OPTIONS TABLE                                                       
***********************************************************************         
OTABD    DSECT                                                                  
OTCODE   DS    XL1                 OPTION CODE                                  
OTWRD    DS    XL2                 OPTION WORD DICTIONARY REFERENCE#            
OTDSC    DS    XL2                 OPTION TEXT DESCRIPTION#                     
OTABL    EQU   *-OTABD                                                          
                                                                                
***********************************************************************         
* DSECT FOR FIELD TABLE                                                         
***********************************************************************         
FTABD    DSECT                                                                  
FTCODE   DS    XL1                 FIELD CODE                                   
FTWRD    DS    XL2                 FIELD WORD DICTIONARY REFERENCE#             
FTDSC    DS    XL2                 FIELD TEXT DESCRIPTION#                      
FTABL    EQU   *-FTABD                                                          
                                                                                
***********************************************************************         
* WORKING STORAGE SAVED IN TWA                                        *         
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   SAVOVER                                                          
*                                                                               
SAVOVPG  DS    0XL2                CURRENT OVERLAY SYSTEM/PROGRAM               
SAVOVS   DS    XL1                 CURRENT OVERLAY SYSTEM                       
SAVPGM   DS    XL1                 CURRENT PROGRAM                              
*                                                                               
SAVKEY   DS    XL(L'SAASKEY)       CURRENT ACCESS RECORD KEY                    
PRNTRKEY DS    XL(L'SAASKEY)       CURRENT ACCESS PARENT'S RECORD KEY           
PRNTOKEY DS    XL(L'SAASKEY)       CURRENT ACCESS PARENT'S OPTION KEY           
PRNTFKEY DS    XL(L'SAASKEY)       CURRENT ACCESS PARENT'S FIELD KEY            
*                                                                               
SAVDATE  DS    XL(L'FVXTRA)        ACTIVITY DATE OF CURRENT RECORD              
SAVSEQNO DS    XL(L'SAACVSEQ)      SEQUENCE NUMBER OF CURRENT RECORD            
*                                                                               
SAVAPIND DS    XL1                 SAVED APINDS                                 
SAVAPACT DS    XL1                 SAVED APACTN                                 
*                                                                               
RCDINDS  DS    XL1                 * ACCESS RECORD INDICATORS *                 
RCDIAGY  EQU   X'80'               MAIN AGENCY ACCESS RECORD                    
RCDISIBL EQU   X'40'               RECORD IS A SIBLING OF LAST RECORD           
RCDIPAGY EQU   X'20'               RECORD'S PARENT IS AGENCY RECORD             
RCDIACGP EQU   X'10'               RECORD FOR ACCESS GROUP                      
RCDIPRNT EQU   X'08'               ACCESS RECORD IS A PARENT                    
RCDIREAD EQU   X'01'               READ RECORD                                  
*                                                                               
OCRINDS  DS    XL1                 * OPTION CONTROL RECORD INDICATORS *         
OCRIAGY  EQU   X'80'               MAIN AGENCY DEFAULT RECORD                   
OCRISIBL EQU   X'40'               RECORD IS SIBLING OF LAST RECORD             
OCRIPAGY EQU   X'20'               RECORD'S PARENT IS AGENCY RECORD             
OCRIACGP EQU   X'10'               RECORD FOR ACCESS GROUP                      
OCRIPRNT EQU   X'08'               RECORD IS A PARENT                           
OCRIREAD EQU   X'01'               READ RECORD                                  
*                                                                               
FCRINDS  DS    XL1                 * FIELD CONTROL RECORD INDICATORS *          
FCRIAGY  EQU   X'80'               MAIN AGENCY DEFAULT RECORD                   
FCRISIBL EQU   X'40'               RECORD IS SIBLING OF LAST RECORD             
FCRIPAGY EQU   X'20'               RECORD'S PARENT IS AGENCY RECORD             
FCRIACGP EQU   X'10'               RECORD FOR ACCESS GROUP                      
FCRIPRNT EQU   X'08'               RECORD IS A PARENT                           
FCRIREAD EQU   X'01'               READ RECORD                                  
*                                                                               
DSPINDS  DS    XL1                 * DISPLAY INDICATORS *                       
DSPIKEEP EQU   X'80'               KEEP CURRENT DISPLAY FOR NEXT INPUT          
DSPIDONE EQU   X'40'               USER HAS FINISHED INPUT                      
DSPIINP  EQU   X'20'               USER HAS INPUT TO SUB-SCREEN                 
DSPIDSP  EQU   X'10'               DISPLAY FOR DISPLAY (NOT ADD/CHANGE)         
DSPIACTC EQU   X'08'               ACTION HAS CHANGED                           
DSPINONE EQU   X'04'               NO RECORDS TO BE DISPLAYED                   
*                                                                               
SUBSCRN  DS    XL1                 CURRENT DISPLAYED SUB-SCREEN                 
SUBSNULL EQU   0                   NO SUB-SCREEN DISPLAYED                      
SUBSRCDS EQU   X'E1'               RECORD TYPES SUB-SCREEN                      
SUBSACTS EQU   X'E2'               ACTION CODES SUB-SCREEN                      
*                                                                               
SAVCLRL  EQU   *-SAVOVER           CLEAR TWA UP TO HERE (FOR SETTWA)            
*                                                                               
BITTABLE DS    XL32                BIT TABLE (WORK AREA)                        
VALID    DS    XL32                CURRENT VALID ACTIONS                        
AVAIL    DS    XL32                CURRENT AVAILABLE ACTIONS                    
*                                                                               
VALIDTAB DS    XL32                BIT TABLE OF VALID OPTIONS                   
PRNTVAL  DS    XL32                PARENT BIT TABLE                             
*                                                                               
WRITETAB DS    XL32                BIT TABLE OF VALID WRITE FIELDS              
READTAB  DS    XL32                BIT TABLE OF VALID READ FIELDS               
AVAILWRT DS    XL32                BIT TABLE OF AVAILABLE WRITES                
AVAILRD  DS    XL32                BIT TABLE OF AVAILABLE READS                 
*                                                                               
SUBSTAB  DS    CL2                 SUB-SCREEN TAB TO NEXT PAGE                  
*                                                                               
SAVSEL   DS    XL(SELKLN)                                                       
*                                                                               
ATABNTRY DS    H                   DISPLACEMENT TO ACTION TABLE ENTRY           
RTABNTRY DS    H                   DISPLACEMENT TO RECORD TABLE ENTRY           
OTABNTRY DS    H                   DISPLACEMENT TO OPTION TABLE ENTRY           
FTABNTRY DS    H                   DISPLACEMENT TO FIELD TABLE ENTRY            
*                                                                               
RRFLAG   DS    X                   REPEATING RECORD FLAG                        
RRLOOPQ  EQU   X'80'               . REPEATING RECORDS                          
RRLRDQ   EQU   X'40'               . RECORDS DISPLAYED                          
RRLODQ   EQU   X'20'               . OPTIONS DISPLAYED                          
RRLADQ   EQU   X'10'               . ACTIONS DISPLAYED                          
RRACTSQ  EQU   X'08'               . FILTERED ACTION DISPLAYED                  
RRFACTQ  EQU   X'04'               . FILTERED ACTION FOUND                      
*                                                                               
RNOPAGES DS    XL1                 NUMBER OF PAGES OF RECORD LIST               
RPAGENO  DS    XL1                 CURRENT RECORD LIST PAGE NUMBER              
*                                                                               
ANOPAGES DS    XL1                 NUMBER OF PAGES OF ACTION LIST               
APAGENO  DS    XL1                 CURRENT ACTION LIST PAGE NUMBER              
*                                                                               
RSAPGMEL DS    XL(SAPGMLNQ)        SAVED PROGRAM ELEMENT                        
RSADICEL DS    XL(SADICLNQ)        SAVED PROGRAM ELEMENT                        
*                                                                               
RBITEND  DS    H                   DISPLACEMENT TO END OF BIT TABLES            
RBITLEN  DS    H                   LENGTH OF BIT TABLES                         
RBITXLEN DS    XL1                 EXECUTABLE LENGTH OF BIT TABLES              
         DS    0H                                                               
ATAB     DS    0XL(ATABL)          ACTION CODE TABLE AREA (SEE ATABD)           
         ORG   ATAB+(L'SAPGMACT+1)*ATABL                                        
         DS    0H                                                               
RTAB     DS    0XL(RTABL)          RECORD TYPE TABLE AREA (SEE RTABD)           
         ORG   RTAB+256*RTABL                                                   
         DS    0H                                                               
OTAB     DS    0XL(OTABL)          OPTION TABLE AREA (SEE OTABD)                
         ORG   OTAB+256*OTABL                                                   
         DS    0H                                                               
FTAB     DS    0XL(FTABL)          FIELD TABLE AREA (SEE FTABD)                 
         ORG   FTAB+256*FTABL                                                   
*                                                                               
RBITS    DS    0X                  ACTION BIT TABLE LIST                        
         ORG   SAVOVER+SAVOVERL-1                                               
RBITSX   EQU   *                                                                
                                                                                
***********************************************************************         
* DSECT TO COVER LOCAL W/S                                                      
***********************************************************************         
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
*                                                                               
DUB      DS    D                                                                
BYTE     DS    X                                                                
PARM     DS    6A                                                               
GTPARM   DS    6A                                                               
*                                                                               
ARTAB    DS    A                   A(RECORD TABLE)                              
AATAB    DS    A                   A(ACTION TABLE)                              
AOTAB    DS    A                   A(OPTION TABLE)                              
AFTAB    DS    A                   A(FIELD TABLE)                               
ARTABX   DS    A                   A(END OF RECORD TABLE)                       
AGCTBL   DS    A                   A(GRIDS COLUMN TABLE)                        
*                                                                               
WORK     DS    XL80                                                             
WORKKEY  DS    XL(L'SAASKEY)                                                    
WORKBITT DS    XL32                                                             
*                                                                               
WAGY     DS    CL2                 EBCIDIC AGENCY CODE                          
GCOSEL   DS    CL1                 GRIDS COLUMN SELECTOR                        
*                                                                               
*                                  * DATA FOR SUB-SCREEN MESSAGE *              
SUBMSGNO DS    XL2                 SUB-SCREEN MESSAGE NUMBER                    
SUBMTYP  DS    XL1                 SUB-SCREEN MESSAGE TYPE                      
SUBXTRA  DS    XL40                TEXT TO ADD TO SUB-SCREEN MESSAGE            
SUBXINDS DS    XL1                 EXTRA TEXT INDICATORS                        
SUBXIMID EQU   X'80'                 SUBXTRA IN MIDDLE, NOT AFTER TEXT          
SUBTEXT  DS    XL(L'DSAMSG)        SUB-SCREEN TEXT (IF SET BY USER)             
*                                                                               
INDICS   DS    XL1                 * INDICATORS *                               
INDIPRCD EQU   X'80'               PRINTING DONE FOR RECORD                     
*                                                                               
PRTVAL   DS    CL1                 VALIDITY OF ACTION ON REPORT                 
*                                                                               
SELKEY   DS    0XL25               * SELECTS *                                  
SELOVPG  DS    0XL2                                                             
SELOVS   DS    XL1                 SELECT SYSTEM                                
SELPGM   DS    XL1                 SELECT PROGRAM                               
SELUID   DS    XL2                 SELECT USER-ID                               
SELAGN   DS    XL2                 SELECT ACCESS GROUP NUMBER                   
SELVAL   DS    CL1                 VALIDATION FILTER                            
         ORG   SELKEY+L'SELKEY                                                  
SELRECL  DS    X                   SELECT RECORD LENGTH                         
SELREC   DS    CL8                 SELECT RECORD                                
SELACTL  DS    X                   SELECT RECORD LENGTH                         
SELACT   DS    CL8                 SELECT RECORD                                
SELFOR   DS    C                   SELECT FORMAT                                
SELKLN   EQU   *-SELKEY                                                         
*                                                                               
GVALS    DS    0X                                                               
GVSYS    DS    CL7                 SYSTEM                                       
GVPGM    DS    CL7                 PROGRAM                                      
GVUID    DS    CL8                 USER-ID                                      
GVSEG    DS    CL9                 SECURITY GROUP                               
GVSEGN   DS    CL36                SECURITY GROUP NAME                          
GVLN1Q   EQU   *-GVALS                                                          
GVALS2   DS    0X                                                               
GVREC    DS    CL10                RECORD                                       
GVACTS   DS    CL200               ACTIONS                                      
GVOPT    DS    CL20                OPTION                                       
GVFLD    DS    CL20                FIELD                                        
GVLN2Q   EQU   *-GVALS2                                                         
GVLNQ    EQU   *-GVALS                                                          
*                                                                               
GDDSLIST DS    0H                                                               
CT@CLPSE DS    CL10                                                             
CT@EXPND DS    CL10                                                             
*                                                                               
LOCALX   EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SEACS09   01/25/18'                                      
         END                                                                    
