*          DATA SET SEACS0E    AT LEVEL 005 AS OF 10/27/11                      
*PHASE TA0D0EA                                                                  
ACS0E    TITLE '- SECURITY ACCESS - OPTION CONTROL RECORDS'                     
ACS0E    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACSE**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         LA    RE,SAVOVER                                                       
         LA    RE,OTAB-SAVOVER(RE)                                              
         ST    RE,AOTAB            SAVE A(OPTION TABLE)                         
         SPACE 1                                                                
         XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     DELRES              05 - APMDELR                                 
         B     DELRES              06 - APMRESR                                 
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
         SPACE 1                                                                
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF OPTION CONTROL RECORD                    *         
*  - VALIDATE INPUT FIELDS & INITIALIZE RECORD KEY                    *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   MVI   DSPINDS,0           CLEAR DISPLAY INDICATORS                     
         SPACE 1                                                                
         LA    R2,SAVKEY                                                        
         USING SAOCREC,R2          R2=A(RECORD KEY)                             
         SPACE 1                                                                
         CLC   APACTN,SAVAPACT     IF ACTION CHANGED FROM CHANGE                
         BE    VKEY2                 RECORD SHOULD BE RE-READ                   
         CLI   SAVAPACT,ACTCHA                                                  
         MVC   SAVAPACT,APACTN                                                  
         BNE   *+8                                                              
         OI    OCRINDS,OCRIREAD                                                 
         SPACE 1                                                                
VKEY2    MVC   WAGY,CUAALF         GET AGENCY EBCIDIC CODE                      
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   WAGY,OPTAGY                                                      
         CLC   SAOCAGY,WAGY        TEST CHANGE OF AGENCY                        
         BNE   VKEY4                                                            
         SPACE 1                                                                
         TM    DSPSYSH+FHIID,FHIIVA   TEST ALL FIELDS VALIDATED                 
         BZ    VKEY4                                                            
         TM    DSPPGMH+FHIID,FHIIVA                                             
         BZ    VKEY4                                                            
         TM    DSPUIDH+FHIID,FHIIVA                                             
         BZ    VKEY4                                                            
         TM    DSPACGH+FHIID,FHIIVA                                             
         BO    VKEY12                                                           
         SPACE 1                                                                
VKEY4    XC    DSPPGMD,DSPPGMD     CLEAR & TRANSMIT DESCRIPTIONS                
         XC    DSPUIDD,DSPUIDD                                                  
         XC    DSPACGD,DSPACGD                                                  
         OI    DSPPGMDH+FHOID,FHOITR                                            
         OI    DSPUIDDH+FHOID,FHOITR                                            
         OI    DSPACGDH+FHOID,FHOITR                                            
         SPACE 1                                                                
         MVC   WORKKEY,SAOCKEY     SAVE OLD RECORD KEY                          
         XC    SAOCKEY,SAOCKEY     BUILD NEW RECORD KEY                         
         MVI   SAOCTYP,SAOCTYPQ                                                 
         MVI   SAOCSUB,SAOCSUBQ                                                 
         MVC   SAOCAGY,WAGY                                                     
         SPACE 1                                                                
         GOTO1 AVALOVPG,PARM,DSPSYSH,(0,DSPPGMH),DSPPGMDH                       
         BNE   VALKEYN             VALIDATE SYSTEM/PROGRAM                      
         MVC   SAOCOVPG,APHALF                                                  
         MVC   SAVOVPG,APHALF                                                   
         SPACE 1                                                                
         CLI   DSPUIDH+FHILD,0     TEST USER-ID ENTERED                         
         BNE   VKEY6                                                            
         LA    R0,DSPUIDH                                                       
         ST    R0,FVADDR                                                        
         GOTO1 ATSTUID,SAOCUID     TEST 'ALL' USER-ID VALID                     
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
         MVC   SAOCUID,APHALF                                                   
         MVC   DSPUIDD,APWORK                                                   
*                                                                               
VKEY8    CLI   DSPACGH+FHILD,0     TEST ACCESS GROUP ENTERED                    
         BE    VKEY10                                                           
         GOTO1 AVALACG,DSPACGH     TEST ACCESS GROUP                            
         BNE   VALKEYN                                                          
         MVC   SAOCAGN,APHALF                                                   
         MVC   DSPACGD,APWORK                                                   
         SPACE 1                                                                
VKEY10   CLC   SAOCKEY,WORKKEY     TEST CHANGE OF KEY                           
         BE    *+8                                                              
         OI    OCRINDS,OCRIREAD                                                 
         SPACE 1                                                                
VKEY12   CLI   APACTN,ACTDIS       IF ACTION IS DISPLAY                         
         BE    VKEY16                NO RESTRICTIONS                            
         SPACE 1                                                                
         OC    SAOCAGN,SAOCAGN     IF NO ACCESS GROUP                           
         BNZ   VKEY14                MUST BE DDS                                
         TM    CUSTAT,CUSDDS                                                    
         BO    VKEY16                                                           
         LA    RE,DSPUIDH                                                       
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(CE#CCIDL)                                           
         B     VALKEYN                                                          
VKEY14   GOTO1 ATSTGMAN,FVIFLD     TEST FOR GROUP MANAGER                       
         BNE   VALKEYN                                                          
         SPACE 1                                                                
VKEY16   TM    OCRINDS,OCRIREAD    TEST RECORD TO BE READ                       
         BZ    VALKEYY                                                          
         SPACE 1                                                                
         BAS   RE,INITOTAB         INITIALIZE OPTIONS TABLE                     
         BE    VKEY22                                                           
         LA    RE,DSPSYSH                                                       
         ST    RE,FVADDR                                                        
         B     VALKEYN                                                          
         EJECT                                                                  
***********************************************************************         
*  - READ/SET-UP OCONTROL RECORD                                      *         
***********************************************************************         
         SPACE 1                                                                
VKEY22   L     R2,AIOAREA1         READ OPTION CONTROL RECORD                   
         MVC   IOKEY,SAVKEY                                                     
         GOTO1 AIO,IORDD+IOCONFIL+IO1                                           
         BL    VALKEYN                                                          
         BE    VKEY26                                                           
         SPACE 1                                                                
         MVI   SAVAPIND,APIOKDIS+APIOKRES                                       
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BO    VKEY28                                                           
         SPACE 1                                                                
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
         OI    OCRINDS,OCRIREAD                                                 
         SPACE 1                                                                
VKEY24   TM    OCRINDS,OCRISIBL    IF SAME PARENT                               
         BO    VKEY40                CURRENT DISPLAY IS OKAY                    
         MVC   VALIDTAB,PRNTVAL    SET ALL VALID                                
         B     VKEY30                                                           
         SPACE 1                                                                
VKEY26   MVI   SAVAPIND,APIOKDIS+APIOKCHA+APIOKDEL                              
         TM    OCRINDS,OCRIACGP    IF ACCESS GROUP DEFINED                      
         BO    VKEY28                RECORD IS NOT A PARENT                     
         SPACE 1                                                                
         GOTO1 AIO,IOSQ+IOCONFIL+IO2  TEST NEXT RECORD ON FILE                  
         L     RF,AIOAREA2               IS CHILD OF RECORD                     
         TM    OCRINDS,OCRIAGY                                                  
         BZ    *+14                                                             
         CLC   SAOCKEY(SAOCUID-SAOCKEY),0(RF)                                   
         B     *+10                                                             
         CLC   SAOCKEY(SAOCAGN-SAOCKEY),0(RF)                                   
         BNE   VKEY28                                                           
         OI    OCRINDS,OCRIPRNT    RECORD IS A PARENT                           
         NI    SAVAPIND,FF-APIOKDEL  AND SO CANNOT BE DELETED                   
         SPACE 1                                                                
VKEY28   GOTO1 FINDEL,SAOCTELQ     BUILD BIT TABLE                              
         GOTO1 ABLDBITT,PARM,(R3),VALIDTAB                                      
         NC    VALIDTAB,PRNTVAL                                                 
         SPACE 1                                                                
         GOTO1 SAVEACT,SAOCREC     SAVE ACTIVITY ELEMENT INFO                   
         MVC   FVXTRA,SAVDATE                                                   
         EJECT                                                                  
***********************************************************************         
*  - INITIALIZE DISPLAY                                               *         
***********************************************************************         
         SPACE 1                                                                
VKEY30   BAS   RE,DISOPTS          DISPLAY OPTIONS                              
         MVCDD SUBXTRA,CT#SAVIA                                                 
         BAS   RE,SUBMSG                                                        
         OI    DSPINDS,DSPIKEEP    IGNORE INPUT FOR 1ST DISPLAY                 
         SPACE 1                                                                
VKEY40   TM    DSPINDS,DSPINONE    CAN'T ADD IF NO OPTIONS                      
         BZ    VKEY42                                                           
         CLI   APACTN,ACTADD                                                    
         BNE   VKEY42                                                           
         MVC   FVMSGNO,=AL2(CE#NOPTL)                                           
         LA    RE,DSPSYSH                                                       
         ST    RE,APCURSOR                                                      
         B     VALKEYN                                                          
VKEY42   OI    DSPSYSH+FHIID,FHIIVA  SET ALL FIELDS VALIDATED                   
         OI    DSPPGMH+FHIID,FHIIVA                                             
         OI    DSPUIDH+FHIID,FHIIVA                                             
         OI    DSPACGH+FHIID,FHIIVA                                             
         SPACE 1                                                                
VALKEYY  MVC   APINDS,SAVAPIND                                                  
         MVC   APRECKEY(L'SAVKEY),SAVKEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO  SET SCREEN MODIFIED                 
         B     XIT                                                              
         SPACE 1                                                                
VALKEYN  XC    SAVKEY,SAVKEY       CLEAR SAVED KEY VALUES                       
         XC    PRNTKEY,PRNTKEY                                                  
         NI    DSPSYSH+FHIID,FF-FHIIVA   ENSURE A KEY FIELD                     
         B     XIT                         IS UNVALIDATED                       
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE OCONTROL RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
VALREC   BAS   RE,OPTSINP          VALIDATE OPTIONS ON SCREEN                   
         BE    VREC2                                                            
         SPACE 1                                                                
         MVI   FVOMTYP,GTMINF      DISPLAY 'ENTER FIELDS' FOR AN ADD            
         MVC   FVMSGNO,=AL2(GI#ENTFD)                                           
         CLI   APACTN,ACTADD                                                    
         BE    VALRECX                                                          
         SPACE 1                                                                
         MVC   FVXTRA,SAVDATE      OR 'ENTER CHANGES' FOR A CHANGE              
         MVC   FVMSGNO,=AL2(GI#ENTCH)                                           
         B     VALRECX                                                          
         SPACE 1                                                                
VREC2    L     R2,AIOAREA1                                                      
         USING SAOCREC,R2          R2=A(OCONTROL RECORD)                        
         MVC   SAOCKEY,SAVKEY      SET UP ELEMENTLESS RECORD                    
         LA    R0,SAOCDATA-SAOCREC+1                                            
         STCM  R0,3,SAOCLEN                                                     
         MVI   SAOCSTAT,0                                                       
         MVI   SAOCDATA,0                                                       
         SPACE 1                                                                
         LA    R3,APELEM           ADD OCONTROL ELEMENT                         
         USING SAOCTD,R3                                                        
         XC    SAOCTD(SAOCTLNQ),SAOCTD                                          
         MVI   SAOCTEL,SAOCTELQ                                                 
         GOTO1 ATAGBITT,PARM,SAOCTD,VALIDTAB                                    
         GOTO1 AADDELS,SAOCREC                                                  
         DROP  R3                                                               
         SPACE 1                                                                
         GOTO1 UPDTACT,PARM,AIOAREA2,SAOCREC                                    
         BNE   VALRECX                                                          
         SPACE 1                                                                
         LA    R1,IOPUT+IOCONFIL+IO1                                            
         CLI   APACTN,ACTADD       TEST RECORD BEING ADDED                      
         BNE   *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         SPACE 1                                                                
         GOTO1 AIO                 WRITE RECORD                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SAVAPIND,APIOKDIS+APIOKCHA+APIOKDEL                              
         SPACE 1                                                                
VALRECY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         SPACE 1                                                                
VALRECX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY OPTION CONTROL RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
DISREC   BAS   RE,OPTSINP          VALIDATE OPTIONS INPUT                       
         BE    DREC2                                                            
         OI    TWALSCTL,TWALSHLD+TWALSRTN   HOLD ON TO CURRENT SCREEN           
         B     DREC4                                                            
         SPACE 1                                                                
DREC2    TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BO    DISRECX                                                          
         SPACE 1                                                                
DREC4    MVC   FVXTRA,SAVDATE      DISPLAY ACTIVITY DATE                        
         SPACE 1                                                                
DISRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE/RESTORE RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DELRES   BAS   RE,OPTSINP          VALIDATE OPTIONS INPUT                       
         BE    DELRES2                                                          
         SPACE 1                                                                
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI#PETDE)  'PRESS ENTER TO DELETE'                  
         CLI   APACTN,ACTRES                                                    
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(GI#PETRE)  OR '.... TO RESTORE'                     
         MVC   FVXTRA,SAVDATE                                                   
         B     DELRESX                                                          
         SPACE 1                                                                
DELRES2  L     R2,AIOAREA1         UPDATE ACTIVITY ELEMENT                      
         USING SAOCREC,R2                                                       
         GOTO1 UPDTACT,PARM,SAOCREC,SAOCREC                                     
         BNE   DELRESX                                                          
         SPACE 1                                                                
         XI    SAOCSTAT,X'80'      CHANGE DELETE FLAG IN RECORD                 
         GOTO1 AIO,IOPUT+IOCONFIL+IO1 WRITE RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         MVI   SAVAPIND,APIOKRES+APIOKDIS  RESET APINDS                         
         CLI   APACTN,ACTRES                                                    
         BNE   *+8                                                              
         MVI   SAVAPIND,APIOKDIS+APIOKDEL+APIOKCHA                              
         SPACE 1                                                                
DELRESX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE ACTIVITY ELEMENT                                  *         
*                                                                     *         
* NTRY: P1=A(IO AREA TO RE-READ RECORD)                               *         
*       P2=A(IO AREA OF RECORD TO BE UPDATED)                         *         
* EXIT: CC=NOT EQUAL IF RECORD OR IT'S PARENT                         *         
*                                   HAVE BEEN CHANGED SINCE LAST READ *         
***********************************************************************         
         SPACE 1                                                                
UPDTACT  NTR1  ,                                                                
         SPACE 1                                                                
         LM    R2,R3,0(R1)                                                      
         USING SAOCREC,R2                                                       
         LA    R8,ACTEL                                                         
         USING SAACVD,R8           R8=A(ACTIVITY ELEMENT)                       
         SPACE 1                                                                
UACT2    MVC   IOKEY,SAVKEY        RE-READ RECORD                               
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORDUPD+IOCONFIL                                             
         BE    UACT4                                                            
         TM    IOERR,IOEDEL                                                     
         BO    UACT4                                                            
         XC    SAACVSEQ,SAACVSEQ   CLEAR SEQUENCE NUMBER                        
         CLI   APACTN,ACTADD       IF RECORD NOT ON FILE ACTION                 
         BE    UACT10                MUST BE ADD                                
         DC    H'0'                                                             
         SPACE 1                                                                
UACT4    CLI   APACTN,ACTADD       TEST ACTION IS ADD                           
         BNE   UACT6                                                            
         MVC   FVMSGNO,=AL2(CE#RDADE)  'RECORD ADDED ELSEWHERE'                 
         B     UPDTACTN                                                         
         SPACE 1                                                                
UACT6    GOTO1 AGETACT,SAOCREC     TEST SEQUENCE NUMBER HAS NOT CHANGED         
         CLC   SAVSEQNO,SAACVSEQ                                                
         BE    UACT10                                                           
         MVC   FVMSGNO,=AL2(CE#RDCHE)  'RECORD CHANGED ELSEWHERE'               
         B     UPDTACTN                                                         
         SPACE 1                                                                
UACT10   GOTO1 ASETACT,(R3)        SET NEW ACTIVITY ELEMENT                     
         GOTO1 SAVEACT             SAVE NEW ACTIVITY ELEMENT DETAILS            
         SPACE 1                                                                
UPDTACTY CR    RB,RB               EXIT WITH CC=EQUAL                           
         B     XIT                                                              
         SPACE 1                                                                
UPDTACTN OI    OCRINDS,OCRIREAD    EXIT WITH CC=NOT EQUAL                       
         B     XIT                   & FORCE RE-READING OF RECORD               
         DROP  R2,R8                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SAVE ACTIVITY ELEMENT DETAILS                            *         
*                                                                     *         
* NTRY: R1=A(OCONTROL RECORD)                                         *         
***********************************************************************         
         SPACE 1                                                                
SAVEACT  NTR1  ,                                                                
         GOTO1 ADISACT                                                          
         MVC   SAVSEQNO,ACTEL+(SAACVSEQ-SAACVD)                                 
         MVC   SAVDATE,FVXTRA                                                   
         XC    FVXTRA,FVXTRA                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VERIFY INPUT FOR OPTIONS SUB-SCREEN                      *         
*                                                                     *         
* EXIT: CC=EQUAL IF DISPLAY IS 'FINISHED'                             *         
***********************************************************************         
         SPACE 1                                                                
OPTSINP  NTR1  ,                                                                
         TM    DSPINDS,DSPINONE    TEST ANYTHING TO DISPLAY                     
         BZ    OINP02                                                           
         OI    DSPINDS,DSPIDONE                                                 
         LA    RE,ACSACTH                                                       
         ST    RE,APCURSOR                                                      
         B     OPTSINPY                                                         
         SPACE 1                                                                
OINP02   TM    DSPINDS,DSPIKEEP    TEST KEEP CURRENT DISPLAY                    
         BO    OINPX2                                                           
         SPACE 1                                                                
         CLI   APMODE,APMVALR                                                   
         BE    *+8                                                              
         OI    DSPINDS,DSPIDSP     DISPLAY JUST FOR DISPLAY                     
         SPACE 1                                                                
         XR    RF,RF                                                            
         IC    RF,APPFKEY                                                       
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     PFENTER             ENTER KEY                                    
         B     PFHELP              PFK01 = HELP                                 
         B     PFHELPX             PFK02 = HELP+                                
         B     PFUNDF              PFK03 = UNDEFINED                            
         B     PFUNDF              PFK04 = UNDEFINED                            
         B     PFYES               PFK05 = YES                                  
         B     PFYESX              PFK06 = YES+                                 
         B     PFNO                PFK07 = NO                                   
         B     PFNOX               PFK08 = NO+                                  
         B     PFBACK              PFK09 = SCROLL BACK                          
         B     PFFRWD              PFK10 = SCROLL FORWARD                       
         B     PFUNDF              PFK11 = UNDEFINED                            
         B     PFUNDF              PFK12 = UNDEFINED                            
         SPACE 1                                                                
OPTSINPX BAS   RE,SUBMSG           DISPLAY SUB-SCREEN MESSAGE                   
         SPACE 1                                                                
         TM    DSPINDS,DSPIDONE    TEST DISPLAY FINISHED                        
         BZ    OINPX2                                                           
         XC    APCURSOR,APCURSOR   RESET CURSOR POSN.                           
OPTSINPY CR    RB,RB               SET CC=EQUAL                                 
         B     XIT                                                              
         SPACE 1                                                                
OINPX2   OC    APCURSOR,APCURSOR   TEST CURSOR SET                              
         BNZ   *+12                                                             
         LA    R0,DSPINPH          SET CURSOR                                   
         ST    R0,APCURSOR           TO FIRST OPTION INPUT FIELD                
         SPACE 1                                                                
OPTSINPN LTR   RB,RB               SET CC=NOT EQUAL                             
         B     XIT                                                              
         EJECT                                                                  
PFENTER  BAS   RE,VALOPTS          * ENTER KEY *                                
         BNE   OPTSINPX                                                         
         SPACE 1                                                                
         TM    DSPINDS,DSPIDSP+DSPIINP     TEST ACTION IS 'DISPLAY'             
         BO    *+8                            & INPUT TO SUB-SCREEN             
         OI    DSPINDS,DSPIDONE    SET DISPLAY 'DONE'                           
         B     OPTSINPX                                                         
         SPACE 3                                                                
PFUNDF   XR    R0,R0               * UNDEFINED PF KEY *                         
         IC    R0,APPFKEY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SUBXTRA(2),DUB                                                   
         OI    SUBXINDS,SUBXIMID                                                
         MVC   SUBMSGNO,=AL2(CE#PFUND)                                          
         B     OPTSINPX                                                         
         SPACE 3                                                                
PFHELP   LA    R1,INPHELP          * HELP *                                     
         B     PFCURS                                                           
         SPACE 1                                                                
PFHELPX  LA    R1,INPHELPX         * HELP+ *                                    
         B     PFCURS                                                           
         SPACE 1                                                                
PFYESX   LA    R1,INPYESX          * YES+ *                                     
         B     PFCURS                                                           
         SPACE 1                                                                
PFNOX    LA    R1,INPNOX           * NO+ *                                      
         SPACE 1                                                                
PFCURS   BAS   RE,SETCURS                                                       
         BNE   OPTSINPX                                                         
         BAS   RE,VALOPTS                                                       
         B     OPTSINPX                                                         
         EJECT                                                                  
PFYES    LA    R1,INPYES           * YES *                                      
         B     *+8                                                              
PFNO     LA    R1,INPNO            * NO *                                       
         BAS   RE,SETCURS                                                       
         BNE   OPTSINPX                                                         
         SPACE 1                                                                
         L     RF,APCURSOR         TEST CURSOR ON LAST INPUT FIELD              
         LA    R0,DSPINPLH                                                      
         CR    RF,R0                                                            
         BE    PFFRWD              SCROLL FORWARD IF NECCESSERY                 
         BAS   RE,VALOPTS                                                       
         BNE   OPTSINPX                                                         
         LA    RF,OPTINPLQ(RF)     TAB CURSOR TO NEXT INPUT FIELD               
         TM    FHATD(RF),FHATPR      (UNLESS LAST ONE)                          
         BO    *+8                                                              
         ST    RF,APCURSOR                                                      
         B     OPTSINPX                                                         
         SPACE 3                                                                
PFBACK   BAS   RE,VALOPTS          * SCROLL BACK *                              
         BNE   OPTSINPX                                                         
         SPACE 1                                                                
         XR    RE,RE                                                            
         IC    RE,PAGENO                                                        
         BCT   RE,*+14             TEST CAN DO                                  
         MVCDD SUBXTRA,CT#CTSBK                                                 
         B     OPTSINPX                                                         
         SPACE 1                                                                
         STC   RE,PAGENO                                                        
         MVCDD SUBXTRA,CT#SCDBK                                                 
         BAS   RE,DISOPTS                                                       
         XC    APCURSOR,APCURSOR                                                
         B     OPTSINPX                                                         
         SPACE 3                                                                
PFFRWD   BAS   RE,VALOPTS          * SCROLL FORWARD *                           
         BNE   OPTSINPX                                                         
         SPACE 1                                                                
         IC    RE,PAGENO                                                        
         CLM   RE,1,NUMPAGES       TEST CAN DO                                  
         BNE   *+14                                                             
         MVCDD SUBXTRA,CT#CTSFD                                                 
         B     OPTSINPX                                                         
         SPACE 1                                                                
         LA    RE,1(RE)                                                         
         STC   RE,PAGENO                                                        
         MVCDD SUBXTRA,CT#SCDFD                                                 
         BAS   RE,DISOPTS                                                       
         XC    APCURSOR,APCURSOR                                                
         B     OPTSINPX                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE OPTION INPUT FIELDS                             *         
*                                                                     *         
* EXIT: CC=EQUAL IF ALL FIELDS HAVE BEEN VALIDATED                    *         
***********************************************************************         
         SPACE 1                                                                
VALOPTS  NTR1  ,                                                                
         XR    R8,R8                                                            
         IC    R8,PAGENO                                                        
         MH    R8,=Y(OTABPAGE)                                                  
         LA    R8,OTAB-OTABPAGE(R8)                                             
         USING OTABD,R8            R8=A(OPTION TABLE ENTRY)                     
         SPACE 1                                                                
         LA    R2,OPTINPLQ         R2=L(OPTION/INPUT FIELDS)                    
         LA    R3,DSPINPLH         R3=A(LAST INPUT FIELD)                       
         LA    R9,DSPINPH                                                       
         USING FHD,R9              R9=A(FIRST INPUT FIELD)                      
         SPACE 1                                                                
VOPTS2   TM    FHII,FHIIVA         TEST FIELD PREVIOUSLY VALIDATED              
         BO    VOPTS8                                                           
         ST    R9,APCURSOR                                                      
         OI    DSPINDS,DSPIINP                                                  
         SPACE 1                                                                
         CLC   FHDA(1),INPHELP                                                  
         BE    VOPTHELP                                                         
         CLC   FHDA(1),INPYES                                                   
         BE    VOPTYES                                                          
         CLC   FHDA(1),INPNO                                                    
         BE    VOPTNO                                                           
         B     VOPTUNDF                                                         
         SPACE 1                                                                
VOPTS6   GOTO1 DISOINP,PARM,FHD,OTABD                                           
         SPACE 1                                                                
VOPTS8   LA    R8,OTABL(R8)                                                     
         CLI   OTABD,EOT           TEST E-O-T                                   
         BE    *+8                                                              
         BXLE  R9,R2,VOPTS2                                                     
         SPACE 1                                                                
         ICM   R0,3,SUBSTAB        TEST TAB TO NEXT PAGE                        
         BZ    VALOPTSY                                                         
         SPACE 1                                                                
         IC    RE,PAGENO           SCROLL FORWARD                               
         LA    RE,1(RE)                                                         
         STC   RE,PAGENO                                                        
         BAS   RE,DISOPTS                                                       
         SPACE 1                                                                
         STCM  R0,3,DSPINP         SET FIRST INPUT OF SCREEN TO TAB             
         NI    DSPINPH+FHIID,FF-FHIIVA                                          
         BAS   RE,VALOPTS          VALIDATE NEW SCREEN                          
         SPACE 1                                                                
VALOPTSN LTR   RB,RB               CC=NOT EQUAL                                 
         B     XIT                                                              
VALOPTSY CR    RB,RB               CC=EQUAL FOR ALL FIELDS VALIDATED            
         B     XIT                                                              
         EJECT                                                                  
VOPTUNDF CLI   FHDA,C' '           * INVALID INPUT *                            
         BH    VOUNDF2             TEST FIELD EMPTY                             
         TM    DSPINDS,DSPIDSP     TEST ACTION IS DISPLAY                       
         BO    VOPTS6                                                           
         MVC   SUBMSGNO,=AL2(FVFNONE)                                           
         B     *+10                                                             
VOUNDF2  MVC   SUBMSGNO,=AL2(FVFNOTV)                                           
         MVCDD SUBXTRA,CT#SAVIA                                                 
         B     VALOPTSN                                                         
         SPACE 3                                                                
VOPTYES  TM    DSPINDS,DSPIDSP     * YES *                                      
         BO    VOPTS6                                                           
         CLC   INPYESX,FHDA                                                     
         BE    VOPTYESX                                                         
         GOTO1 SETBIT,PARM,(OTCODE,VALIDTAB)                                    
         B     VOPTS6                                                           
         SPACE 1                                                                
VOPTYESX GOTO1 SETBIT,PARM,(OTCODE,VALIDTAB) * YES+ *                           
         LA    R8,OTABL(R8)                                                     
         CLI   OTABD,EOT                                                        
         BNE   VOPTYESX                                                         
         SPACE 1                                                                
         BAS   RE,DISOPTS                                                       
         B     VALOPTSN                                                         
         SPACE 3                                                                
VOPTNO   TM    DSPINDS,DSPIDSP     * NO *                                       
         BO    VOPTS6                                                           
         CLC   INPNOX,FHDA                                                      
         BE    VOPTNOX                                                          
         GOTO1 RESETBIT,PARM,(OTCODE,VALIDTAB)                                  
         B     VOPTS6                                                           
         SPACE 1                                                                
VOPTNOX  GOTO1 RESETBIT,PARM,(OTCODE,VALIDTAB)  * NO+ *                         
         LA    R8,OTABL(R8)                                                     
         CLI   OTABD,EOT                                                        
         BNE   VOPTNOX                                                          
         SPACE 1                                                                
         BAS   RE,DISOPTS                                                       
         B     VALOPTSN                                                         
         EJECT                                                                  
VOPTHELP GOTO1 DISHELP,OTABD       * HELP *                                     
         SPACE 1                                                                
         CLC   INPHELPX,FHDA       TEST HELP+                                   
         BNE   VOPTH4                                                           
         CLI   OTABD+OTABL,EOT     TEST LAST OPTION                             
         BE    VOPTH4                                                           
         CR    R9,R3               TEST LAST FOR SCREEN                         
         BNE   VOPTH2                                                           
         MVC   SUBSTAB,INPHELPX    SET TAB TO NEXT SCREEN                       
         B     VOPTH4                                                           
         SPACE 1                                                                
VOPTH2   MVC   FHDA+OPTINPLQ(L'INPHELPX),INPHELPX  BUMP HELP+ TO                
         NI    FHII+OPTINPLQ,FF-FHIIVA               NEXT INPUT                 
         OI    FHOI+OPTINPLQ,FHOITR                                             
         SPACE 1                                                                
VOPTH4   GOTO1 DISOINP,PARM,FHD,OTABD                                           
         B     VALOPTSN                                                         
         SPACE 1                                                                
         DROP  R8,R9                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY OPTIONS LIST                                     *         
***********************************************************************         
         SPACE 1                                                                
DISOPTS  NTR1  ,                                                                
         XC    SUBSTAB,SUBSTAB     CLEAR TAB TO NEXT SCREEN                     
         SPACE 1                                                                
         MVC   DSPPAGE(1),PAGENO   DISPLAY PAGE NUMBER INFO.                    
         OI    DSPPAGE,C'0'                                                     
         MVC   DSPPAGE+2(1),NUMPAGES                                            
         OI    DSPPAGE+2,C'0'                                                   
         OI    DSPPAGEH+FHOID,FHOITR                                            
         SPACE 1                                                                
         XR    R8,R8                                                            
         IC    R8,PAGENO                                                        
         MH    R8,=Y(OTABPAGE)                                                  
         LA    R8,OTAB-OTABPAGE(R8)                                             
         USING OTABD,R8            R8=A(OPTION TABLE ENTRY)                     
         SPACE 1                                                                
         LA    R2,OPTINPLQ         R2=L(OPTION/INPUT FIELDS)                    
         LA    R3,DSPOPTLH         R3=A(LAST FIELD ON SCREEN)                   
         LA    R9,DSPOPTH                                                       
         USING DSPOPTH,R9          R9=A(FIRST FIELD ON DISPLAY)                 
         SPACE 1                                                                
DOPTS2   OI    DSPINPH+FHOID,FHOITR  TRANSMIT FIELDS                            
         OI    DSPOPTH+FHOID,FHOITR                                             
         SPACE 1                                                                
         CLI   OTABD,EOT            TEST ANY MORE OPTIONS ON SCREEN             
         BNE   DOPTS4                                                           
         XC    DSPINP,DSPINP       CLEAR                                        
         OI    DSPINPH+FHATD,FHATPR PROTECT                                     
         XC    DSPOPT,DSPOPT       CLEAR                                        
         B     DOPTS10                                                          
         SPACE 1                                                                
DOPTS4   GOTO1 ADISDIC,PARM,(SAVOVS,L'DSPOPT),(C'L',OTWRD)                      
         MVC   DSPOPT,APWORK       COPY OPTION WORD                             
         GOTO1 DISOINP,PARM,DSPOPTH,OTABD                                       
         NI    DSPINPH+FHATD,FF-FHATPR                                          
         SPACE 1                                                                
         LA    R8,OTABL(R8)        BUMP R8 TO NEXT TABLE ENTRY                  
         SPACE 1                                                                
DOPTS10  BXLE  R9,R2,DOPTS2        BUMP R9 TO NEXT OPTION FIELD                 
         SPACE 1                                                                
DISOPTSX B     XIT                                                              
         DROP  R8,R9                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY OPTION INPUT                                     *         
*                                                                     *         
* NTRY: P1=A(OPTION/INPUT FIELD)                                      *         
*       P2=A(OPTION TABLE ENTRY)                                      *         
***********************************************************************         
         SPACE 1                                                                
DISOINP  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
OPT      USING DSPOPTH,R2          R2=A(OPTION FIELD)                           
         USING OTABD,R3            R3=A(OPTION TABLE ENTRY)                     
         SPACE 1                                                                
         TM    OPT.DSPOPTH+FHATD,FHATPR                                         
         BO    *+8                 ADJUST R2 IF IS THE INPUT FIELD              
         SH    R2,=Y(DSPINPH-DSPOPTH)                                           
         SPACE 1                                                                
         OI    OPT.DSPOPTH+FHOID,FHOITR TRANSMIT                                
         OI    OPT.DSPINPH+FHOID,FHOITR                                         
         OI    OPT.DSPINPH+FHIID,FHIIVA SET INPUT VALIDATED                     
         SPACE 1                                                                
         GOTO1 TESTBIT,PARM,(OTCODE,VALIDTAB)                                   
         BZ    DFINP2              TEST OPTION IS VALID                         
         MVC   OPT.DSPINP,INPYES                                                
         OI    OPT.DSPOPTH+FHATD,FHATHI                                         
         OI    OPT.DSPINPH+FHATD,FHATHI                                         
         B     XIT                                                              
         SPACE 1                                                                
DFINP2   MVC   OPT.DSPINP,INPNO    OPTION IS NOT VALID                          
         NI    OPT.DSPOPTH+FHATD,FF-FHATHI                                      
         NI    OPT.DSPINPH+FHATD,FF-FHATHI                                      
         B     XIT                                                              
         SPACE 1                                                                
         DROP  OPT,R3                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET CURSOR INPUT FIELD                                   *         
*                                                                     *         
* NTRY: R1=A(OUTPUT FOR FIELD)                                        *         
* EXIT: CC=EQUAL IF CURSOR VALIDLY WITHIN OPTIONS SUB-SCREEN          *         
*       APCURSOR=A(INPUT FIELD)                                       *         
***********************************************************************         
         SPACE 1                                                                
SETCURS  NTR1  ,                                                                
         L     R3,AINP                                                          
         USING TIOBD,R3            TRANSLATOR I/O BLOCK                         
         SPACE 1                                                                
         XR    R2,R2                                                            
         ICM   R2,3,TIOBCURD                                                    
         LA    R2,TWAD(R2)         R2=A(FIELD OF CURSOR)                        
         USING FHD,R2                                                           
         BNZ   SCURS10                                                          
         SPACE 1                                                                
         LA    R2,ACSMSGH          CURSOR NOT WITHIN FIELD                      
         XR    RF,RF                                                            
SCURS2   CLC   FHAD,TIOBCURS       FIND NEAREST FIELD TO CURSOR                 
         BH    SCURS4                                                           
         ICM   RF,1,FHLN                                                        
         BZ    *+10                                                             
         BXH   R2,RF,SCURS2                                                     
SCURS4   SR    R2,RF                                                            
         DROP  R3                                                               
         SPACE 1                                                                
SCURS10  TM    FHAT,FHATPR         TEST FIELD PROTECTED                         
         BZ    SCURS12                                                          
         XR    RF,RF               BUMP R2 TO NEXT FIELD                        
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         TM    FHAT,FHATPR         TEST THIS FIELD PROTECTED                    
         BO    SETCURSN                                                         
         SPACE 1                                                                
SCURS12  LA    R0,DSPINPH          TEST CURSOR WITHIN SUB-SCREEN                
         CR    R2,R0                                                            
         BL    SETCURSN                                                         
         SPACE 1                                                                
         OI    FHOI,FHOITR         SET INPUT FIELD                              
         NI    FHII,FF-FHIIVA                                                   
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+1)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FHDA(0),0(R1)                                                    
         SPACE 1                                                                
SETCURSY ST    R2,APCURSOR         SAVE A(CURSOR INPUT FIELD)                   
         CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
SETCURSN MVC   SUBMSGNO,=AL2(CE#PCWIF)                                          
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY HELP FOR OPTION                                  *         
*                                                                     *         
* NTRY: R1=A(OPTION TABLE ENTRY)                                      *         
***********************************************************************         
         SPACE 1                                                                
DISHELP  NTR1  ,                                                                
         LR    R2,R1                                                            
         USING OTABD,R2            R2=A(OPTION ENTRY)                           
         XC    WORK,WORK                                                        
         SPACE 1                                                                
         GOTO1 ADISDIC,PARM,(SAVOVS,HELPWRDL),(C'U',OTWRD)                      
         MVC   WORK+1(HELPWRDL),APWORK                                          
         SPACE 1                                                                
         IC    RF,SAVOVS                                                        
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),OTDSC),OTWRD                                  
         SPACE 1                                                                
         LA    RF,WORK+HELPWRDL    COPY DESCRIPTION TO LEFT OF WORD             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(L'APWORK,RF),APWORK                                            
         SPACE 1                                                                
         MVC   SUBTEXT,WORK+1                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO DISPLAY SUB SCREEN MESSAGE                              *         
***********************************************************************         
         SPACE 1                                                                
SUBMSG   NTR1  ,                                                                
         OC    SUBTEXT,SUBTEXT     TEST TEXT SET                                
         BZ    SMSG2                                                            
         MVC   DSPMSG,SUBTEXT                                                   
         B     SMSG20                                                           
         SPACE 1                                                                
SMSG2    LA    R1,PARM                                                          
         USING GETTXTD,R1          R1=A(GETTXT PARAMETER LIST)                  
         XC    GTBLOCK,GTBLOCK                                                  
         SPACE 1                                                                
         OC    SUBMSGNO,SUBMSGNO   TEST MESSAGE NUMBER SET                      
         BNZ   SMSG4                                                            
         TM    DSPINDS,DSPINONE    SET DEFAULT NO OPTIONS MESSAGE               
         BZ    *+14                                                             
         MVC   GTMSGNO,=AL2(CE#NOPTL)                                           
         B     SMSG10                                                           
         MVI   GTMTYP,GTMINF       SET DEFAULT OPTIONS DISPLAYED MESAGE         
         MVC   GTMSGNO,=AL2(CI#OLDIS)                                           
         B     SMSG6                                                            
         SPACE 1                                                                
SMSG4    MVC   GTMTYP,SUBMTYP      SET MESSAGE TYPE AND NUMBER                  
         MVC   GTMSGNO,SUBMSGNO                                                 
         CLI   SUBMSGNO,FF         TEST GENERAL SYSTEM MESSAGE                  
         BNE   *+12                                                             
         MVI   GTMSGNO,0                                                        
         MVI   GTMSYS,FF                                                        
         SPACE 1                                                                
SMSG6    OC    SUBXTRA,SUBXTRA     TEST ANY TEXT TO TAG ON                      
         BZ    SMSG10                                                           
         TM    SUBXINDS,SUBXIMID   TEST TEXT ADDED IN MIDDLE                    
         BZ    SMSG8                                                            
         MVI   GTLTXT,L'SUBXTRA                                                 
         LA    RE,SUBXTRA                                                       
         STCM  RE,7,GTATXT                                                      
         B     SMSG10                                                           
         SPACE 1                                                                
SMSG8    MVI   WORK,C'-'           TEXT IS TO BE APPENDED                       
         MVI   WORK+1,C' '                                                      
         MVC   WORK+2(L'SUBXTRA),SUBXTRA                                        
         MVI   GTLTXT,L'SUBXTRA+2                                               
         LA    RE,WORK                                                          
         STCM  RE,7,GTATXT                                                      
         SPACE 1                                                                
SMSG10   LA    RE,DSPMSGH                                                       
         STCM  RE,7,GTAOUT                                                      
         GOTO1 VGETTXT                                                          
         DROP  R1                                                               
         SPACE 1                                                                
SMSG20   OI    DSPMSGH+FHOID,FHOITR  TRANSMIT MESSAGE                           
         SPACE 1                                                                
SUBMSGX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY         INITIALIZE RECORD KEY                        
         USING SAOCREC,R2                                                       
         XC    SAOCKEY,SAOCKEY                                                  
         MVI   SAOCTYP,SAOCTYPQ                                                 
         MVI   SAOCSUB,SAOCSUBQ                                                 
         MVC   SAOCAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   SAOCAGY,OPTAGY                                                   
         SPACE 1                                                                
VSEL2    GOTO1 AVALOVPG,PARM,(X'80',LSTSYSH),LSTPGMH,0                          
         BNE   VALSELX                                                          
         MVC   SAOCOVPG,APHALF                                                  
         SPACE 1                                                                
VSEL4    CLI   LSTUIDH+FHILD,0     TEST USER-ID ENTERD                          
         BE    VSEL6                                                            
         GOTO1 AVALUID,LSTUIDH                                                  
         BNE   VALSELX                                                          
         MVC   SAOCUID,APHALF                                                   
         SPACE 1                                                                
VSEL6    CLI   LSTACGH+FHILD,0     TEST ACCESS GROUP ENTERED                    
         BE    VSEL8                                                            
         GOTO1 AVALACG,LSTACGH                                                  
         BNE   VALSELX                                                          
         MVC   SAOCAGN,APHALF                                                   
         SPACE 1                                                                
VSEL8    MVC   SELOVS,SAOCOVS                                                   
         MVC   SELPGM,SAOCPGM                                                   
         MVC   SELUID,SAOCUID                                                   
         MVC   SELAGN,SAOCAGN                                                   
         SPACE 1                                                                
         LA    R0,LSTACTH          SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM                                                        
         MVI   APPARM+4,LSTLINEN   SET NO. OF LIST LINES                        
         LA    R0,LSTLINEL         SET LIST LINE LENGTH                         
         STCM  R0,3,APPARM+6                                                    
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO  SET SCREEN TO MODIFIED              
         SPACE 1                                                                
VALSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   L     R2,AIOAREA2                                                      
         USING SAOCREC,R2                                                       
         MVC   IOKEY(L'SAOCKEY),APRECKEY                                        
         SPACE 1                                                                
         TM    APINDS,APILRERD     TEST SEQUENTIAL READ BROKEN                  
         BZ    GSEL2                                                            
         GOTO1 AIO,IOCONFIL+IORD+IO2                                            
         BNE   GETSELN                                                          
         B     GSEL4                                                            
         SPACE 1                                                                
GSEL2    TM    APINDS,APILNSEQ     TEST START OF NEW SCREEN                     
         BO    GSEL4                                                            
         LA    R1,IOCONFIL+IOHI+IO2                                             
         B     *+8                                                              
GSEL4    LA    R1,IOCONFIL+IOSQ+IO2                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         SPACE 1                                                                
         CLC   SAOCKEY(SAOCOVS-SAOCKEY),IOKEYSAV                                
         BNE   GETSELN             TEST RECORD TYPE/AGENCY                      
         SPACE 1                                                                
         GOTO1 ATSTSYS,SAOCOVS     TEST USER CAN CONNECT TO SYSTEM              
         BNE   GSEL4                                                            
         CLI   SELOVS,0            MATCH ON SYSTEM/PROGRAM                      
         BE    GSEL8                                                            
         CLC   SAOCOVS,SELOVS                                                   
         BNE   GETSELN                                                          
         CLI   SELPGM,0                                                         
         BE    GSEL8                                                            
         CLC   SAOCPGM,SELPGM                                                   
         BNE   GETSELN                                                          
         SPACE 1                                                                
GSEL8    GOTO1 ATSTUID,SAOCUID     TEST USER CAN CONNECT TO USER-ID             
         BNE   GSEL4                                                            
         OC    SELUID,SELUID       MATCH ON USER-ID                             
         BZ    GSEL10                                                           
         CLC   SAOCUID,SELUID                                                   
         BNE   GSEL4                                                            
         SPACE 1                                                                
GSEL10   OC    SELAGN,SELAGN       MATCH ON ACCESS GROUP                        
         BZ    GSEL12                                                           
         CLC   SAOCAGN,SELAGN                                                   
         BNE   GSEL4                                                            
         SPACE 1                                                                
GSEL12   OC    SAOCUID(L'SAOCUID+L'SAOCAGN),SAOCUID                             
         BNZ   *+12                TEST MAIN AGENCY RECORD                      
         TM    CUSTAT,CUSDDS       TEST DDS TERMINAL                            
         BZ    GSEL4                                                            
         MVC   APRECKEY(L'SAOCKEY),SAOCKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
         SPACE 1                                                                
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
         SPACE 1                                                                
GETSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA2                                                      
         USING SAOCREC,R2                                                       
         SPACE 1                                                                
         L     R4,APPARM                                                        
         USING LSTACTH,R4          R4=A(LIST/SELECT LINE)                       
         SPACE 1                                                                
         GOTO1 ADISSYS,SAOCOVS                                                  
         MVC   LSTLSYS,APWORK      SYSTEM NAME                                  
         SPACE 1                                                                
         OC    SAOCPGM,SAOCPGM                                                  
         BZ    DSEL2                                                            
         GOTO1 ADISPGM,PARM,(SAOCOVS,SAOCPGM)                                   
         MVC   LSTLPGM,APWORK      PROGRAM NAME                                 
         SPACE 1                                                                
DSEL2    OC    SAOCUID,SAOCUID                                                  
         BZ    DSEL4                                                            
         GOTO1 ADISUID,SAOCUID                                                  
         MVC   LSTLUID,APWORK      USER-ID                                      
         SPACE 1                                                                
DSEL4    OC    SAOCAGN,SAOCAGN                                                  
         BZ    DISSELX                                                          
         GOTO1 ADISACG,SAOCAGN                                                  
         MVC   LSTLACG,APWORK      ACCESS GROUP                                 
         SPACE 1                                                                
DISSELX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         USING SAOCREC,R2                                                       
         SPACE 1                                                                
         GOTO1 ADISSYS,SAOCOVS     SYSTEM NAME                                  
         MVC   DSPSYS,APWORK                                                    
         SPACE 1                                                                
         CLI   SAOCPGM,0           PROGRAM NAME                                 
         BE    DKEY2                                                            
         GOTO1 ADISPGM,PARM,(SAOCOVS,SAOCPGM)                                   
         MVC   DSPPGM,APWORK                                                    
         MVI   DSPPGMH+FHILD,L'DSPPGM                                           
         SPACE 1                                                                
DKEY2    OC    SAOCUID,SAOCUID     USER-ID                                      
         BZ    DKEY4                                                            
         GOTO1 ADISUID,SAOCUID                                                  
         MVC   DSPUID,APWORK                                                    
         MVI   DSPUIDH+FHILD,L'DSPUID                                           
         SPACE 1                                                                
DKEY4    OC    SAOCAGN,SAOCAGN     ACCESS GROUP                                 
         BZ    DISKEYX                                                          
         GOTO1 ADISACG,SAOCAGN                                                  
         MVC   DSPACG,APWORK                                                    
         MVI   DSPACGH+FHILD,L'DSPACG                                           
         SPACE 1                                                                
DISKEYX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR LIST (SET SCREEN TO MODIFIED)           *         
***********************************************************************         
         SPACE 1                                                                
FSTLST   OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE TWA                                           *         
***********************************************************************         
         SPACE 1                                                                
SETTWA   XC    SAVOVER(SAVCLRL),SAVOVER                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         XC    APRECKEY,APRECKEY                                                
         SPACE 1                                                                
         MVI   FVMINL,1            VALIDATE REQUESTOR                           
         GOTO1 AFVAL,REPREQH                                                    
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
         SPACE 1                                                                
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
         SPACE 1                                                                
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
         SPACE 1                                                                
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
         SPACE 1                                                                
VREQ1    GOTO1 AFVAL,REPREQPH      VALIDATE REQUEST PID/PIN                     
         BNE   VREQ1X                                                           
         OI    REPREQPH+6,X'80'                                                 
         ICM   RF,15,=V(FAPQSEC)   TEST IF NEW PID/PIN VALIDATION               
         BZ    *+12                                                             
         A     RF,APRELO           RELOCATE IF FAPQSEC INCLUDED                 
         B     VREQ1A                                                           
         L     RF,ACOM                                                          
         ICM   RF,15,CFAPQSEC-COMFACSD(RF)                                      
         BNZ   VREQ1A                                                           
         MVC   REPPSWD,FVIFLD      SET 4-CHR PIN                                
         MVI   REPSECF1,REPSPIN    SET PIN PROTECTED                            
         MVI   REPSECF2,0                                                       
         B     VREQ1X                                                           
VREQ1A   GOTO1 (RF),APPARM,(X'80',REPREQPH),0,ACOM,APDUB                        
         CLI   12(R1),0                                                         
         BE    VREQ1B                                                           
         TM    12(R1),X'81'        TEST INVALID PID                             
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(106)                                                
         B     VALREQX                                                          
         TM    12(R1),X'82'        TEST INVALID PIN                             
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(105)                                                
         B     VALREQX                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALREQX                                                          
VREQ1B   CLI   APDUB,0             TEST NO PID OR PIN INPUT                     
         BE    VREQ1X                                                           
         MVC   REPPSWD,APDUB+1     SET PID OR PIN VALUE                         
         MVI   REPSECF2,0                                                       
         CLI   APDUB,2                                                          
         BNE   *+12                                                             
         MVI   REPSECF1,REPSPIN    SET PIN PROTECTED                            
         B     VREQ1X                                                           
         CLI   APDUB,3                                                          
         BNE   *+12                                                             
         MVI   REPSECF1,REPSPID    SET PID PROTECTED                            
         B     VREQ1X                                                           
VREQ1X   EQU   *                                                                
         SPACE 1                                                                
VREQ2    GOTO1 AVALOVPG,PARM,(X'80',REPSYSH),REPPGMH,0                          
         BNE   VALREQX                                                          
         MVC   SELOVPG,APHALF                                                   
         SPACE 1                                                                
VREQ4    CLI   REPUIDH+FHILD,0     TEST USER-ID ENTERD                          
         BE    VREQ6                                                            
         GOTO1 AVALUID,REPUIDH                                                  
         BNE   VALREQX                                                          
         MVC   SELUID,APHALF                                                    
         SPACE 1                                                                
VREQ6    CLI   REPACGH+FHILD,0     TEST ACCESS GROUP ENTERED                    
         BE    VREQ8                                                            
         GOTO1 AVALACG,REPACGH                                                  
         BNE   VALREQX                                                          
         MVC   SELAGN,APHALF                                                    
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
VREQ10   MVC   APRECKEY,SELKEY                                                  
         SPACE 1                                                                
         MVCDD REPDESC,CT#OCONL    SET REPORT DESCRIPTION                       
         GOTO1 VDICTAT,PARM,C'SL  ',REPDESC                                     
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         OI    REPIND2,REPILOW                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SPACE 1                                                                
VALREQX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE REPORT                                          *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   MVC   SELKEY,APRECKEY                                                  
         L     R9,AREP                                                          
         USING REPD,R9                                                          
         SPACE 1                                                                
         LA    RF,REPM1            SET UP MIDLINES                              
         USING REPP1,RF                                                         
         MVCDD REPPSYS,CT#SYS,L                                                 
         MVCDD REPPPGM,CT#PROG,L                                                
         MVCDD REPPUID,CT#USRID,L                                               
         MVCDD REPPACG,CT#GROUP,L                                               
         MVCDD REPPOPTS,CT#AOP,L                                                
         CLC   SELVAL,INPYES                                                    
         BNE   *+10                                                             
         MVCDD REPPOPTS,CT#VALOP,L                                              
         CLC   SELVAL,INPNO                                                     
         BNE   *+10                                                             
         MVCDD REPPOPTS,CT#INVOP,L                                              
         MVC   REPP2,REPP1         SET MIDLINE2 TO MIDLINE1 UNDERLINED          
         LA    RF,REPP2                                                         
         MVI   REPPSYS,CT#ESUL                                                  
         MVI   REPPPGM,CT#ESUL                                                  
         MVI   REPPUID,CT#ESUL                                                  
         MVI   REPPACG,CT#ESUL                                                  
         MVI   REPPOPTS,CT#ESUL                                                 
         DROP  RF                                                               
         SPACE 1                                                                
         ICM   R8,15,REPABOX       TEST FOR BOXES DSECT                         
         USING BOXD,R8             R8=A(BOXD)                                   
         BZ    PREP2                                                            
         MVI   BOXYORN,YES         INITIALIZE BOXES                             
         MVI   BOXOFF,NO                                                        
         MVI   BOXBLANK,NO                                                      
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         OI    BOXDDCTL,BOXDDUL+BOXDDLC                                         
         SPACE 1                                                                
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+99,C'B'                                                  
         OI    REPHEADI,REPHOPEN                                                
         OI    REPMIDSI,REPMMLIN                                                
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES      SET COLUMNS                                  
         LA    RF,BOXCOLS                                                       
         USING REPP1+1,RF                                                       
         MVI   REPPSYS,C'L'                                                     
         MVI   REPPPGM,C'C'                                                     
         MVI   REPPUID,C'C'                                                     
         MVI   REPPACG,C'C'                                                     
         MVI   REPPOPTS,C'C'                                                    
         MVI   REPPEND,C'R'                                                     
         DROP  RF                                                               
         SPACE 1                                                                
PREP2    LA    R2,IOKEY            SET UP INITIAL KEY                           
         USING SAOCREC,R2          R2=A(OCONTROL RECORD KEY)                    
         XC    SAOCKEY,SAOCKEY                                                  
         MVI   SAOCTYP,SAOCTYPQ                                                 
         MVI   SAOCSUB,SAOCSUBQ                                                 
         MVC   SAOCAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   SAOCAGY,OPTAGY                                                   
         MVC   SAOCOVS,SELOVS                                                   
         MVC   SAOCPGM,SELPGM                                                   
         MVC   SAOCUID,SELUID                                                   
         MVC   SAOCAGN,SELAGN                                                   
         L     R2,AIOAREA1                                                      
         SPACE 1                                                                
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
PREP10   LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   PREP30                                                           
         CLC   SAOCKEY(SAOCOVS-SAOCKEY),IOKEYSAV                                
         BNE   PREP30              TEST RECORD TYPE/AGENCY                      
         SPACE 1                                                                
         OC    SAOCUID(L'SAOCUID+L'SAOCAGN),SAOCUID                             
         BZ    PREP10              IGNORE AGENCY RECORDS                        
         SPACE 1                                                                
         GOTO1 ATSTSYS,SAOCOVS     TEST USER CAN CONNECT TO SYSTEM              
         BNE   PREP10                                                           
         CLI   SELOVS,0            MATCH ON SYSTEM/PROGRAM                      
         BE    PREP12                                                           
         CLC   SAOCOVS,SELOVS                                                   
         BNE   PREP30                                                           
         CLI   SELPGM,0                                                         
         BE    PREP12                                                           
         CLC   SAOCPGM,SELPGM                                                   
         BNE   PREP30                                                           
         SPACE 1                                                                
PREP12   GOTO1 ATSTUID,SAOCUID     TEST USER CAN CONNECT TO USER-ID             
         BNE   PREP10                                                           
         OC    SELUID,SELUID       MATCH ON USER-ID                             
         BZ    PREP14                                                           
         CLC   SAOCUID,SELUID                                                   
         BNE   PREP10                                                           
         SPACE 1                                                                
PREP14   OC    SELAGN,SELAGN       MATCH ON ACCESS GROUP                        
         BZ    PREP20                                                           
         CLC   SAOCAGN,SELAGN                                                   
         BNE   PREP10                                                           
         SPACE 1                                                                
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
         SPACE 1                                                                
PREP26   BAS   RE,PRTRCD           PRINT RECORD                                 
         SPACE 1                                                                
         MVC   IOKEY,SAVKEY        RE-READ CURRENT RECORD                       
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         B     PREP10                                                           
         SPACE 1                                                                
PREP30   LTR   R8,R8               TEST FOR BOXES                               
         BZ    PRTREPX                                                          
         IC    RE,REPLINE          CLOSE BOX                                    
         LA    RE,1(RE)              UNLESS AT END OF PAGE                      
         CLM   RE,1,REPMAXL                                                     
         BNL   PRTREPX                                                          
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VREPORT,REPD                                                     
         SPACE 1                                                                
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R8,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT OCONTROL RECORD DETAILS                            *         
***********************************************************************         
         SPACE 1                                                                
PRTRCD   NTR1  ,                                                                
         L     R2,AIOAREA1                                                      
         USING SAOCREC,R2                                                       
         SPACE 1                                                                
         GOTO1 ADISSYS,SAOCOVS     PRINT SYSTEM                                 
         MVC   REPPSYS,APWORK                                                   
         SPACE 1                                                                
         CLI   SAOCPGM,0           PRINT PROGRAM                                
         BE    PRCD2                                                            
         GOTO1 ADISPGM,PARM,(SAOCOVS,SAOCPGM)                                   
         MVC   REPPPGM,APWORK                                                   
         SPACE 1                                                                
PRCD2    OC    SAOCUID,SAOCUID     PRINT USER-ID                                
         BZ    PRCD4                                                            
         GOTO1 ADISUID,SAOCUID                                                  
         MVC   REPPUID,APWORK                                                   
         SPACE 1                                                                
PRCD4    OC    SAOCAGN,SAOCAGN     PRINT ACCESS GROUP                           
         BZ    PRCD6                                                            
         GOTO1 ADISACG,SAOCAGN                                                  
         MVC   REPPACG,APWORK                                                   
         SPACE 1                                                                
PRCD6    MVC   SAVOVPG,SAOCOVPG    SET RECORD DETAILS                           
         MVC   SAVKEY,SAOCKEY                                                   
         GOTO1 FINDEL,SAOCTELQ                                                  
         GOTO1 ABLDBITT,PARM,(R3),VALIDTAB                                      
         BAS   RE,INITOTAB         INITIALIZE OPTION TABLE                      
         NC    VALIDTAB,PRNTVAL                                                 
         DROP  R2                                                               
         SPACE 1                                                                
PRCD8    L     R8,AOTAB                                                         
         USING OTABD,R8            R8=A(OPTION TABLE ENTRY)                     
         XR    R2,R2                                                            
         SPACE 1                                                                
PRCD10   CLI   OTABD,EOT                                                        
         BE    PRCD20                                                           
         SPACE 1                                                                
         MVC   PRTVAL,INPNO        SET NO/YES                                   
         GOTO1 TESTBIT,PARM,(OTCODE,VALIDTAB)                                   
         BZ    *+10                                                             
         MVC   PRTVAL,INPYES                                                    
         SPACE 1                                                                
         CLI   SELVAL,0            TEST VALIDATION FILTER                       
         BE    PRCD12                                                           
         CLC   SELVAL,PRTVAL                                                    
         BNE   PRCD18                                                           
         SPACE 1                                                                
PRCD12   GOTO1 ADISDIC,PARM,(SAVOVS,L'REPPOWRD),(C'L',OTWRD)                    
         SPACE 1                                                                
         LTR   R2,R2               TEST FIRST OPTION PRINTED                    
         BZ    PRCD14                                                           
         LA    R0,L'REPPOPT        BUMP R2 TO NEXT COLUMN                       
         LA    R1,REPPOPTS+L'REPPOPTS-1                                         
         BXLE  R2,R0,PRCD16                                                     
         SPACE 1                                                                
         GOTO1 VREPORT,REPD        PRINT IF ROW IS FULL                         
         SPACE 1                                                                
PRCD14   LA    R2,REPPOPTS         RESET R2 TO FIRST COLUMN                     
         SPACE 1                                                                
         USING REPPOPT,R2                                                       
PRCD16   MVC   REPPOWRD,APWORK     PRINT WORD/VALIDATION                        
         MVC   REPPOVAL,PRTVAL                                                  
         DROP  R2                                                               
         SPACE 1                                                                
PRCD18   LA    R8,OTABL(R8)                                                     
         B     PRCD10                                                           
         DROP  R8                                                               
         SPACE 1                                                                
PRCD20   LTR   R2,R2               TEST ANY OPTIONS LISTED                      
         BNZ   PRCD22                                                           
         MVI   REPPOWRD,C'*'                                                    
         MVCDD REPPOWRD+1(L'REPPOWRD),CT#NONE,C                                 
         MVI   REPPOWRD+L'REPPOWRD+1,C'*'                                       
PRCD22   GOTO1 VREPORT,REPD        PRINT LAST LINE                              
         SPACE 1                                                                
PRTRCDX  B     XIT                                                              
         SPACE 3                                                                
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE OPTION TABLE                                             *         
*                                                                     *         
* NTRY: SAVKEY=CURRENT OCONTROL RECORD KEY                            *         
* EXIT: OCRINDS, PRNTKEY INITIALIZED                                  *         
*       OPTIONS TABLE SET UP                                          *         
***********************************************************************         
         SPACE 1                                                                
INITOTAB NTR1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* - SET UP OCONTROL RECORD PARENT KEY                                 *         
***********************************************************************         
         SPACE 1                                                                
         MVI   OCRINDS,0           RESET OCONTROL RECORD INDICATORS             
         SPACE 1                                                                
         MVC   WORKKEY,SAVKEY      SET UP PARENT KEY IN WORK KEY                
         LA    R2,WORKKEY                                                       
         USING SAOCREC,R2                                                       
         SPACE 1                                                                
         OC    SAOCAGN,SAOCAGN     TEST FOR ACCESS GROUP                        
         BZ    IOTAB2                                                           
         OI    OCRINDS,OCRIACGP                                                 
         XC    SAOCAGN,SAOCAGN                                                  
         B     IOTAB6                                                           
         SPACE 1                                                                
IOTAB2   OC    SAOCUID,SAOCUID     TEST FOR USER-ID                             
         BZ    IOTAB4                                                           
         XC    SAOCUID,SAOCUID                                                  
         B     IOTAB6                                                           
         SPACE 1                                                                
IOTAB4   OI    OCRINDS,OCRIAGY     MAIN AGENCY RECORD                           
         XC    SAOCAGY,SAOCAGY                                                  
         SPACE 1                                                                
IOTAB6   OC    SAOCUID,SAOCUID     TEST PARENT IS FOR USER-ID                   
         BNZ   *+8                                                              
         OI    OCRINDS,OCRIPAGY      OR FOR AGENCY                              
         SPACE 1                                                                
         CLC   PRNTKEY,SAOCKEY     TEST CHANGE OF PARENT                        
         BNE   IOTAB8                                                           
         OI    OCRINDS,OCRISIBL                                                 
         B     IOTABYES                                                         
         SPACE 1                                                                
IOTAB8   MVC   PRNTKEY,SAOCKEY                                                  
         EJECT                                                                  
***********************************************************************         
*  - SET UP OPTIONS TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
         MVI   PRNTVAL,FF          SET EVERYTHING VALID FOR PARENT              
         MVC   PRNTVAL+1(L'PRNTVAL-1),PRNTVAL                                   
         SPACE 1                                                                
         TM    OCRINDS,OCRIAGY     TEST MAIN AGENCY RECORD                      
         BO    IOTAB10                                                          
         LA    R2,IOKEY              NO - MERGE IN MAIN AGENCY RECORD           
         USING SAOCREC,R2                                                       
         MVC   SAOCKEY,PRNTKEY                                                  
         XC    SAOCUID,SAOCUID                                                  
         BAS   RE,MERGPRNT                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#DRANF)                                           
         B     IOTABNO                                                          
         SPACE 1                                                                
         TM    OCRINDS,OCRIPAGY    TEST PARENT IS MAIN AGENCY RECORD            
         BO    IOTAB10                                                          
         MVC   SAOCKEY,PRNTKEY       NO - MERGE IN USER-ID RECORD               
         BAS   RE,MERGPRNT                                                      
         DROP  R2                                                               
         SPACE 1                                                                
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
         SPACE 1                                                                
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
IOTAB12  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   IOTAB20             TEST OPTION RECORD FOR PROGRAM               
         CLC   SAOPKEY(SAOPOCD-SAOPKEY),IOKEYSAV                                
         BNE   IOTAB20                                                          
         SPACE 1                                                                
         GOTO1 TESTBIT,PARM,(SAOPOCD,PRNTVAL)  TEST OPTION VALID                
         BZ    IOTAB12                                                          
         GOTO1 SETBIT,PARM,(SAOPOCD,WORKBITT)                                   
         SPACE 1                                                                
         MVC   OTCODE,SAOPOCD      ADD OPTION TABLE ENTRY                       
         GOTO1 FINDEL,SAOPTELQ                                                  
         MVC   OTWRD,SAOPTWRD-SAOPTD(R3)                                        
         MVC   OTDSC,SAOPTDSC-SAOPTD(R3)                                        
         SPACE 1                                                                
         LA    R8,OTABL(R8)                                                     
         B     IOTAB12                                                          
         SPACE 1                                                                
IOTAB20  MVI   OTABD,EOT           SET END OF TABLE                             
         NC    PRNTVAL,WORKBITT    ENSURE NO EXTRA BITS SET ON                  
         DROP  R8,R2                                                            
         SPACE 1                                                                
         MVI   PAGENO,1            SET INITIAL PAGE INFO                        
         LA    R0,OTAB-OTABPAGE+OTABL                                           
         SR    R8,R0                                                            
         SRDL  R8,32                                                            
         LA    R0,OTABPAGE                                                      
         DR    R8,R0                                                            
         STC   R9,NUMPAGES                                                      
         LTR   R9,R9               TEST >0 ENTRIES                              
         BNZ   IOTABYES                                                         
         OI    DSPINDS,DSPINONE                                                 
         OI    OCRINDS,OCRIREAD                                                 
         XC    PRNTKEY,PRNTKEY                                                  
         SPACE 1                                                                
IOTABYES CR    RB,RB                                                            
         B     XIT                                                              
IOTABNO  LTR   RB,RB                                                            
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO MERGE IN PARENT OCONTROL RECORD DETAILS                  *         
*                                                                     *         
* NTRY: IOKEY=KEY OF PARENT RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
MERGPRNT NTR1  ,                                                                
         L     R2,AIOAREA1         READ PARENT RECORD                           
         USING SAOCREC,R2                                                       
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   MPRNTN                                                           
         SPACE 1                                                                
         GOTO1 FINDEL,SAOCTELQ     AND IN PARENT BIT TABLE                      
         GOTO1 ABLDBITT,PARM,(R3),WORKBITT                                      
         NC    PRNTVAL,WORKBITT                                                 
         SPACE 1                                                                
MPRNTY   CR    RB,RB                                                            
         B     XIT                                                              
MPRNTN   LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND AN ELEMENT                                          *         
*                                                                     *         
* NTRY: RECORD IN IOAREA1, R1=ELEMENT CODE                            *         
* EXIT: R3=A(ELEMENT), RF=L(ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
FINDEL   L     R3,AIOAREA1         SET R3 TO FIRST ELEMENT                      
         LA    R3,SAOCDATA-SAOCREC(R3)                                          
         XR    RF,RF                                                            
         SPACE 1                                                                
FEL2     CLI   0(R3),0             TEST E-O-R                                   
         BE    FELLOVER                                                         
         SPACE 1                                                                
         IC    RF,1(R3)            RF=LENGTH OF ELEMENT                         
         CLM   R1,1,0(R3)          MATCH ON ELEMENT CODE                        
         BER   RE                                                               
         BXH   R3,RF,FEL2          BUMP R3 TO NEXT ELEMENT                      
         SPACE 1                                                                
FELLOVER SR    RE,RB               SHIT                                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO TEST/SET/RESET A BIT OF A BIT TABLE                     *         
*                                                                     *         
* NTRY: P1=(BIT CODE,A(BIT TABLE))                                    *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
TESTBITM TM    0(RF),0                                                          
         SPACE 3                                                                
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
         SPACE 3                                                                
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
         SPACE 3                                                                
MASKS    DC    X'8040201008040201'                                              
UNMASKS  DC    X'7FBFDFEFF7FBFDFE'                                              
         EJECT                                                                  
GI#ENTFD EQU   X'FF00'+02          ENTER FIELDS AS REQUIRED                     
GI#ENTCH EQU   X'FF00'+04          RECORD DISPLAYED - ENTER CHANGES             
GI#PETDE EQU   X'FF00'+24          PRESS ENTER TO DELETE                        
GI#PETRE EQU   X'FF00'+25          PRESS ENTER TO RESTORE                       
         SPACE 1                                                                
OPTINPLQ EQU   DSPOPT2H-DSPOPTH    LENGTH OF OPTION/INPUT FIELDS                
OPTINPSQ EQU   DSPBBARH-DSPOPTH    LENGTH OF 1 PAGE OF OPTION/INPUTS            
OPTSDISQ EQU   OPTINPSQ/OPTINPLQ   NUMBER OF OPTION/INPUTS DISPLAYED            
OTABPAGE EQU   OTABL*OPTSDISQ      LENGTH OF 1 PAGE OF OTAB ENTRIES             
         SPACE 1                                                                
HELPWRDL EQU   8                   LENGTH OF WORD FOR 'DISHELP' ROUTINE         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
SPACES   DC    CL(L'REPP1)' '                                                   
         SPACE 1                                                                
REPSPEC  DS    0X                  * REPORT SPECS *                             
         SPEC  H1,1,RUN                                                         
         SPEC  H1,50,CT#OCONL,32,C                                              
         SPEC  H2,50,CT#OCONL,32,CU                                             
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,126,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         SPACE 3                                                                
* SEACSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSF1D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSD1D                                                       
         SPACE 1                                                                
         ORG   LSTLINE             * LIST LINE LAYOUT *                         
LSTLSYS  DS    CL7                 SYSTEM                                       
         DS    CL2                                                              
LSTLPGM  DS    CL7                 PROGRAM                                      
         DS    CL2                                                              
LSTLUID  DS    CL8                 USER-ID                                      
         DS    CL2                                                              
LSTLACG  DS    CL46                ACCESS GROUP                                 
         ORG                                                                    
         SPACE 1                                                                
LSTLINEL EQU   LSTACT2H-LSTACTH    LIST LINE LENGTH                             
LSTLINES EQU   LSTFOOTH-LSTACTH    LIST LINES LENGTH                            
LSTLINEN EQU   LSTLINES/LSTLINEL   NO. OF LIST LINES                            
         SPACE 1                                                                
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSB1D                                                       
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* PRINT LINE LAYOUT                                                   *         
***********************************************************************         
         SPACE 1                                                                
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
REPPOPTS DS    0CL96               OPTIONS                                      
REPPOPT  DS    0CL26                                                            
REPPOWRD DS    CL16                OPTION WORD                                  
         DS    CL1                                                              
REPPOVAL DS    CL1                 OPTION VALIDITY                              
         ORG   REPPOPTS+L'REPPOPTS                                              
         DS    CL1                                                              
REPPEND  DS    0C                  END OF LINE                                  
         SPACE 3                                                                
***********************************************************************         
* DSECT FOR OPTIONS TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
OTABD    DSECT                                                                  
OTCODE   DS    XL1                 OPTION CODE                                  
OTWRD    DS    XL2                 OPTION WORD DICTIONARY REFERENCE#            
OTDSC    DS    XL2                 OPTION TEXT DESCRIPTION#                     
OTABL    EQU   *-OTABD                                                          
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE SAVED IN TWA                                        *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SAVOVER                                                          
         SPACE 1                                                                
SAVOVPG  DS    0XL2                CURRENT OVERLAY SYSTEM/PROGRAM               
SAVOVS   DS    XL1                 CURRENT OVERLAY SYSTEM                       
SAVPGM   DS    XL1                 CURRENT PROGRAM                              
         SPACE 1                                                                
SAVKEY   DS    XL(L'SAOCKEY)       CURRENT OCONTROL RECORD KEY                  
PRNTKEY  DS    XL(L'SAOCKEY)       CUURENT OCONTROL'S PARENT RECORD KEY         
         SPACE 1                                                                
SAVDATE  DS    CL(L'FVXTRA)        ACTIVITY DATE OF CURRENT RECORD              
SAVSEQNO DS    XL(L'SAACVSEQ)      SEQUENCE NUMBER OF CURRENT RECORD            
         SPACE 1                                                                
SAVAPIND DS    XL1                 SAVED APINDS                                 
SAVAPACT DS    XL1                 SAVED APACTN                                 
         SPACE 1                                                                
OCRINDS  DS    XL1                 * OPTION CONTROL RECORD INDICATORS *         
OCRIAGY  EQU   X'80'               MAIN AGENCY DEFAULT RECORD                   
OCRISIBL EQU   X'40'               RECORD IS SIBLING OF LAST RECORD             
OCRIPAGY EQU   X'20'               RECORD'S PARENT IS AGENCY RECORD             
OCRIACGP EQU   X'10'               RECORD FOR ACCESS GROUP                      
OCRIPRNT EQU   X'08'               RECORD IS A PARENT                           
OCRIREAD EQU   X'01'               READ RECORD                                  
         SPACE 1                                                                
DSPINDS  DS    XL1                 * DISPLAY INDICATORS *                       
DSPIKEEP EQU   X'80'               KEEP CURRENT DISPLAY FOR NEXT INPUT          
DSPIDONE EQU   X'40'               USER HAS FINISHED INPUT                      
DSPIINP  EQU   X'20'               USER HAS INPUT TO SUB-SCREEN                 
DSPIDSP  EQU   X'10'               DISPLAY FOR DISPLAY (NOT ADD/CHANGE)         
DSPINONE EQU   X'08'               NO OPTIONS TO LIST IN SUB-SCREEN             
         SPACE 1                                                                
SAVCLRL  EQU   *-SAVOVER           CLEAR TWA UP TO HERE (FOR SETTWA)            
         SPACE 3                                                                
VALIDTAB DS    XL32                BIT TABLE OF VALID OPTIONS                   
PRNTVAL  DS    XL32                PARENT BIT TABLE                             
         SPACE 1                                                                
SUBSTAB  DS    CL2                 SUB-SCREEN TAB TO NEXT PAGE                  
         SPACE 1                                                                
PAGENO   DS    XL1                 CURRENT DISPLAYED PAGE NUMBER                
NUMPAGES DS    XL1                 NUMBER OF PAGES                              
         ORG   SAVAREAX                                                         
OTAB     DS    (250)XL(OTABL)      OPTION TABLE                                 
OTABX    DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
DUB      DS    D                                                                
WORK     DS    XL80                                                             
PARM     DS    6A                                                               
AOTAB    DS    A                   A(OPTION TABLE)                              
WORKKEY  DS    XL(L'SAOCKEY)                                                    
WORKBITT DS    XL32                                                             
         SPACE 1                                                                
WAGY     DS    CL2                 EBCIDIC AGENCY CODE                          
         SPACE 1                                                                
*                                  * DATA FOR SUB-SCREEN MESSAGE *              
SUBMSGNO DS    XL2                 SUB-SCREEN MESSAGE NUMBER                    
SUBMTYP  DS    XL1                 SUB-SCREEN MESSAGE TYPE                      
SUBXTRA  DS    XL40                TEXT TO ADD TO SUB-SCREEN MESSAGE            
SUBXINDS DS    XL1                 EXTRA TEXT INDICATORS                        
SUBXIMID EQU   X'80'                 SUBXTRA IN MIDDLE, NOT AFTER TEXT          
SUBTEXT  DS    XL(L'DSPMSG)        SUB-SCREEN TEXT (IF SET BY USER)             
         SPACE 1                                                                
SELKEY   DS    0XL25               * SELECTS *                                  
SELOVPG  DS    0XL2                                                             
SELOVS   DS    XL1                 SELECT SYSTEM                                
SELPGM   DS    XL1                 SELECT PROGRAM                               
SELUID   DS    XL2                 SELECT USER-ID                               
SELAGN   DS    XL2                 SELECT ACCESS GROUP NUMBER                   
SELVAL   DS    CL1                 VALIDATION FILTER                            
         ORG   SELKEY+L'SELKEY                                                  
         SPACE 1                                                                
PRTVAL   DS    CL1                 VALIDITY OF OPTION ON REPORT                 
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SEACS0E   10/27/11'                                      
         END                                                                    
