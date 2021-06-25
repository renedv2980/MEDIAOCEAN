*          DATA SET SEACS0F    AT LEVEL 004 AS OF 10/27/11                      
*PHASE TA0D0FA                                                                  
ACS0F    TITLE '- SECURITY ACCESS - FIELD CONTROL RECORDS'                      
ACS0F    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACSF**,RA,RR=RE                                              
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
         LA    RE,FTAB-SAVOVER(RE)                                              
         ST    RE,AFTAB            SAVE A(FIELD TABLE)                          
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
* ROUTINE TO VALIDATE KEY OF FIELD CONTOL RECORD                      *         
*  - VALIDATE INPUT FIELDS & INITIALIZE RECORD KEY                    *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   MVI   DSPINDS,0           CLEAR DISPLAY INDICATORS                     
         SPACE 1                                                                
         LA    R2,SAVKEY                                                        
         USING SAFCREC,R2          R2=A(RECORD KEY)                             
         SPACE 1                                                                
         CLC   APACTN,SAVAPACT     IF ACTION CHANGED FROM CHANGE                
         BE    VKEY2                 RECORD SHOULD BE RE-READ                   
         CLI   SAVAPACT,ACTCHA                                                  
         MVC   SAVAPACT,APACTN                                                  
         BNE   *+8                                                              
         OI    FCRINDS,FCRIREAD                                                 
         SPACE 1                                                                
VKEY2    MVC   WAGY,CUAALF         GET AGENCY EBCIDIC CODE                      
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   WAGY,OPTAGY                                                      
         CLC   SAFCAGY,WAGY        TEST CHANGE OF AGENCY                        
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
         MVC   WORKKEY,SAFCKEY     SAVE OLD RECORD KEY                          
         XC    SAFCKEY,SAFCKEY     BUILD NEW RECORD KEY                         
         MVI   SAFCTYP,SAFCTYPQ                                                 
         MVI   SAFCSUB,SAFCSUBQ                                                 
         MVC   SAFCAGY,WAGY                                                     
         SPACE 1                                                                
         GOTO1 AVALOVPG,PARM,DSPSYSH,(0,DSPPGMH),DSPPGMDH                       
         BNE   VALKEYN             VALIDATE SYSTEM/PROGRAM                      
         MVC   SAFCOVPG,APHALF                                                  
         MVC   SAVOVPG,APHALF                                                   
         BAS   RE,BLDPFK                                                        
         SPACE 1                                                                
         CLI   DSPUIDH+FHILD,0     TEST USER-ID ENTERED                         
         BNE   VKEY6                                                            
         LA    R0,DSPUIDH                                                       
         ST    R0,FVADDR                                                        
         GOTO1 ATSTUID,SAFCUID     TEST 'ALL' USER-ID VALID                     
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
         MVC   SAFCUID,APHALF                                                   
         MVC   DSPUIDD,APWORK                                                   
*                                                                               
VKEY8    CLI   DSPACGH+FHILD,0     TEST ACCESS GROUP ENTERED                    
         BE    VKEY10                                                           
         GOTO1 AVALACG,DSPACGH                                                  
         BNE   VALKEYN                                                          
         MVC   SAFCAGN,APHALF                                                   
         MVC   DSPACGD,APWORK                                                   
         SPACE 1                                                                
VKEY10   CLC   SAFCKEY,WORKKEY     TEST CHANGE OF KEY                           
         BE    *+8                                                              
         OI    FCRINDS,FCRIREAD                                                 
         SPACE 1                                                                
VKEY12   CLI   APACTN,ACTDIS       IF ACTION IS DISPLAY                         
         BE    VKEY16                NO RESTRICTIONS                            
         SPACE 1                                                                
         OC    SAFCAGN,SAFCAGN     IF NO ACCESS GROUP                           
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
VKEY16   TM    FCRINDS,FCRIREAD    TEST RECORD TO BE READ                       
         BZ    VALKEYY                                                          
         SPACE 1                                                                
         BAS   RE,INITFTAB         INITIALIZE FIELDS TABLE                      
         BE    VKEY22                                                           
         LA    RE,DSPSYSH                                                       
         ST    RE,FVADDR                                                        
         B     VALKEYN                                                          
         EJECT                                                                  
***********************************************************************         
*  - READ/SET-UP FCONTROL RECORD                                      *         
***********************************************************************         
         SPACE 1                                                                
VKEY22   L     R2,AIOAREA1         READ FIELD CONTROL RECORD                    
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
         OI    FCRINDS,FCRIREAD                                                 
         SPACE 1                                                                
VKEY24   TM    FCRINDS,FCRISIBL    IF SAME PARENT                               
         BO    VKEY40                CURRENT DISPLAY IS OKAY                    
         MVC   READTAB,AVAILRD     SET EVERYTHING VALID                         
         MVC   WRITETAB,AVAILWRT                                                
         B     VKEY30                                                           
         SPACE 1                                                                
VKEY26   MVI   SAVAPIND,APIOKDIS+APIOKCHA+APIOKDEL                              
         TM    FCRINDS,FCRIACGP    IF ACCESS GROUP DEFINED                      
         BO    VKEY28                RECORD IS NOT A PARENT                     
         SPACE 1                                                                
         GOTO1 AIO,IOSQ+IOCONFIL+IO2  TEST NEXT RECORD ON FILE                  
         L     RF,AIOAREA2               IS CHILD OF RECORD                     
         TM    FCRINDS,FCRIAGY                                                  
         BZ    *+14                                                             
         CLC   SAFCKEY(SAFCUID-SAFCKEY),0(RF)                                   
         B     *+10                                                             
         CLC   SAFCKEY(SAFCAGN-SAFCKEY),0(RF)                                   
         BNE   VKEY28                                                           
         OI    FCRINDS,FCRIPRNT    RECORD IS A PARENT                           
         NI    SAVAPIND,FF-APIOKDEL  AND SO CANNOT BE DELETED                   
         SPACE 1                                                                
VKEY28   GOTO1 FINDEL,SAFCWELQ     BUILD BIT TABLES                             
         GOTO1 ABLDBITT,PARM,(R3),WRITETAB                                      
         NC    WRITETAB,AVAILWRT                                                
         GOTO1 FINDEL,SAFCRELQ                                                  
         GOTO1 ABLDBITT,PARM,(R3),READTAB                                       
         NC    READTAB,AVAILRD                                                  
         SPACE 1                                                                
         GOTO1 SAVEACT,SAFCREC     SAVE ACTIVITY ELEMENT INFO                   
         MVC   FVXTRA,SAVDATE                                                   
         SPACE 3                                                                
***********************************************************************         
*  - INITIALIZE DISPLAY                                               *         
***********************************************************************         
         SPACE 1                                                                
VKEY30   BAS   RE,DISFLDS          DISPLAY FIELDS                               
         BAS   RE,SUBMSG                                                        
         OI    DSPINDS,DSPIKEEP    IGNORE INPUT FOR 1ST DISPLAY                 
         SPACE 1                                                                
VKEY40   TM    DSPINDS,DSPINONE    CAN'T ADD IF NO FIELDS TO LIST               
         BZ    VKEY42                                                           
         CLI   APACTN,ACTADD                                                    
         BNE   VKEY42                                                           
         MVC   FVMSGNO,=AL2(CE#NFDTL)                                           
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
         NI    DSPSYSH+FHIID,FF-FHIIVA    ENSURE A KEY FIELD                    
         B     XIT                          IS UNVALIDATED                      
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A FIELD CONTROL RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
VALREC   BAS   RE,FLDSINP          VALIDATE FIELDS ON SCREEN                    
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
         USING SAFCREC,R2          R2=A(FCONTROL RECORD)                        
         MVC   SAFCKEY,SAVKEY      SET UP ELEMENTLESS RECORD                    
         LA    R0,SAFCDATA-SAFCREC+1                                            
         STCM  R0,3,SAFCLEN                                                     
         MVI   SAFCSTAT,0                                                       
         MVI   SAFCDATA,0                                                       
         SPACE 1                                                                
         LA    R3,APELEM           ADD WRITE ELEMENT                            
         USING SAFCWD,R3                                                        
         XC    SAFCWD(SAFCWLNQ),SAFCWD                                          
         MVI   SAFCWEL,SAFCWELQ                                                 
         GOTO1 ATAGBITT,PARM,SAFCWD,WRITETAB                                    
         GOTO1 AADDELS,SAFCREC                                                  
         SPACE 1                                                                
         USING SAFCRD,R3           ADD READ ELEMENT                             
         XC    SAFCRD(SAFCRLNQ),SAFCRD                                          
         MVI   SAFCREL,SAFCRELQ                                                 
         GOTO1 ATAGBITT,PARM,SAFCRD,READTAB                                     
         GOTO1 AADDELS,SAFCREC                                                  
         DROP  R3                                                               
         SPACE 1                                                                
         GOTO1 UPDTACT,PARM,AIOAREA2,SAFCREC                                    
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
* ROUTINE TO DISPLAY FIELD CONTROL RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DISREC   BAS   RE,FLDSINP          VALIDATE FIELDS INPUT                        
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
DELRES   BAS   RE,FLDSINP          VALIDATE FIELDS INPUT                        
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
         USING SAFCREC,R2                                                       
         GOTO1 UPDTACT,PARM,SAFCREC,SAFCREC                                     
         BNE   DELRESX                                                          
         SPACE 1                                                                
         XI    SAFCSTAT,X'80'      CHANGE DELETE FLAG IN RECORD                 
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
* EXIT: CC=NOT EQUAL IF RECORD HAS BEEN CHANGED SINCE LAST READ       *         
***********************************************************************         
         SPACE 1                                                                
UPDTACT  NTR1  ,                                                                
         SPACE 1                                                                
         LM    R2,R3,0(R1)                                                      
         USING SAFCREC,R2                                                       
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
UACT6    GOTO1 AGETACT,SAFCREC     TEST SEQUENCE NUMBER HAS NOT CHANGED         
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
UPDTACTN OI    FCRINDS,FCRIREAD    EXIT WITH CC=NOT EQUAL                       
         B     XIT                   & FORCE RE-READING OF RECORD               
         DROP  R2,R8                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SAVE ACTIVITY ELEMENT DETAILS                            *         
*                                                                     *         
* NTRY: R1=A(FCONTROL RECORD)                                         *         
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
* ROUTINE TO VERIFY INPUT FOR FIELDS SUB-SCREEN                       *         
*                                                                     *         
* EXIT: CC=EQUAL IF DISPLAY IS 'FINISHED'                             *         
***********************************************************************         
         SPACE 1                                                                
FLDSINP  NTR1  ,                                                                
         TM    DSPINDS,DSPINONE    TEST ANYTHING TO DISPLAY                     
         BZ    FINP02                                                           
         OI    DSPINDS,DSPIDONE                                                 
         LA    RE,ACSACTH                                                       
         ST    RE,APCURSOR                                                      
         B     FLDSINPY                                                         
         SPACE 1                                                                
FINP02   TM    DSPINDS,DSPIKEEP    TEST KEEP CURRENT DISPLAY                    
         BO    FINPX2                                                           
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
         B     PFWRT               PFK03 = WRITE                                
         B     PFWRTX              PFK04 = WRITE+                               
         B     PFREAD              PFK05 = READ                                 
         B     PFREADX             PFK06 = READ+                                
         B     PFNO                PFK07 = NO                                   
         B     PFNOX               PFK08 = NO+                                  
         B     PFBACK              PFK09 = SCROLL BACK                          
         B     PFFRWD              PFK10 = SCROLL FORWARD                       
         B     PFUNDF              PFK11 = UNDEFINED                            
         B     PFUNDF              PFK12 = UNDEFINED                            
         SPACE 1                                                                
FLDSINPX BAS   RE,SUBMSG           DISPLAY SUB-SCREEN MESSAGE                   
         SPACE 1                                                                
         TM    DSPINDS,DSPIDONE    TEST DISPLAY FINISHED                        
         BZ    FINPX2                                                           
         XC    APCURSOR,APCURSOR   RESET CURSOR POSN.                           
         SPACE 1                                                                
FLDSINPY CR    RB,RB               SET CC=EQUAL                                 
         B     XIT                                                              
         SPACE 1                                                                
FINPX2   OC    APCURSOR,APCURSOR   TEST CURSOR SET                              
         BNZ   *+12                                                             
         LA    R0,DSPINPH          SET CURSOR                                   
         ST    R0,APCURSOR           TO FIRST FIELD INPUT FIELD                 
         SPACE 1                                                                
FLDSINPN LTR   RB,RB               SET CC=NOT EQUAL                             
         B     XIT                                                              
         EJECT                                                                  
PFENTER  BAS   RE,VALFLDS          * ENTER KEY *                                
         BNE   FLDSINPX                                                         
         SPACE 1                                                                
         TM    DSPINDS,DSPIDSP+DSPIINP   TEST ACTION IS 'DISPLAY'               
         BO    *+8                         AND INPUT TO SUB-SCREEN              
         OI    DSPINDS,DSPIDONE    SET DISPLAY 'DONE'                           
         B     FLDSINPX                                                         
         SPACE 3                                                                
PFUNDF   XR    R0,R0               * UNDEFINED PF KEY *                         
         IC    R0,APPFKEY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SUBXTRA(2),DUB                                                   
         OI    SUBXINDS,SUBXIMID                                                
         MVC   SUBMSGNO,=AL2(CE#PFUND)                                          
         B     FLDSINPX                                                         
         SPACE 3                                                                
PFHELP   LA    R1,INPHELP          * HELP *                                     
         B     PFCURS                                                           
         SPACE 1                                                                
PFHELPX  LA    R1,INPHELPX         * HELP+ *                                    
         B     PFCURS                                                           
         SPACE 1                                                                
PFWRTX   LA    R1,INPWRTX          * WRITE+ *                                   
         B     PFCURS                                                           
         SPACE 1                                                                
PFREADX  LA    R1,INPREADX         * READ+ *                                    
         B     PFCURS                                                           
         SPACE 1                                                                
PFNOX    LA    R1,INPNOX           * NO+ *                                      
         SPACE 1                                                                
PFCURS   BAS   RE,SETCURS                                                       
         BNE   FLDSINPX                                                         
         BAS   RE,VALFLDS                                                       
         B     FLDSINPX                                                         
         EJECT                                                                  
PFWRT    LA    R1,INPWRT           * WRITE *                                    
         B     *+8                                                              
PFREAD   LA    R1,INPREAD          * READ *                                     
         B     *+8                                                              
PFNO     LA    R1,INPNO            * NO *                                       
         BAS   RE,SETCURS                                                       
         BNE   FLDSINPX                                                         
         SPACE 1                                                                
         L     RF,APCURSOR         TEST CURSOR ON LAST INPUT FIELD              
         LA    R0,DSPINPLH                                                      
         CR    RF,R0                                                            
         BE    PFFRWD              SCROLL FORWARD IF NECCESSERY                 
         BAS   RE,VALFLDS                                                       
         BNE   FLDSINPX                                                         
         LA    RF,FLDINPLQ(RF)     TAB CURSOR TO NEXT INPUT FIELD               
         TM    FHATD(RF),FHATPR      (UNLESS LAST ONE)                          
         BO    *+8                                                              
         ST    RF,APCURSOR                                                      
         B     FLDSINPX                                                         
         SPACE 3                                                                
PFBACK   BAS   RE,VALFLDS          * SCROLL BACK *                              
         BNE   FLDSINPX                                                         
         SPACE 1                                                                
         XR    RE,RE                                                            
         IC    RE,PAGENO                                                        
         BCT   RE,*+14             TEST CAN DO                                  
         MVCDD SUBXTRA,CT#CTSBK                                                 
         B     FLDSINPX                                                         
         SPACE 1                                                                
         STC   RE,PAGENO                                                        
         MVCDD SUBXTRA,CT#SCDBK                                                 
         BAS   RE,DISFLDS                                                       
         XC    APCURSOR,APCURSOR                                                
         B     FLDSINPX                                                         
         SPACE 3                                                                
PFFRWD   BAS   RE,VALFLDS          * SCROLL FORWARD *                           
         BNE   FLDSINPX                                                         
         SPACE 1                                                                
         IC    RE,PAGENO                                                        
         CLM   RE,1,NUMPAGES       TEST CAN DO                                  
         BNE   *+14                                                             
         MVCDD SUBXTRA,CT#CTSFD                                                 
         B     FLDSINPX                                                         
         SPACE 1                                                                
         LA    RE,1(RE)                                                         
         STC   RE,PAGENO                                                        
         MVCDD SUBXTRA,CT#SCDFD                                                 
         BAS   RE,DISFLDS                                                       
         XC    APCURSOR,APCURSOR                                                
         B     FLDSINPX                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE FIELD INPUT FIELDS                              *         
*                                                                     *         
* EXIT: CC=EQUAL IF ALL FIELDS HAVE BEEN VALIDATED                    *         
***********************************************************************         
         SPACE 1                                                                
VALFLDS  NTR1  ,                                                                
         XR    R8,R8                                                            
         IC    R8,PAGENO                                                        
         MH    R8,=Y(FTABPAGE)                                                  
         LA    R8,FTAB-FTABPAGE(R8)                                             
         USING FTABD,R8            R8=A(FIELD TABLE ENTRY)                      
         SPACE 1                                                                
         LA    R2,FLDINPLQ         R2=L(FIELD/INPUT FIELDS)                     
         LA    R3,DSPINPLH         R3=A(LAST INPUT FIELD)                       
         LA    R9,DSPINPH                                                       
         USING FHD,R9              R9=A(FIRST INPUT FIELD)                      
         SPACE 1                                                                
VFLDS2   TM    FHII,FHIIVA         TEST FIELD PREVIOUSLY VALIDATED              
         BO    VFLDS8                                                           
         ST    R9,APCURSOR                                                      
         OI    DSPINDS,DSPIINP                                                  
         SPACE 1                                                                
         CLC   FHDA(1),INPHELP                                                  
         BE    VFLDHELP                                                         
         CLC   FHDA(1),INPWRT                                                   
         BE    VFLDWRT                                                          
         CLC   FHDA(1),INPREAD                                                  
         BE    VFLDREAD                                                         
         CLC   FHDA(1),INPNO                                                    
         BE    VFLDNO                                                           
         B     VFLDUNDF                                                         
         SPACE 1                                                                
VFLDS6   GOTO1 DISFINP,PARM,FHD,FTABD                                           
         SPACE 1                                                                
VFLDS8   LA    R8,FTABL(R8)                                                     
         CLI   FTABD,EOT           TEST E-O-T                                   
         BE    *+8                                                              
         BXLE  R9,R2,VFLDS2                                                     
         SPACE 1                                                                
         ICM   R0,3,SUBSTAB        TEST TAB TO NEXT PAGE                        
         BZ    VALFLDSY                                                         
         SPACE 1                                                                
         IC    RE,PAGENO           SCROLL FORWARD                               
         LA    RE,1(RE)                                                         
         STC   RE,PAGENO                                                        
         BAS   RE,DISFLDS                                                       
         SPACE 1                                                                
         STCM  R0,3,DSPINP         SET FIRST INPUT OF SCREEN TO TAB             
         NI    DSPINPH+FHIID,FF-FHIIVA                                          
         BAS   RE,VALFLDS          VALIDATE NEW SCREEN                          
         SPACE 1                                                                
VALFLDSN LTR   RB,RB                                                            
         B     XIT                                                              
VALFLDSY CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 5                                                                
VFLDUNDF CLI   FHDA,C' '           * INVALID INPUT *                            
         BH    VFUNDF2             TEST FIELD EMPTY                             
         TM    DSPINDS,DSPIDSP     TEST ACTION IS DISPLAY                       
         BO    VFLDS6                                                           
         MVC   SUBMSGNO,=AL2(FVFNONE)                                           
         B     *+10                                                             
VFUNDF2  MVC   SUBMSGNO,=AL2(FVFNOTV)                                           
         BAS   RE,SUBHELP                                                       
         B     VALFLDSN                                                         
         SPACE 3                                                                
VFLDWRT  TM    DSPINDS,DSPIDSP     * WRITE *                                    
         BO    VFLDS6                                                           
         CLC   INPWRTX,FHDA                                                     
         BE    VFLDWRTX                                                         
         SPACE 1                                                                
         GOTO1 TESTBIT,PARM,(FTCODE,AVAILWRT)                                   
         BO    VFLDWRT2            TEST FIELD CAN BE SET TO WRITE               
         MVC   SUBMSGNO,=AL2(CE#FOVFR)                                          
         B     VALFLDSN                                                         
         SPACE 1                                                                
VFLDWRT2 GOTO1 SETBIT,PARM,(FTCODE,WRITETAB)                                    
         GOTO1 SETBIT,PARM,(FTCODE,READTAB)                                     
         B     VFLDS6                                                           
         SPACE 1                                                                
VFLDWRTX GOTO1 SETBIT,PARM,(FTCODE,READTAB)  * WRITE+ *                         
         GOTO1 TESTBIT,PARM,(FTCODE,AVAILWRT)  SET TO READ ONLY                 
         BZ    VFLDWRT6                          IF WRITE NOT AVAILABLE         
         GOTO1 SETBIT,PARM,(FTCODE,WRITETAB)                                    
         SPACE 1                                                                
VFLDWRT6 LA    R8,FTABL(R8)        BUMP R8 TO NEXT TABLE ENTRY                  
         CLI   FTABD,EOT                                                        
         BNE   VFLDWRTX                                                         
         SPACE 1                                                                
         BAS   RE,DISFLDS                                                       
         B     VALFLDSN                                                         
         SPACE 3                                                                
VFLDREAD TM    DSPINDS,DSPIDSP     * READ *                                     
         BO    VFLDS6                                                           
         CLC   INPREADX,FHDA                                                    
         BE    VFLDRDX                                                          
         GOTO1 RESETBIT,PARM,(FTCODE,WRITETAB)                                  
         GOTO1 SETBIT,PARM,(FTCODE,READTAB)                                     
         B     VFLDS6                                                           
         SPACE 1                                                                
VFLDRDX  GOTO1 RESETBIT,PARM,(FTCODE,WRITETAB)  * READ+ *                       
         GOTO1 SETBIT,PARM,(FTCODE,READTAB)                                     
         LA    R8,FTABL(R8)                                                     
         CLI   FTABD,EOT                                                        
         BNE   VFLDRDX                                                          
         BAS   RE,DISFLDS                                                       
         B     VALFLDSN                                                         
         SPACE 3                                                                
VFLDNO   TM    DSPINDS,DSPIDSP     * NO *                                       
         BO    VFLDS6                                                           
         CLC   INPNOX,FHDA                                                      
         BE    VFLDNOX                                                          
         GOTO1 RESETBIT,PARM,(FTCODE,WRITETAB)                                  
         GOTO1 RESETBIT,PARM,(FTCODE,READTAB)                                   
         B     VFLDS6                                                           
         SPACE 1                                                                
VFLDNOX  GOTO1 RESETBIT,PARM,(FTCODE,WRITETAB)  * NO+ *                         
         GOTO1 RESETBIT,PARM,(FTCODE,READTAB)                                   
         LA    R8,FTABL(R8)                                                     
         CLI   FTABD,EOT                                                        
         BNE   VFLDNOX                                                          
         BAS   RE,DISFLDS                                                       
         B     VALFLDSN                                                         
         SPACE 3                                                                
VFLDHELP GOTO1 DISHELP,FTABD       * HELP *                                     
         SPACE 1                                                                
         CLC   INPHELPX,FHDA       TEST HELP+                                   
         BNE   VFLDH4                                                           
         CLI   FTABD+FTABL,EOT     TEST LAST FIELD                              
         BE    VFLDH4                                                           
         CR    R9,R3               TEST LAST FOR SCREEN                         
         BNE   VFLDH2                                                           
         MVC   SUBSTAB,INPHELPX    SET TAB TO NEXT SCREEN                       
         B     VFLDH4                                                           
         SPACE 1                                                                
VFLDH2   MVC   FHDA+FLDINPLQ(L'INPHELPX),INPHELPX  BUMP HELP+ TO                
         NI    FHII+FLDINPLQ,FF-FHIIVA               NEXT INPUT                 
         OI    FHOI+FLDINPLQ,FHOITR                                             
         SPACE 1                                                                
VFLDH4   GOTO1 DISFINP,PARM,FHD,FTABD                                           
         B     VALFLDSN                                                         
         SPACE 1                                                                
         DROP  R8,R9                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY FIELDS LIST                                      *         
***********************************************************************         
         SPACE 1                                                                
DISFLDS  NTR1  ,                                                                
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
         MH    R8,=Y(FTABPAGE)                                                  
         LA    R8,FTAB-FTABPAGE(R8)                                             
         USING FTABD,R8            R8=A(FIELD TABLE ENTRY)                      
         SPACE 1                                                                
         LA    R2,FLDINPLQ         R2=L(FIELD/INPUT FIELDS)                     
         LA    R3,DSPFLDLH         R3=A(LAST FIELD ON SCREEN)                   
         LA    R9,DSPFLDH                                                       
         USING DSPFLDH,R9          R9=A(FIRST FIELD ON DISPLAY)                 
         SPACE 1                                                                
DFLDS2   OI    DSPINPH+FHOID,FHOITR  TRANSMIT FIELDS                            
         OI    DSPFLDH+FHOID,FHOITR                                             
         SPACE 1                                                                
         CLI   FTABD,EOT            TEST ANY MORE FIELDS ON SCREEN              
         BNE   DFLDS4                                                           
         XC    DSPINP,DSPINP       CLEAR                                        
         OI    DSPINPH+FHATD,FHATPR PROTECT                                     
         XC    DSPFLD,DSPFLD       CLEAR                                        
         B     DFLDS10                                                          
         SPACE 1                                                                
DFLDS4   GOTO1 ADISDIC,PARM,(SAVOVS,L'DSPFLD),(C'L',FTWRD)                      
         MVC   DSPFLD,APWORK       COPY FIELD WORD                              
         GOTO1 DISFINP,PARM,DSPFLDH,FTABD                                       
         NI    DSPINPH+FHATD,FF-FHATPR                                          
         SPACE 1                                                                
         LA    R8,FTABL(R8)        BUMP R8 TO NEXT TABLE ENTRY                  
         SPACE 1                                                                
DFLDS10  BXLE  R9,R2,DFLDS2        BUMP R9 TO NEXT FIELD FIELD                  
         SPACE 1                                                                
DISFLDSX B     XIT                                                              
         DROP  R8,R9                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY FIELD INPUT                                      *         
*                                                                     *         
* NTRY: P1=A(FIELD WORD/INPUT FIELD)                                  *         
*       P2=A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DISFINP  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
FLD      USING DSPFLDH,R2          R2=A(FIELD FIELD)                            
         USING FTABD,R3            R3=A(FIELD TABLE ENTRY)                      
         SPACE 1                                                                
         TM    FLD.DSPFLDH+FHATD,FHATPR                                         
         BO    *+8                 ADJUST R2 IF IS THE INPUT FIELD              
         SH    R2,=Y(DSPINPH-DSPFLDH)                                           
         SPACE 1                                                                
         OI    FLD.DSPFLDH+FHOID,FHOITR TRANSMIT                                
         OI    FLD.DSPINPH+FHOID,FHOITR                                         
         OI    FLD.DSPINPH+FHIID,FHIIVA SET INPUT VALIDATED                     
         SPACE 1                                                                
         GOTO1 TESTBIT,PARM,(FTCODE,WRITETAB)                                   
         BZ    DFINP2              TEST FIELD IS WRITE                          
         MVC   FLD.DSPINP,INPWRT                                                
         OI    FLD.DSPFLDH+FHATD,FHATHI                                         
         OI    FLD.DSPINPH+FHATD,FHATHI                                         
         B     XIT                                                              
         SPACE 1                                                                
DFINP2   GOTO1 TESTBIT,PARM,(FTCODE,READTAB)                                    
         BZ    DFINP4              TEST FIELD IS READ                           
         MVC   FLD.DSPINP,INPREAD                                               
         OI    FLD.DSPFLDH+FHATD,FHATHI                                         
         NI    FLD.DSPINPH+FHATD,FF-FHATHI                                      
         B     XIT                                                              
         SPACE 1                                                                
DFINP4   MVC   FLD.DSPINP,INPNO    FIELD IS NOT VALID                           
         NI    FLD.DSPFLDH+FHATD,FF-FHATHI                                      
         NI    FLD.DSPINPH+FHATD,FF-FHATHI                                      
         B     XIT                                                              
         SPACE 1                                                                
         DROP  FLD,R3                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET CURSOR INPUT FIELD                                   *         
*                                                                     *         
* NTRY: R1=A(OUTPUT FOR FIELD)                                        *         
* EXIT: CC=EQUAL IF CURSOR VALIDLY WITHIN FIELDS SUB-SCREEN           *         
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
* ROUTINE TO DISPLAY HELP FOR FIELD                                   *         
*                                                                     *         
* NTRY: R1=A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DISHELP  NTR1  ,                                                                
         LR    R2,R1                                                            
         USING FTABD,R2            R2=A(FIELD ENTRY)                            
         XC    WORK,WORK                                                        
         SPACE 1                                                                
         GOTO1 ADISDIC,PARM,(SAVOVS,HELPWRDL),(C'U',FTWRD)                      
         MVC   WORK+1(HELPWRDL),APWORK                                          
         SPACE 1                                                                
         IC    RF,SAVOVS                                                        
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),FTDSC),FTWRD                                  
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
SMSG2    LA    R1,GTPARM                                                        
         USING GETTXTD,R1          R1=A(GETTXT PARAMETER LIST)                  
         XC    GTBLOCK,GTBLOCK                                                  
         SPACE 1                                                                
         OC    SUBMSGNO,SUBMSGNO   TEST MESSAGE NUMBER SET                      
         BNZ   SMSG4                                                            
         TM    DSPINDS,DSPINONE    SET DEFAULT NO FIELDS MESSAGE                
         BZ    *+14                                                             
         MVC   GTMSGNO,=AL2(CE#NFDTL)                                           
         B     SMSG10                                                           
         MVI   GTMTYP,GTMINF       SET DEFAULT FIELDS DISPLAYED MESSAGE         
         MVC   GTMSGNO,=AL2(CI#FLDIS)                                           
         OC    SUBXTRA,SUBXTRA                                                  
         BNZ   *+8                                                              
         BAS   RE,SUBHELP                                                       
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
*  ROUTINE TO PUT HELP FOR SUB-ACTIONS IN SUBXTRA                     *         
***********************************************************************         
         SPACE 1                                                                
SUBHELP  NTR1  ,                                                                
         LA    R4,SUBXTRA                                                       
         GOTO1 SHELP,PARM,FC@WRITE,(L'FC@WRITE,FC@WRITE)                        
         GOTO1 (RF),(R1),FC@READ,(L'FC@READ,FC@READ)                            
         GOTO1 (RF),(R1),FC@NO,(L'FC@NO,FC@NO)                                  
         GOTO1 (RF),(R1),=C'?',(L'SE@HELP,SE@HELP)                              
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         MVI   0(R4),0                                                          
         B     XIT                                                              
         SPACE 1                                                                
SHELP    LR    R0,RE                                                            
         LM    R2,R3,0(R1)                                                      
         MVC   0(1,R4),0(R2)                                                    
         MVI   1(R4),C'='                                                       
         XR    RE,RE                                                            
         IC    RE,4(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),0(R3)                                                    
         LA    R4,3(R4,RE)                                                      
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C','                                                       
         LA    R4,3(R4)                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY         INITIALIZE RECORD KEY                        
         USING SAFCREC,R2                                                       
         XC    SAFCKEY,SAFCKEY                                                  
         MVI   SAFCTYP,SAFCTYPQ                                                 
         MVI   SAFCSUB,SAFCSUBQ                                                 
         MVC   SAFCAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   SAFCAGY,OPTAGY                                                   
         SPACE 1                                                                
VSEL2    GOTO1 AVALOVPG,PARM,(X'80',LSTSYSH),LSTPGMH,0                          
         BNE   VALSELX                                                          
         MVC   SAFCOVPG,APHALF                                                  
         SPACE 1                                                                
VSEL4    CLI   LSTUIDH+FHILD,0     TEST USER-ID ENTERD                          
         BE    VSEL6                                                            
         GOTO1 AVALUID,LSTUIDH                                                  
         BNE   VALSELX                                                          
         MVC   SAFCUID,APHALF                                                   
         SPACE 1                                                                
VSEL6    CLI   LSTACGH+FHILD,0     TEST ACCESS GROUP ENTERED                    
         BE    VSEL8                                                            
         GOTO1 AVALACG,LSTACGH                                                  
         BNE   VALSELX                                                          
         MVC   SAFCAGN,APHALF                                                   
         SPACE 1                                                                
VSEL8    MVC   SELOVS,SAFCOVS                                                   
         MVC   SELPGM,SAFCPGM                                                   
         MVC   SELUID,SAFCUID                                                   
         MVC   SELAGN,SAFCAGN                                                   
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
         USING SAFCREC,R2                                                       
         MVC   IOKEY(L'SAFCKEY),APRECKEY                                        
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
         CLC   SAFCKEY(SAFCOVS-SAFCKEY),IOKEYSAV                                
         BNE   GETSELN             TEST RECORD TYPE/AGENCY                      
         SPACE 1                                                                
         GOTO1 ATSTSYS,SAFCOVS     TEST SYSTEM CONNECTABLE TO                   
         BNE   GSEL4                                                            
         CLI   SELOVS,0            MATCH ON SYSTEM/PROGRAM                      
         BE    GSEL8                                                            
         CLC   SAFCOVS,SELOVS                                                   
         BNE   GETSELN                                                          
         CLI   SELPGM,0                                                         
         BE    GSEL8                                                            
         CLC   SAFCPGM,SELPGM                                                   
         BNE   GETSELN                                                          
         SPACE 1                                                                
GSEL8    GOTO1 ATSTUID,SAFCUID     TEST USER-ID CONNECTABLE TO                  
         BNE   GSEL4                                                            
         OC    SELUID,SELUID       MATCH ON USER-ID                             
         BZ    GSEL10                                                           
         CLC   SAFCUID,SELUID                                                   
         BNE   GSEL4                                                            
         SPACE 1                                                                
GSEL10   OC    SELAGN,SELAGN       MATCH ON ACCESS GROUP                        
         BZ    GSEL12                                                           
         CLC   SAFCAGN,SELAGN                                                   
         BNE   GSEL4                                                            
         SPACE 1                                                                
GSEL12   OC    SAFCUID(L'SAFCUID+L'SAFCAGN),SAFCUID                             
         BNZ   *+12                TEST MAIN AGENCY RECORD                      
         TM    CUSTAT,CUSDDS       TEST DDS TERMINAL                            
         BZ    GSEL4                                                            
         MVC   APRECKEY(L'SAFCKEY),SAFCKEY                                      
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
         USING SAFCREC,R2                                                       
         SPACE 1                                                                
         L     R4,APPARM                                                        
         USING LSTACTH,R4          R4=A(LIST/SELECT LINE)                       
         SPACE 1                                                                
         GOTO1 ADISSYS,SAFCOVS                                                  
         MVC   LSTLSYS,APWORK      SYSTEM NAME                                  
         SPACE 1                                                                
         OC    SAFCPGM,SAFCPGM                                                  
         BZ    DSEL2                                                            
         GOTO1 ADISPGM,PARM,(SAFCOVS,SAFCPGM)                                   
         MVC   LSTLPGM,APWORK      PROGRAM NAME                                 
         SPACE 1                                                                
DSEL2    OC    SAFCUID,SAFCUID                                                  
         BZ    DSEL4                                                            
         GOTO1 ADISUID,SAFCUID                                                  
         MVC   LSTLUID,APWORK      USER-ID                                      
         SPACE 1                                                                
DSEL4    OC    SAFCAGN,SAFCAGN                                                  
         BZ    DISSELX                                                          
         GOTO1 ADISACG,SAFCAGN                                                  
         MVC   LSTLACG,APWORK      ACCESS GROUP                                 
         SPACE 1                                                                
DISSELX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF FIELD RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         USING SAFCREC,R2                                                       
         SPACE 1                                                                
         GOTO1 ADISSYS,SAFCOVS     SYSTEM NAME                                  
         MVC   DSPSYS,APWORK                                                    
         SPACE 1                                                                
         CLI   SAFCPGM,0           PROGRAM NAME                                 
         BE    DKEY2                                                            
         GOTO1 ADISPGM,PARM,(SAFCOVS,SAFCPGM)                                   
         MVC   DSPPGM,APWORK                                                    
         MVI   DSPPGMH+FHILD,L'DSPPGM                                           
         SPACE 1                                                                
DKEY2    OC    SAFCUID,SAFCUID     USER-ID                                      
         BZ    DKEY4                                                            
         GOTO1 ADISUID,SAFCUID                                                  
         MVC   DSPUID,APWORK                                                    
         MVI   DSPUIDH+FHILD,L'DSPUID                                           
         SPACE 1                                                                
DKEY4    OC    SAFCAGN,SAFCAGN     ACCESS GROUP                                 
         BZ    DISKEYX                                                          
         GOTO1 ADISACG,SAFCAGN                                                  
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
         CLC   REPVAL,INPWRT                                                    
         BE    VREQ9                                                            
         CLC   REPVAL,INPREAD                                                   
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
         MVCDD REPDESC,CT#FCONL    SET REPORT DESCRIPTION                       
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
         MVI   REPPFLDS,C'C'                                                    
         MVI   REPPEND,C'R'                                                     
         DROP  RF                                                               
         SPACE 1                                                                
PREP2    LA    R2,IOKEY            SET UP INITIAL KEY                           
         USING SAFCREC,R2          R2=A(FCONTROL RECORD KEY)                    
         XC    SAFCKEY,SAFCKEY                                                  
         MVI   SAFCTYP,SAFCTYPQ                                                 
         MVI   SAFCSUB,SAFCSUBQ                                                 
         MVC   SAFCAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   SAFCAGY,OPTAGY                                                   
         MVC   SAFCOVS,SELOVS                                                   
         MVC   SAFCPGM,SELPGM                                                   
         MVC   SAFCUID,SELUID                                                   
         MVC   SAFCAGN,SELAGN                                                   
         L     R2,AIOAREA1                                                      
         SPACE 1                                                                
         XC    SAVOVPG,SAVOVPG                                                  
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
PREP10   LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   PREP30                                                           
         CLC   SAFCKEY(SAFCOVS-SAFCKEY),IOKEYSAV                                
         BNE   PREP30              TEST RECORD TYPE/AGENCY                      
         SPACE 1                                                                
         OC    SAFCUID(L'SAFCUID+L'SAFCAGN),SAFCUID                             
         BZ    PREP10              IGNORE AGENCY RECORDS                        
         SPACE 1                                                                
         GOTO1 ATSTSYS,SAFCOVS     TEST USER CAN CONNECT TO SYSTEM              
         BNE   PREP10                                                           
         CLI   SELOVS,0            MATCH ON SYSTEM/PROGRAM                      
         BE    PREP12                                                           
         CLC   SAFCOVS,SELOVS                                                   
         BNE   PREP30                                                           
         CLI   SELPGM,0                                                         
         BE    PREP12                                                           
         CLC   SAFCPGM,SELPGM                                                   
         BNE   PREP30                                                           
         SPACE 1                                                                
PREP12   GOTO1 ATSTUID,SAFCUID     TEST USER CAN CONNET TO USER-ID              
         BNE   PREP10                                                           
         OC    SELUID,SELUID       MATCH ON USER-ID                             
         BZ    PREP14                                                           
         CLC   SAFCUID,SELUID                                                   
         BNE   PREP10                                                           
         SPACE 1                                                                
PREP14   OC    SELAGN,SELAGN       MATCH ON ACCESS GROUP                        
         BZ    PREP20                                                           
         CLC   SAFCAGN,SELAGN                                                   
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
* ROUTINE TO PRINT FCONTROL RECORD DETAILS                            *         
***********************************************************************         
         SPACE 1                                                                
PRTRCD   NTR1  ,                                                                
         L     R2,AIOAREA1                                                      
         USING SAFCREC,R2                                                       
         SPACE 1                                                                
         GOTO1 ADISSYS,SAFCOVS     PRINT SYSTEM                                 
         MVC   REPPSYS,APWORK                                                   
         SPACE 1                                                                
         CLI   SAFCPGM,0           PRINT PROGRAM                                
         BE    PRCD02                                                           
         GOTO1 ADISPGM,PARM,(SAFCOVS,SAFCPGM)                                   
         MVC   REPPPGM,APWORK                                                   
         SPACE 1                                                                
PRCD02   OC    SAFCUID,SAFCUID     PRINT USER-ID                                
         BZ    PRCD04                                                           
         GOTO1 ADISUID,SAFCUID                                                  
         MVC   REPPUID,APWORK                                                   
         SPACE 1                                                                
PRCD04   OC    SAFCAGN,SAFCAGN     PRINT ACCESS GROUP                           
         BZ    PRCD06                                                           
         GOTO1 ADISACG,SAFCAGN                                                  
         MVC   REPPACG,APWORK                                                   
         SPACE 1                                                                
PRCD06   CLC   SAVOVPG,SAFCOVPG    TEST CHANGE OF PROGRAM                       
         BE    PRCD16                                                           
         MVC   SAVOVPG,SAFCOVPG                                                 
         CLI   SAFCPGM,0                                                        
         BNE   PRCD08                                                           
         GOTO1 ASUBDIC,0                                                        
         B     PRCD10                                                           
         PUSH  USING                                                            
         USING SAPGREC,IOKEY                                                    
PRCD08   XC    SAPGKEY,SAPGKEY                                                  
         MVI   SAPGTYP,SAPGTYPQ                                                 
         MVI   SAPGSUB,SAPGSUBQ                                                 
         MVC   SAPGOVPG,SAVOVPG                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOAREA3                                                      
         GOTO1 ASUBDIC                                                          
         POP   USING                                                            
         SPACE 1                                                                
PRCD10   MVC   REPM1,SPACES                                                     
         LA    RF,REPM1            SET UP MIDLINES                              
         USING REPP1,RF                                                         
         MVCDD REPPSYS,CT#SYS,L                                                 
         MVCDD REPPPGM,CT#PROG,L                                                
         MVCDD REPPUID,CT#USRID,L                                               
         MVCDD REPPACG,CT#GROUP,L                                               
         MVCDD REPPFLDS,CT#AFD,L   AVAILABLE FIELDS                             
         CLC   SELVAL,INPYES                                                    
         BNE   *+10                                                             
         MVCDD REPPFLDS,CT#VALFD,L VALID FIELDS                                 
         CLC   SELVAL,INPWRT                                                    
         BNE   *+10                                                             
         MVCDD REPPFLDS,CT#VALWF,L VALID WRITE FIELDS                           
         CLC   SELVAL,INPREAD                                                   
         BNE   *+10                                                             
         MVCDD REPPFLDS,CT#VALRF,L VALID READ ONLY FIELDS                       
         CLC   SELVAL,INPNO                                                     
         BNE   *+10                                                             
         MVCDD REPPFLDS,CT#INVFD,L INVALID FIELDS                               
         TM    DICINDS,DICIFCTO                                                 
         BZ    *+10                                                             
         MVCDD  REPPFLDS,CT#FIELD  OUTPUT 'FIELD' IF NOT WRITE/READ/NO          
         MVC   REPP2,REPP1                                                      
         LA    RF,REPP2            SET MIDLINE2 TO MIDLINE 1 UNDERLINED         
         MVI   REPPSYS,CT#ESUL                                                  
         MVI   REPPPGM,CT#ESUL                                                  
         MVI   REPPUID,CT#ESUL                                                  
         MVI   REPPACG,CT#ESUL                                                  
         MVI   REPPFLDS,CT#ESUL                                                 
         DROP  RF                                                               
         SPACE 1                                                                
PRCD16   MVC   SAVKEY,SAFCKEY                                                   
         GOTO1 FINDEL,SAFCWELQ                                                  
         GOTO1 ABLDBITT,PARM,(R3),WRITETAB                                      
         GOTO1 FINDEL,SAFCRELQ                                                  
         GOTO1 ABLDBITT,PARM,(R3),READTAB                                       
         BAS   RE,INITFTAB                                                      
         NC    WRITETAB,AVAILWRT                                                
         NC    READTAB,AVAILRD                                                  
         DROP  R2                                                               
         SPACE 1                                                                
PRCD18   L     R8,AFTAB                                                         
         USING FTABD,R8            R8=A(FIELD TABLE ENTRY)                      
         XR    R2,R2                                                            
         SPACE 1                                                                
PRCD20   CLI   FTABD,EOT                                                        
         BE    PRCD40                                                           
         SPACE 1                                                                
         MVC   PRTVAL,INPNO        SET NO/WRITE/READ                            
         GOTO1 TESTBIT,PARM,(FTCODE,WRITETAB)                                   
         BZ    *+14                                                             
         MVC   PRTVAL,INPWRT                                                    
         B     PRCD22                                                           
         GOTO1 TESTBIT,PARM,(FTCODE,READTAB)                                    
         BZ    *+10                                                             
         MVC   PRTVAL,INPREAD                                                   
         SPACE 1                                                                
PRCD22   CLI   SELVAL,0            TEST VALIDATION FILTER                       
         BE    PRCD24                                                           
         CLC   SELVAL,PRTVAL                                                    
         BE    PRCD24                                                           
         CLC   SELVAL,INPYES                                                    
         BNE   PRCD30                                                           
         CLC   PRTVAL,INPNO                                                     
         BE    PRCD30                                                           
         SPACE 1                                                                
PRCD24   GOTO1 ADISDIC,PARM,(SAVOVS,L'REPPFWRD),(C'L',FTWRD)                    
         SPACE 1                                                                
         LTR   R2,R2               TEST FIRST FIELD PRINTED                     
         BZ    PRCD26                                                           
         LA    R0,L'REPPFLD        BUMP R2 TO NEXT COLUMN                       
         LA    R1,REPPFLDS+L'REPPFLDS-1                                         
         BXLE  R2,R0,PRCD28                                                     
         SPACE 1                                                                
         GOTO1 VREPORT,REPD        PRINT IF ROW IS FULL                         
         SPACE 1                                                                
PRCD26   LA    R2,REPPFLDS         RESET R2 TO FIRST COLUMN                     
         SPACE 1                                                                
         USING REPPFLD,R2                                                       
PRCD28   MVC   REPPFWRD,APWORK     PRINT WORD/VALIDATION                        
         MVC   REPPFVAL,PRTVAL                                                  
         DROP  R2                                                               
         SPACE 1                                                                
PRCD30   LA    R8,FTABL(R8)                                                     
         B     PRCD20                                                           
         DROP  R8                                                               
         SPACE 1                                                                
PRCD40   LTR   R2,R2               TEST ANY FIELDS LISTED                       
         BNZ   PRCD42                                                           
         MVI   REPPFWRD,C'*'                                                    
         MVCDD REPPFWRD+1(L'REPPFWRD),CT#NONE,C                                 
         MVI   REPPFWRD+L'REPPFWRD+1,C'*'                                       
PRCD42   GOTO1 VREPORT,REPD        PRINT LAST LINE                              
         SPACE 1                                                                
PRTRCDX  B     XIT                                                              
         SPACE 3                                                                
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FIELD TABLE                                              *         
*                                                                     *         
* NTRY: SAVKEY=CURRENT FCONTROL RECORD KEY                            *         
* EXIT: FCRINDS, PRNTKEY INITIALIZED                                  *         
*       FIELDS TABLE SET UP                                           *         
***********************************************************************         
         SPACE 1                                                                
INITFTAB NTR1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* - SET UP FCONTROL RECORD PARENT KEY                                 *         
***********************************************************************         
         SPACE 1                                                                
         MVI   FCRINDS,0           RESET FCONTROL RECORD INDICATORS             
         SPACE 1                                                                
         MVC   WORKKEY,SAVKEY      SET UP PARENT KEY IN WORKKEY                 
         LA    R2,WORKKEY                                                       
         USING SAFCREC,R2                                                       
         SPACE 1                                                                
         OC    SAFCAGN,SAFCAGN     TEST FOR ACCESS GROUP                        
         BZ    IFTAB2                                                           
         OI    FCRINDS,FCRIACGP                                                 
         XC    SAFCAGN,SAFCAGN                                                  
         B     IFTAB6                                                           
         SPACE 1                                                                
IFTAB2   OC    SAFCUID,SAFCUID     TEST FOR USER-ID                             
         BZ    IFTAB4                                                           
         XC    SAFCUID,SAFCUID                                                  
         B     IFTAB6                                                           
         SPACE 1                                                                
IFTAB4   OI    FCRINDS,FCRIAGY     MAIN AGENCY RECORD                           
         XC    SAFCAGY,SAFCAGY                                                  
         SPACE 1                                                                
IFTAB6   OC    SAFCUID,SAFCUID     TEST PARENT IS FOR USER-ID                   
         BNZ   *+8                                                              
         OI    FCRINDS,FCRIPAGY      OR FOR AGENCY                              
         SPACE 1                                                                
         CLC   PRNTKEY,SAFCKEY     TEST CHANGE OF PARENT                        
         BNE   IFTAB8                                                           
         OI    FCRINDS,FCRISIBL                                                 
         B     IFTABYES                                                         
         SPACE 1                                                                
IFTAB8   MVC   PRNTKEY,SAFCKEY                                                  
         EJECT                                                                  
***********************************************************************         
*  - SET UP FIELDS TABLE                                              *         
***********************************************************************         
         SPACE 1                                                                
         MVI   AVAILWRT,FF         SET EVERYTHING AVAILABLE                     
         MVC   AVAILWRT+1(L'AVAILWRT-1),AVAILWRT                                
         MVC   AVAILRD,AVAILWRT                                                 
         SPACE 1                                                                
         TM    FCRINDS,FCRIAGY     TEST MAIN AGENCY RECORD                      
         BO    IFTAB10                                                          
         LA    R2,IOKEY              NO - MERGE IN MAIN AGENCY RECORD           
         USING SAFCREC,R2                                                       
         MVC   SAFCKEY,PRNTKEY                                                  
         XC    SAFCUID,SAFCUID                                                  
         BAS   RE,MERGPRNT                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#DRANF)                                           
         B     IFTABNO                                                          
         SPACE 1                                                                
         TM    FCRINDS,FCRIPAGY    TEST PARENT IS MAIN AGENCY RECORD            
         BO    IFTAB10                                                          
         MVC   SAFCKEY,PRNTKEY       NO - MERGE IN USER-ID RECORD               
         BAS   RE,MERGPRNT                                                      
         DROP  R2                                                               
         SPACE 1                                                                
IFTAB10  LA    R2,IOKEY                                                         
         USING SAFDREC,R2          R2=A(FIELD RECORD KEY)                       
         XC    SAFDKEY,SAFDKEY                                                  
         MVI   SAFDTYP,SAFDTYPQ                                                 
         MVI   SAFDSUB,SAFDSUBQ                                                 
         MVC   SAFDOVPG,SAVOVPG                                                 
         L     R2,AIOAREA1                                                      
         L     R8,AFTAB                                                         
         USING FTABD,R8            R8=A(FIELD TABLE ENTRY)                      
         XC    WORKBITT,WORKBITT                                                
         SPACE 1                                                                
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
IFTAB12  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   IFTAB20             TEST FIELD RECORD FOR PROGRAM                
         CLC   SAFDKEY(SAFDFCD-SAFDKEY),IOKEYSAV                                
         BNE   IFTAB20                                                          
         SPACE 1                                                                
         GOTO1 TESTBIT,PARM,(SAFDFCD,AVAILRD)  TEST CAN AT LEAST READ           
         BZ    IFTAB12                                                          
         SPACE 1                                                                
         GOTO1 SETBIT,PARM,(SAFDFCD,WORKBITT)                                   
         MVC   FTCODE,SAFDFCD      ADD FIELD TABLE ENTRY                        
         GOTO1 FINDEL,SAFLDELQ                                                  
         MVC   FTWRD,SAFLDWRD-SAFLDD(R3)                                        
         MVC   FTDSC,SAFLDDSC-SAFLDD(R3)                                        
         SPACE 1                                                                
         LA    R8,FTABL(R8)                                                     
         B     IFTAB12                                                          
         SPACE 1                                                                
IFTAB20  MVI   FTABD,EOT           SET END OF TABLE                             
         NC    AVAILRD,WORKBITT    ENSURE NO EXTRA BITS ARE ON                  
         NC    AVAILWRT,WORKBITT                                                
         DROP  R8,R2                                                            
         SPACE 1                                                                
         MVI   PAGENO,1            SET INITIAL PAGE INFO                        
         LA    R0,FTAB-FTABPAGE+FTABL                                           
         SR    R8,R0                                                            
         SRDL  R8,32                                                            
         LA    R0,FTABPAGE                                                      
         DR    R8,R0                                                            
         STC   R9,NUMPAGES                                                      
         LTR   R9,R9               TEST LIST IS EMPTY                           
         BNZ   IFTABYES                                                         
         OI    DSPINDS,DSPINONE                                                 
         OI    FCRINDS,FCRIREAD                                                 
         XC    PRNTKEY,PRNTKEY                                                  
         SPACE 1                                                                
IFTABYES CR    RB,RB                                                            
         B     XIT                                                              
IFTABNO  LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MERGE IN PARENT FCONTROL RECORD DETAILS                  *         
*                                                                     *         
* NTRY: IOKEY=KEY OF PARENT RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
MERGPRNT NTR1  ,                                                                
         L     R2,AIOAREA1                                                      
         USING SAFCREC,R2          READ PARENT RECORD                           
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   MPRNTN                                                           
         SPACE 1                                                                
         GOTO1 FINDEL,SAFCWELQ     AND IN PARENT BIT TABLES                     
         GOTO1 ABLDBITT,PARM,(R3),WORKBITT                                      
         NC    AVAILWRT,WORKBITT                                                
         GOTO1 FINDEL,SAFCRELQ                                                  
         GOTO1 ABLDBITT,PARM,(R3),WORKBITT                                      
         NC    AVAILRD,WORKBITT                                                 
         OC    AVAILRD,AVAILWRT                                                 
         SPACE 1                                                                
MPRNTY   CR    RB,RB                                                            
         B     XIT                                                              
MPRNTN   LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PFK LINES                                          *         
***********************************************************************         
         SPACE 1                                                                
BLDPFK   NTR1  ,                                                                
         XC    DSPPFK1,DSPPFK1                                                  
         OI    DSPPFK1H+FHOID,FHOITR                                            
         MVC   DSPPF,SE@PF                                                      
         MVC   DSP01,=C'01='                                                    
         MVC   DSPHELP(L'SE@HELP),SE@HELP                                       
         MVC   DSP03,=C'03='                                                    
         MVC   DSPWRT(L'FC@WRITE),FC@WRITE                                      
         MVC   DSP05,=C'05='                                                    
         MVC   DSPREAD(L'FC@READ),FC@READ                                       
         MVC   DSP07,=C'07='                                                    
         MVC   DSPNO(L'FC@NO),FC@NO                                             
         MVC   DSP09,=C'09='                                                    
         MVC   DSPBACK(L'SE@BACK),SE@BACK                                       
         MVC   DSP11,=C'11='                                                    
         MVC   DSPNXTLT(L'SE@NXTLT),SE@NXTLT                                    
*                                                                               
         XC    DSPPFK2,DSPPFK2                                                  
         OI    DSPPFK2H+FHOID,FHOITR                                            
         MVC   DSPPF2,SE@PF                                                     
         MVC   DSP02,=C'02='                                                    
         MVC   DSPHELPX,DSPHELP                                                 
         GOTO1 ADDPLUS,DSPHELPX                                                 
         MVC   DSP04,=C'04='                                                    
         MVC   DSPWRTX,DSPWRT                                                   
         GOTO1 ADDPLUS,DSPWRTX                                                  
         MVC   DSP06,=C'06='                                                    
         MVC   DSPREADX,DSPREAD                                                 
         GOTO1 ADDPLUS,DSPREADX                                                 
         MVC   DSP08,=C'08='                                                    
         MVC   DSPNOX,DSPNO                                                     
         GOTO1 ADDPLUS,DSPNOX                                                   
         MVC   DSP10,=C'10='                                                    
         MVC   DSPFWD(L'SE@FWD),SE@FWD                                          
         MVC   DSP12,=C'12='                                                    
         MVC   DSPQUILT(L'SE@QUILT),SE@QUILT                                    
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
ADDPLUS  LA    R1,L'DSPWRTX(R1)                                                 
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'+'                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND AN ELEMENT                                          *         
*                                                                     *         
* NTRY: RECORD IN IOAREA1, R1=ELEMENT CODE                            *         
* EXIT: R3=A(ELEMENT), RF=L(ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
FINDEL   L     R3,AIOAREA1         SET R3 TO FIRST ELEMENT                      
         LA    R3,SAFCDATA-SAFCREC(R3)                                          
         XR    RF,RF                                                            
         SPACE 1                                                                
FEL2     CLI   0(R3),0             TEST E-O-R                                   
         BE    FELLOVER                                                         
         SPACE 1                                                                
         IC    RF,1(R3)            RF=LENGTH OF ELEMENT                         
         CLM   R1,1,0(R3)          MATCH ON ELEMENT CODE                        
         BER   RE                                                               
         BXH   R3,RF,FEL2                                                       
         SPACE 1                                                                
FELLOVER SR    RE,RB               POOEY                                        
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
FLDINPLQ EQU   DSPFLD2H-DSPFLDH    LENGTH OF FIELD/INPUT FIELDS                 
FLDINPSQ EQU   DSPBBARH-DSPFLDH    LENGTH OF 1 PAGE OF FIELD/INPUTS             
FLDSDISQ EQU   FLDINPSQ/FLDINPLQ   NUMBER OF FIELD/INPUTS DISPLAYED             
FTABPAGE EQU   FTABL*FLDSDISQ      LENGTH OF 1 PAGE OF FTAB ENTRIES             
         SPACE 1                                                                
HELPWRDL EQU   8                   LENGTH OF WORD FOR 'DISHELP' ROUTINE         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
         EJECT                                                                  
         LTORG                                                                  
SPACES   DC    CL(L'REPP1)' '                                                   
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,50,CT#FCONL,32,C                                              
         SPEC  H2,50,CT#FCONL,32,CU                                             
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,126,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         SPACE 5                                                                
* SEACSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSF0D                                                       
         ORG   DSPPFK1             * PFK LINE LAYOUT *                          
DSPPF    DS    CL2                 'PF'                                         
DSP01    DS    CL3                 '01='                                        
DSPHELP  DS    CL7,CL1             'HELP'                                       
DSP03    DS    CL3                 '03='                                        
DSPWRT   DS    CL9,CL1             'WRITE'                                      
DSP05    DS    CL3                 '05='                                        
DSPREAD  DS    CL9,CL1             'READ'                                       
DSP07    DS    CL3                 '07='                                        
DSPNO    DS    CL9,CL1             'NO'                                         
DSP09    DS    XL3                 '09='                                        
DSPBACK  DS    CL9,CL1             'BACK'                                       
DSP11    DS    XL3                 '11='                                        
DSPNXTLT DS    CL11                'NEXT(LIST)'                                 
         ORG   DSPPFK1+L'DSPPFK1                                                
         ORG   DSPPFK2             * PFK LINE LAYOUT *                          
DSPPF2   DS    CL2                 'PF'                                         
DSP02    DS    CL3                 '02='                                        
DSPHELPX DS    CL7,CL1             'HELP+'                                      
DSP04    DS    CL3                 '04='                                        
DSPWRTX  DS    CL9,CL1             'WRITE+'                                     
DSP06    DS    CL3                 '06='                                        
DSPREADX DS    CL9,CL1             'READ+'                                      
DSP08    DS    CL3                 '08='                                        
DSPNOX   DS    CL9,CL1             'NO+'                                        
DSP10    DS    XL3                 '10='                                        
DSPFWD   DS    CL9,CL1             'FORWARD'                                    
DSP12    DS    XL3                 '12='                                        
DSPQUILT DS    CL11                'QUIT(LIST)'                                 
         ORG   DSPPFK2+L'DSPPFK2                                                
*                                                                               
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSD0D                                                       
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
       ++INCLUDE SEACSB0D                                                       
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
REPPFLDS DS    0CL96               FIELDS                                       
REPPFLD  DS    0CL26                                                            
REPPFWRD DS    CL16                FIELD WORD                                   
         DS    CL1                                                              
REPPFVAL DS    CL1                 VALID/INVALID                                
         ORG   REPPFLDS+L'REPPFLDS                                              
         DS    CL1                                                              
REPPEND  DS    0C                  END OF LINE                                  
         SPACE 3                                                                
***********************************************************************         
* DSECT FOR FIELD TABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
FTABD    DSECT                                                                  
FTCODE   DS    XL1                 FIELD CODE                                   
FTWRD    DS    XL2                 FIELD WORD DICTIONARY REFERENCE#             
FTDSC    DS    XL2                 FIELD TEXT DESCRIPTION#                      
FTABL    EQU   *-FTABD                                                          
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
SAVKEY   DS    XL(L'SAFCKEY)       CURRENT FCONTROL RECORD KEY                  
PRNTKEY  DS    XL(L'SAFCKEY)       CURRENT FCONTROL'S PARENT RECORD KEY         
         SPACE 1                                                                
SAVDATE  DS    CL(L'FVXTRA)        ACTIVITY DATE OF CURRENT RECORD              
SAVSEQNO DS    XL(L'SAACVSEQ)      SEQUENCE NUMBER OF CURRENT RECORD            
         SPACE 1                                                                
SAVAPIND DS    XL1                 SAVED APINDS                                 
SAVAPACT DS    XL1                 SAVED APACTN                                 
         SPACE 1                                                                
FCRINDS  DS    XL1                 * FIELD CONTROL RECORD INDICATORS *          
FCRIAGY  EQU   X'80'               MAIN AGENCY DEFAULT RECORD                   
FCRISIBL EQU   X'40'               RECORD IS SIBLING OF LAST RECORD             
FCRIPAGY EQU   X'20'               RECORD'S PARENT IS AGENCY RECORD             
FCRIACGP EQU   X'10'               RECORD FOR ACCESS GROUP                      
FCRIPRNT EQU   X'08'               RECORD IS A PARENT                           
FCRIREAD EQU   X'01'               READ RECORD                                  
         SPACE 1                                                                
DSPINDS  DS    XL1                 * DISPLAY INDICATORS *                       
DSPIKEEP EQU   X'80'               KEEP CURRENT DISPLAY FOR NEXT INPUT          
DSPIDONE EQU   X'40'               USER HAS FINISHED INPUT                      
DSPIINP  EQU   X'20'               USER HAS INPUT TO SUB-SCREEM                 
DSPIDSP  EQU   X'10'               DISPLAY FOR DISPLAY (NOT ADD/CHANGE)         
DSPINONE EQU   X'08'               NO FIELDS TO LIST IN SUB-SCREEN              
         SPACE 1                                                                
SAVCLRL  EQU   *-SAVOVER           CLEAR TWA UP TO HERE (FOR SETTWA)            
         SPACE 2                                                                
WRITETAB DS    XL32                BIT TABLE OF VALID WRITE FIELDS              
READTAB  DS    XL32                BIT TABLE OF VALID READ FIELDS               
AVAILWRT DS    XL32                BIT TABLE OF AVAILABLE WRITES                
AVAILRD  DS    XL32                BIT TABLE OF AVAILABLE READS                 
         SPACE 1                                                                
SUBSTAB  DS    CL2                 SUB-SCREEN TAB TO NEXT PAGE                  
         SPACE 1                                                                
PAGENO   DS    XL1                 CURRENT DISPLAYED PAGE NUMBER                
NUMPAGES DS    XL1                 NUMBER OF FIELD PAGES                        
         ORG   SAVAREAX                                                         
FTAB     DS    (250)XL(FTABL)      FIELD TABLE                                  
FTABX    DS    0X                                                               
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
GTPARM   DS    6A                                                               
AFTAB    DS    A                   A(FIELD TABLE)                               
WORKKEY  DS    XL(L'SAFCKEY)                                                    
WORKBITT DS    XL32                                                             
         SPACE 1                                                                
WAGY     DS    CL2                 EBCIDIC AGENCY CODE                          
         SPACE 1                                                                
*                                  * DATA FOR SUB-SCREEN MESSAGE *              
SUBMSGNO DS    XL2                 SUB-SCREEN MESSAGE NUMBER                    
SUBMTYP  DS    XL1                 SUB-SCREEN MESSAGE TYPE                      
SUBXTRA  DS    XL50                TEXT TO ADD TO SUB-SCREEN MESSAGE            
SUBXINDS DS    XL1                 EXTRA TEXT INDICATORS                        
SUBXIMID EQU   X'80'                 SUBXTRA IN MIDDLE, NOT AFTER TEXT          
SUBTEXT  DS    XL(L'DSPMSG)        SUB-SCREEN TEXT (IF SET BY USER)             
         SPACE 1                                                                
PRTVAL   DS    CL1                 VALIDITY OF FIELD ON REPORT                  
         SPACE 1                                                                
SELKEY   DS    0XL25               * SELECTS *                                  
SELOVPG  DS    0XL2                                                             
SELOVS   DS    XL1                 SELECT SYSTEM                                
SELPGM   DS    XL1                 SELECT PROGRAM                               
SELUID   DS    XL2                 SELECT USER-ID                               
SELAGN   DS    XL2                 SELECT ACCESS GROUP NUMBER                   
SELVAL   DS    CL1                 VALIDATION FILTER                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SEACS0F   10/27/11'                                      
         END                                                                    
