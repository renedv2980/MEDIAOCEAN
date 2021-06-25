*          DATA SET SEACS04    AT LEVEL 010 AS OF 01/25/18                      
*PHASE TA0D04A                                                                  
ACS04    TITLE '- SECURITY ACCESS - RECORD RECORDS'                             
ACS04    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS4**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         USING SAPGMD,SSAPGMEL                                                  
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         LA    RE,SAVOVER                                                       
         LA    RE,ATAB-SAVOVER(RE)                                              
         ST    RE,AATAB            SAVE A(ACTION TABLE)                         
*                                                                               
         XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
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
         B     XIT                 15 - APMVALQ                                 
         B     XIT                 16 - APMREPP                                 
         B     SETTWA              17 - APMSETT                                 
         B     XIT                 18 - APMPUTK                                 
         B     XIT                 19 - APMNEWK                                 
         B     XIT                 20 - APMFRP                                  
         B     XIT                 21 - APMDISS2                                
         SPACE 1                                                                
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF RECORD RECORD                            *         
*  - VALIDATE INPUT FIELDS                                            *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   MVI   DSPINDS,0           CLEAR DISPLAY INDIACATORS                    
*                                                                               
         LA    R2,SAVKEY                                                        
         USING SARCREC,R2          R1=A(RECORD KEY)                             
*                                                                               
         CLC   SAVAPACT,APACTN     IF ACTION CHANGED FROM CHANGE                
         BE    VKEY02                RECORD SHOULD BE RE-READ                   
         CLI   SAVAPACT,ACTCHA                                                  
         MVC   SAVAPACT,APACTN                                                  
         BNE   *+8                                                              
         OI    RCDINDS,RCDIREAD                                                 
*                                                                               
VKEY02   TM    DSPSYSH+FHIID,FHIIVA  TEST ALL FIELDS VALIDATED                  
         BZ    VKEY04                                                           
         TM    DSPPGMH+FHIID,FHIIVA                                             
         BZ    VKEY04                                                           
         TM    DSPRECH+FHIID,FHIIVA                                             
         BO    VKEY10                                                           
*                                                                               
VKEY04   XC    DSPPGMD,DSPPGMD     CLEAR & TRANSMIT DESCRIPTIONS                
         XC    DSPFMTD,DSPFMTD                                                  
         OI    DSPPGMDH+FHOID,FHOITR                                            
         OI    DSPFMTDH+FHOID,FHOITR                                            
*                                                                               
         MVC   WORKKEY,SARCKEY     SAVE OLD RECORD KEY                          
         XC    SARCKEY,SARCKEY     BUILD NEW RECORD KEY                         
         MVI   SARCTYP,SARCTYPQ                                                 
         MVI   SARCSUB,SARCSUBQ                                                 
*                                                                               
         GOTO1 AVALOVPG,PARM,DSPSYSH,(1,DSPPGMH),DSPPGMDH                       
         BNE   VALKEYN             VALIDATE SYSTEM/PROGRAM                      
         MVC   SARCOVPG,APHALF                                                  
         MVC   SSAPGMEL,APELEM                                                  
         MVC   DSPRECW,AC@RECTY                                                 
         OI    DSPRECWH+FHOID,FHOITR                                            
         MVCDD DSPFMTD,CT#HEX      DISPLAY FORMAT FOR RECORD CODE               
         CLI   SAPGMRIF,SAPGMIFA                                                
         BNE   *+10                                                             
         MVCDD DSPFMTD,CT#ALPH                                                  
         CLI   SAPGMRIF,SAPGMIFN                                                
         BNE   *+10                                                             
         MVCDD DSPFMTD,CT#NMRCL                                                 
*                                                                               
         CLI   DSPREC,FF           TEST LAST ACTION WAS DISKEY                  
         BNE   VKEY06                                                           
         MVC   SARCRCD,DSPREC+1    DISPLAY RECORD CODE                          
         GOTO1 ADISCODE,PARM,(SAPGMRIF,SARCRCD)                                 
         MVC   DSPREC,APWORK                                                    
         B     VKEY08                                                           
*                                                                               
VKEY06   GOTO1 AVALCODE,PARM,(SAPGMRIF,DSPRECH)                                 
         BNE   VALKEYN             VALIDATE RECORD CODE                         
         MVC   SARCRCD,APBYTE                                                   
*                                                                               
VKEY08   MVC   SAVRCD,SARCRCD                                                   
         CLC   SARCKEY,WORKKEY     TEST CHANGE OF RECORD KEY                    
         BE    *+8                                                              
         OI    RCDINDS,RCDIREAD                                                 
         SPACE 1                                                                
***********************************************************************         
*  - READ/SET-UP RECORD RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
VKEY10   TM    RCDINDS,RCDIREAD    TEST READ RECORD                             
         BZ    VALKEYY                                                          
         MVI   RCDINDS,0                                                        
*                                                                               
         CLC   SARCOVPG,SAVOVPG    TEST CHANGE OF PROGRAM                       
         BE    *+12                                                             
         OI    DSPINDS,DSPIVNOS                                                 
         B     *+8                                                              
         OI    RCDINDS,RCDISPGM                                                 
         MVC   SAVOVPG,SARCOVPG                                                 
*                                                                               
VKEY12   MVC   IOKEY,SARCKEY       READ RECORD INTO IO1                         
         L     R2,AIOAREA1                                                      
         LA    R1,IORDD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BL    VALKEYN                                                          
*                                                                               
         MVI   SAVAPIND,APIOKDIS+APIOKCHA+APIOKDEL                              
         BE    VKEY14                                                           
*                                                                               
         MVI   SAVAPIND,APIOKDIS+APIOKRES                                       
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BO    VKEY14                                                           
*                                                                               
         MVI   SAVAPIND,APIOKADD   RECORD CAN BE ADDED                          
         TM    RCDINDS,RCDISPGM    IF SAME PROGRAM THEN CURRENT                 
         BO    VKEY30                DISPLAY IS OKAY                            
*                                                                               
         XC    WRDNO,WRDNO         CLEAR RECORD INFO                            
         XC    DSCNO,DSCNO                                                      
         XC    ATTRCD,ATTRCD                                                    
         B     VKEY20                                                           
*                                                                               
VKEY14   GOTO1 SAVEACT,SARCREC     SAVE ACTIVITY ELEMENT INFO                   
         MVC   FVXTRA,SAVDATE                                                   
*                                                                               
         GOTO1 FINDEL,SARCDELQ     EXTRACT INFO FROM RECORD ELEMENT             
         USING SARCDD,R3                                                        
         MVC   DSCNO,SARCDDSC                                                   
         MVC   WRDNO,SARCDWRD                                                   
         MVC   ATTRCD,SARCDATT                                                  
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
*  - INITIALIZE DISPLAY                                               *         
***********************************************************************         
         SPACE 1                                                                
VKEY20   BAS   RE,INITATAB         SET UP ACTION TABLE                          
         BNL   *+6                                                              
         DC    H'0'                AN INVALID RECORD IS ATTACHED                
*                                                                               
         BAS   RE,DISACTS          DISPLAY ACTION LIST                          
*                                                                               
         MVCDD SUBXTRA,CT#SAVIA                                                 
         BAS   RE,SUBMSG                                                        
         CLI   APACTN,ACTADD                                                    
         BNE   *+12                                                             
         CLI   ATTRCD,0                                                         
         BE    *+8                                                              
         OI    DSPINDS,DSPIKEEP    IGNORE INPUT FOR 1ST DISPLAY                 
*                                                                               
VKEY30   OI    DSPSYSH+FHIID,FHIIVA  SET KEY INPUTS VALIDATED                   
         OI    DSPPGMH+FHIID,FHIIVA                                             
         OI    DSPRECH+FHIID,FHIIVA                                             
*                                                                               
VALKEYY  MVC   APINDS,SAVAPIND                                                  
         MVC   APRECKEY(L'SAVKEY),SAVKEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO  SET SCREEN MODIFIED                 
         B     XIT                                                              
*                                                                               
VALKEYN  XC    SAVKEY,SAVKEY       CLEAR SAVED KEY VALUES                       
         XC    SAVOVPG,SAVOVPG                                                  
         NI    DSPSYSH+FHIID,FF-FHIIVA  ENSURE A KEY FIELD                      
         B     XIT                         IS UNVALIDATED                       
         SPACE 1                                                                
         DROP R2                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A RECORD RECORD                            *         
*   - VALIDATE INPUT ON SCREEN                                        *         
***********************************************************************         
         SPACE 1                                                                
VALREC   BAS   RE,VALNOS           VALIDATE NUMBERS                             
         BNE   VALRECX                                                          
         BAS   RE,ACTSINP          VALIDATE ACTIONS                             
         BE    VREC02                                                           
*                                                                               
         MVI   FVOMTYP,GTMINF      DISPLAY 'ENTER FIELDS' FOR AN ADD            
         MVC   FVMSGNO,=AL2(GI#ENTFD)                                           
         CLI   APACTN,ACTADD                                                    
         BE    VALRECX                                                          
*                                                                               
         MVC   FVXTRA,SAVDATE      OR 'ENTER CHANGES' FOR A CHANGE              
         MVC   FVMSGNO,=AL2(GI#ENTCH)                                           
         B     VALRECX                                                          
         SPACE 1                                                                
***********************************************************************         
*   - BUILD RECORD                                                    *         
***********************************************************************         
         SPACE 1                                                                
VREC02   L     R2,AIOAREA1                                                      
         USING SARCREC,R2          R2=A(RECORD RECORD)                          
         MVC   SARCKEY,SAVKEY                                                   
         LA    R0,SARCDATA-SARCREC+1  SET UP ELEMENTLESS RECORD                 
         STCM  R0,3,SARCLEN                                                     
         MVI   SARCSTAT,0                                                       
         MVI   SARCDATA,0                                                       
*                                                                               
         LA    R3,APELEM                                                        
         USING SARCDD,R3           ADD RECORD ELEMENT                           
         XC    SARCDD(SARCDLNQ),SARCDD                                          
         MVI   SARCDEL,SARCDELQ                                                 
         MVI   SARCDLN,SARCDLNQ                                                 
         MVC   SARCDDSC,DSCNO                                                   
         MVC   SARCDWRD,WRDNO                                                   
         MVC   SARCDATT,ATTRCD                                                  
         GOTO1 AADDELS,SARCREC                                                  
*                                                                               
         USING SAMIXD,R3                                                        
         XC    SAMIXD(SAMIXLNQ),SAMIXD                                          
         MVI   SAMIXEL,SAMIXELQ                                                 
         MVC   SAMIXRCD,SARCRCD                                                 
         MVC   BITTABLE,VALID                                                   
         MVC   SAMIXATT,ATTRCD                                                  
         CLI   SAMIXATT,0          IF RECORD IS ATTACHED                        
         BE    VREC04                REVERSE BITTABLE                           
         XC    BITTABLE,EFFS                                                    
         NC    BITTABLE,AVAIL                                                   
         BZ    VREC06                                                           
VREC04   GOTO1 ATAGBITT,PARM,SAMIXD,BITTABLE                                    
         GOTO1 AADDELS,SARCREC                                                  
         DROP  R3                                                               
*                                                                               
VREC06   CLI   APACTN,ACTADD       TEST ACTION ADD                              
         BNE   VREC20                                                           
         EJECT                                                                  
***********************************************************************         
*   - ADD RECORD TO FILE                                              *         
***********************************************************************         
         SPACE 1                                                                
         GOTO1 ASETACT,SARCREC     DEFINE ACTIVITY ELEMENT                      
         GOTO1 SAVEACT             SAVE NEW ACTIVITY ELEMENT DETAILS            
*                                                                               
         MVC   IOKEY,SARCKEY       TEST RECORD STILL NOT ON FILE                
         GOTO1 AIO,IORDD+IOCONFIL+IO2                                           
         TM    IOERR,IOERNF                                                     
         BO    VREC12                                                           
         OI    RCDINDS,RCDIREAD                                                 
         MVC   FVMSGNO,=AL2(CE#RDADE)                                           
         B     VALRECX                                                          
*                                                                               
VREC12   GOTO1 AIO,IOADD+IOCONFIL+IO1  ADD RECORD                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SAVAPIND,APIOKDEL+APIOKDIS+APIOKCHA                              
*                                                                               
         BE    VALRECY                                                          
         SPACE 1                                                                
***********************************************************************         
*   - WRITE CHANGED RECORD TO FILE                                    *         
***********************************************************************         
         SPACE 1                                                                
VREC20   GOTO1 UPDTACT,PARM,AIOAREA2,SARCREC                                    
         BNE   VALRECX                                                          
*                                                                               
VREC22   GOTO1 AIO,IOPUT+IOCONFIL+IO1  WRITE RECORD                             
         BE    VALRECY                                                          
         DC    H'0'                                                             
*                                                                               
VALRECY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALRECX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ACTION RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISREC   BAS   RE,DISNOS                                                        
         BAS   RE,ACTSINP          VALIDATE ACTIONS INPUT                       
         BE    DREC2                                                            
         OI    TWALSCTL,TWALSHLD+TWALSRTN  HOLD ON TO CURRENT SCREEN            
         B     DREC4                                                            
*                                                                               
DREC2    TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BO    DISRECX                                                          
*                                                                               
DREC4    MVC   FVXTRA,SAVDATE      DISPLAY ACTIVITY DATE                        
*                                                                               
DISRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE/RESTORE RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DELREC   BAS   RE,DISNOS                                                        
         BAS   RE,ACTSINP          VALIDATE ACTIONS INPUT                       
         BE    DELREC2                                                          
*                                                                               
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI#PETDE)   'PRESS ENTER TO DELETE'                 
         MVC   FVXTRA,SAVDATE                                                   
         B     DELRECX                                                          
*                                                                               
DELREC2  CLI   ATTRCD,0            IF THIS RECORD IS ATTACHED                   
         BNE   DELREC10              NO RECORDS ARE ATTACHED TO IT              
*                                                                               
         LA    R2,IOKEY            READ THROUGH ALL PROGRAM'S                   
         USING SARCREC,R2            RECORD RECORDS                             
         MVC   SARCKEY,SAVKEY                                                   
         MVI   SARCRCD,0                                                        
         L     R2,AIOAREA1                                                      
*                                                                               
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
DELREC4  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   DELREC10                                                         
         CLC   SARCKEY(SARCRCD-SARCKEY),IOKEYSAV                                
         BNE   DELREC10                                                         
*                                                                               
         GOTO1 FINDEL,SARCDELQ     TEST ATTACHED TO CURRENT RECORD              
         USING SARCDD,R3                                                        
         CLC   SARCDATT,SAVRCD                                                  
         DROP  R3                                                               
         BNE   DELREC4             IF SO,                                       
         MVC   FVMSGNO,=AL2(FVFXDEL)  CURRENT RECORD CAN'T BE DELETED           
         B     DELRECX                                                          
*                                                                               
DELREC10 L     R2,AIOAREA1         UPDATE ACTIVITY ELEMENT                      
         USING SARCREC,R2                                                       
         GOTO1 UPDTACT,PARM,SARCREC,SARCREC                                     
         BNE   DELRECX                                                          
*                                                                               
         OI    SARCSTAT,X'80'      TURN ON DELETE FLAG IN RECORD                
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SAVAPIND,APIOKRES+APIOKDIS  RESET APINDS                         
*                                                                               
DELRECX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RESTORE RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
RESREC   BAS   RE,DISNOS                                                        
         BAS   RE,ACTSINP          VALIDATE ACTIONS INPUT                       
         BE    RESREC2                                                          
*                                                                               
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI#PETRE)   'PRESS ENTER TO RESTORE'                
         MVC   FVXTRA,SAVDATE                                                   
         B     RESRECX                                                          
*                                                                               
RESREC2  L     R2,AIOAREA1         UPDATE ACTIVITY ELEMENT                      
         USING SARCREC,R2                                                       
         GOTO1 UPDTACT,PARM,SARCREC,SARCREC                                     
         BNE   RESRECX                                                          
*                                                                               
         NI    SARCSTAT,FF-X'80'    TURN OFF DELETE FLAG IN RECORD              
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SAVAPIND,APIOKDIS+APIOKDEL+APIOKCHA                              
*                                                                               
RESRECX  B     XIT                                                              
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
         LM    R2,R3,0(R1)                                                      
*                                                                               
         USING SARCREC,R2                                                       
         MVC   IOKEY,SAVKEY        RE-READ RECORD                               
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORDD+IOCONFIL+IOLOCK                                        
         BE    UPDTACT2                                                         
         TM    IOERR,IOEDEL                                                     
         BO    *+6                                                              
         DC    H'0'                ERROR OTHER THAN DELETED RECORD              
*                                                                               
UPDTACT2 GOTO1 AGETACT,SARCREC     TEST SEQUENCE NUMBER HAS NOT CHANGED         
         CLC   SAVSEQNO,ACTEL+(SAACVSEQ-SAACVD)                                 
         BE    UPDTACT4                                                         
         DROP  R2                                                               
*                                                                               
         MVC   FVMSGNO,=AL2(CE#RDCHE)                                           
         OI    RCDINDS,RCDIREAD    ENSURE RECORD IS RE-READ                     
         LTR   RB,RB                                                            
         B     XIT                 EXIT WITH CC=NOT EQUAL                       
*                                                                               
         USING SARCREC,R3                                                       
UPDTACT4 GOTO1 ASETACT,SARCREC     SET NEW ACTIVITY ELEMENT                     
         GOTO1 SAVEACT             SAVE NEW ACTIVITY ELEMENT DETAILS            
         DROP  R3                                                               
*                                                                               
         CR    RB,RB               EXIT WITH CC=EQUAL                           
         B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SAVE ACTIVITY ELEMENT DETAILS                            *         
*                                                                     *         
* NTRY: R1=A(RECORD RECORD)                                           *         
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
* VALIDATE ATTACHED/TEXT/DICTIONARY NUMBERS                           *         
*                                                                     *         
* EXIT: CC=EQUAL IF NUMBERS ARE VALID                                 *         
***********************************************************************         
         SPACE 1                                                                
VALNOS   NTR1  ,                                                                
*                                                                               
         TM    DSPINDS,DSPIVNOS    TEST NUMBERS MUST BE VALIDATED               
         BZ    VNOS2                                                            
         NI    DSPATTH+FHIID,FF-FHIIVA                                          
         NI    DSPDSCNH+FHIID,FF-FHIIVA                                         
         NI    DSPWRDNH+FHIID,FF-FHIIVA                                         
*                                                                               
VNOS2    TM    DSPATTH+FHIID,FHIIVA  TEST ATTACHED VALIDATED                    
         BO    VNOS10                                                           
         MVI   ATTRCD,0                                                         
         CLI   DSPATTH+FHILD,0     TEST ATTACHED RECORD ENTERED                 
         BE    VNOS4                                                            
         GOTO1 AVALCODE,PARM,(SAPGMRIF,DSPATTH)                                 
         BNE   VALNOSN                                                          
         MVC   ATTRCD,APBYTE                                                    
VNOS4    BAS   RE,INITATAB         RE-INITIALZE ACTION TABLE                    
         BNE   VALNOSN                                                          
         BAS   RE,DISACTS          DISPLAY NEW VALID ACTIONS                    
VNOS6    OI    DSPATTH+FHIID,FHIIVA                                             
*                                                                               
VNOS10   TM    DSPWRDNH+FHIID,FHIIVA TEST WORD VALIDATED                        
         BO    VNOS20                                                           
         GOTO1 AVALDIC,PARM,(SAVOVS,L'DSPWRDA),(C'U',DSPWRDNH)                  
         BNE   VALNOSN                                                          
         MVC   WRDNO,APHALF                                                     
         MVC   DSPWRDA,APWORK                                                   
         OI    DSPWRDAH+FHOID,FHOITR                                            
         OI    DSPWRDNH+FHIID,FHIIVA                                            
*                                                                               
VNOS20   TM    DSPDSCNH+FHIID,FHIIVA TEST DESCRIPTION VALIDATED                 
         BO    VALNOSY                                                          
         IC    RF,SAVOVS                                                        
         LA    RF,X'80'(RF)                                                     
         GOTO1 AVALTXT,PARM,((RF),DSPDSCNH),WRDNO                               
         BNE   VALNOSN                                                          
         MVC   DSCNO,APHALF                                                     
         MVC   DSPDSCA,APWORK                                                   
         OI    DSPDSCAH+FHOID,FHOITR                                            
         OI    DSPDSCNH+FHIID,FHIIVA                                            
*                                                                               
VALNOSY  CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
VALNOSN  LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ATTACHED/DESCRIPTION/WORD NUMBERS                *         
***********************************************************************         
         SPACE 1                                                                
DISNOS   NTR1  ,                                                                
*                                                                               
         XC    DSPATT,DSPATT       ATTACHED RECORD TYPE                         
         CLI   ATTRCD,0                                                         
         BE    DNOS2                                                            
         GOTO1 ADISCODE,PARM,(SAPGMRIF,ATTRCD)                                  
         MVC   DSPATT,APWORK                                                    
DNOS2    OI    DSPATTH+FHIID,FHIIVA                                             
         OI    DSPATTH+FHOID,FHOITR                                             
*                                                                               
         MVC   APBYTE,SAVOVS       DICTIONARY EQUATE                            
         OI    APBYTE,X'80'                                                     
         GOTO1 ADISDIC,PARM,(APBYTE,L'DSPWRDA),(C'U',WRDNO)                     
         MVC   DSPWRDN,APWORK+64                                                
         OI    DSPWRDNH+FHOID,FHOITR                                            
         OI    DSPWRDNH+FHIID,FHIIVA                                            
         MVC   DSPWRDA,APWORK                                                   
         OI    DSPWRDAH+FHOID,FHOITR                                            
*                                                                               
         XC    DSPDSCN,DSPDSCN     DESCRIPTION NUMBER                           
         XC    FULL,FULL           NUMBER IS POSITIVE                           
         MVC   FULL+2(2),DSCNO                                                  
         EDIT  (B4,FULL),(5,DSPDSCN),ALIGN=LEFT                                 
         OI    DSPDSCNH+FHOID,FHOITR                                            
         OI    DSPDSCNH+FHIID,FHIIVA                                            
         IC    RF,SAVOVS                                                        
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),DSCNO),WRDNO                                  
         MVC   DSPDSCA,APWORK                                                   
         OI    DSPDSCAH+FHOID,FHOITR                                            
*                                                                               
DISNOSX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VERIFY INPUT FOR ACTIONS SUB-SCREEN                      *         
*                                                                     *         
* EXIT: CC=EQUAL IF DISPLAY IS 'FINISHED'                             *         
***********************************************************************         
         SPACE 1                                                                
ACTSINP  NTR1  ,                                                                
*                                                                               
         TM    DSPINDS,DSPIKEEP    TEST KEEP CURRENT DISPALY                    
         BO    AINPX2                                                           
*                                                                               
         CLI   APMODE,APMVALR      TEST VALIDATING RECORD                       
         BE    *+8                                                              
         OI    DSPINDS,DSPIDSP     DISPLAY JUST FOR DISPLAY                     
*                                                                               
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
         B     PFUNDF              PFK11 = UNDEFINED (LIST FUNCTION)            
         B     PFUNDF              PFK12 = UNDEFINED (LIST FUNCTION)            
*                                                                               
ACTSINPX BAS   RE,SUBMSG           DISPLAY SUB-SCREEN MESSAGE                   
*                                                                               
         TM    DSPINDS,DSPIDONE    TEST DISPLAY FINISHED                        
         BZ    AINPX2                                                           
         XC    APCURSOR,APCURSOR   RESER CURSOR POSN. & SET CC=EQUAL            
         B     XIT                                                              
*                                                                               
AINPX2   OC    APCURSOR,APCURSOR   TEST CURSOR SET                              
         BNZ   *+12                                                             
         LA    R0,DSPINPH          SET CURSOR                                   
         ST    R0,APCURSOR           TO FIRST ACTION INPUT FIELD                
*                                                                               
         LTR   RB,RB               SET CC=NOT EQUAL                             
         B     XIT                                                              
         EJECT                                                                  
PFENTER  BAS   RE,VALACTS          * ENTER KEY *                                
         BNE   ACTSINPX                                                         
*                                                                               
         CLI   ATTRCD,0            TEST HAVE RECORD ATTACHED                    
         BNE   PFENTER2                                                         
         OC    VALID,VALID         TEST >0 ACTIONS ARE VALID                    
         BNZ   *+14                                                             
         MVC   SUBMSGNO,=AL2(CE#NOVAC)                                          
         B     ACTSINPX                                                         
*                                                                               
PFENTER2 TM    DSPINDS,DSPIDSP+DSPIINP      TEST ACTION IS 'DISPLAY'            
         BO    *+8                            AND INPUT TO SUB-SCREEN           
         OI    DSPINDS,DSPIDONE    SET DISPLAY 'DONE'                           
         B     ACTSINPX                                                         
         SPACE 3                                                                
PFUNDF   XR    R0,R0               * UNDEFINED PF KEY *                         
         IC    R0,APPFKEY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SUBXTRA(2),DUB                                                   
         OI    SUBXINDS,SUBXIMID                                                
         MVC   SUBMSGNO,=AL2(CE#PFUND)                                          
         B     ACTSINPX                                                         
         SPACE 3                                                                
PFHELP   LA    R1,INPHELP          * HELP *                                     
         B     PFCURS                                                           
*                                                                               
PFHELPX  LA    R1,INPHELPX         * HELP+ *                                    
         B     PFCURS                                                           
*                                                                               
PFYESX   LA    R1,INPYESX          * YES+ *                                     
         B     PFCURS                                                           
*                                                                               
PFNOX    LA    R1,INPNOX           * NO+ *                                      
*                                                                               
PFCURS   BAS   RE,SETCURS                                                       
         BNE   ACTSINPX                                                         
         BAS   RE,VALACTS                                                       
         B     ACTSINPX                                                         
         EJECT                                                                  
PFYES    LA    R1,INPYES           * YES *                                      
         B     *+8                                                              
PFNO     LA    R1,INPNO            * NO *                                       
         BAS   RE,SETCURS                                                       
         BNE   ACTSINPX                                                         
*                                                                               
         L     RF,APCURSOR         TEST CURSOR ON LAST INPUT FIELD              
         LA    R0,DSPINPLH                                                      
         CR    RF,R0                                                            
         BE    PFFRWD              SCROLL FORWARD IF NECCESSERY                 
         BAS   RE,VALACTS                                                       
         BNE   ACTSINPX                                                         
         LA    RF,ACTINPLQ(RF)     TAB CURSOR TO NEXT INPUT FIELD               
         TM    FHATD(RF),FHATPR      (UNLESS LAST ONE)                          
         BO    *+8                                                              
         ST    RF,APCURSOR                                                      
         B     ACTSINPX                                                         
         SPACE 3                                                                
PFBACK   BAS   RE,VALACTS          * SCROLL BACK *                              
         BNE   ACTSINPX                                                         
*                                                                               
         XR    RE,RE                                                            
         IC    RE,PAGENO                                                        
         BCT   RE,*+14             TEST CAN DO                                  
         MVCDD SUBXTRA,CT#CTSBK                                                 
         B     ACTSINPX                                                         
*                                                                               
         STC   RE,PAGENO                                                        
         MVCDD SUBXTRA,CT#SCDBK                                                 
         BAS   RE,DISACTS                                                       
         XC    APCURSOR,APCURSOR                                                
         B     ACTSINPX                                                         
         SPACE 3                                                                
PFFRWD   BAS   RE,VALACTS          * SCROLL FORWARD *                           
         BNE   ACTSINPX                                                         
*                                                                               
         IC    RE,PAGENO                                                        
         CLM   RE,1,NUMPAGES       TEST CAN DO                                  
         BNE   *+14                                                             
         MVCDD SUBXTRA,CT#CTSFD                                                 
         B     ACTSINPX                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         STC   RE,PAGENO                                                        
         MVCDD SUBXTRA,CT#SCDFD                                                 
         BAS   RE,DISACTS                                                       
         XC    APCURSOR,APCURSOR                                                
         B     ACTSINPX                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ACTION INPUT FIELDS                             *         
*                                                                     *         
* EXIT: CC=EQUAL IF ALL FIELDS HAVE BEEN VALIDATED                    *         
***********************************************************************         
         SPACE 1                                                                
VALACTS  NTR1  ,                                                                
         XR    R8,R8                                                            
         IC    R8,PAGENO                                                        
         MH    R8,=Y(ATABPAGE)                                                  
         LA    R8,ATAB-ATABPAGE(R8)                                             
         USING ATABD,R8            R8=A(ACTION TABLE ENTRY)                     
*                                                                               
         LA    R2,ACTINPLQ         R2=L(ACTION/INPUT FIELDS)                    
         LA    R3,DSPINPLH         R3=A(LAST INPUT FIELD)                       
         LA    R9,DSPINPH                                                       
         USING FHD,R9              R9=A(FIRST ACTION INPUT)                     
*                                                                               
VACTS2   TM    FHII,FHIIVA         TEST FIELD PREVIOUSLY VALIDATED              
         BO    VACTS8                                                           
         ST    R9,APCURSOR                                                      
         OI    DSPINDS,DSPIINP                                                  
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
         CLI   ATABD,ATEOTQ        TEST E-O-T                                   
         BE    *+8                                                              
         BXLE  R9,R2,VACTS2                                                     
*                                                                               
         OC    SUBSTAB,SUBSTAB     TEST TAB TO NEXT PAGE                        
         BZ    VALACTSY                                                         
         MVC   WORK(L'SUBSTAB),SUBSTAB   SAVE TABBED INPUT                      
*                                                                               
         IC    RE,PAGENO           SCROLL FORWARD                               
         LA    RE,1(RE)                                                         
         STC   RE,PAGENO                                                        
         BAS   RE,DISACTS                                                       
*                                                                               
         MVC   DSPINP,WORK         SET FIRST INPUT OF ACTION SCREEN             
         NI    DSPINPH+FHIID,FF-FHIIVA                                          
         BAS   RE,VALACTS          VALIDATE NEW SCREEN                          
*                                                                               
VALACTSN LTR   RB,RB               CC=NOT EQUAL                                 
         B     XIT                                                              
VALACTSY CR    RB,RB               CC=EQUAL FOR ALL FIELDS VALIDATED            
         B     XIT                                                              
         EJECT                                                                  
VACTUNDF CLI   FHDA,C' '           * INVALID INPUT *                            
         BH    VAUNDF2             TEST FIELD EMPTY                             
         TM    DSPINDS,DSPIDSP     TEST ACTION IS DISPLAY                       
         BO    VACTS6                                                           
         MVC   SUBMSGNO,=AL2(FVFNONE)                                           
         B     *+10                                                             
VAUNDF2  MVC   SUBMSGNO,=AL2(FVFNOTV)                                           
         MVCDD SUBXTRA,CT#SAVIA                                                 
         B     VALACTSN                                                         
         SPACE 3                                                                
VACTYES  TM    DSPINDS,DSPIDSP     * YES *                                      
         BO    VACTS6                                                           
         CLC   INPYESX,FHDA                                                     
         BE    VACTYESX                                                         
         GOTO1 SETBIT,PARM,(ATSEQ,VALID)                                        
         B     VACTS6                                                           
*                                                                               
VACTYESX GOTO1 SETBIT,PARM,(ATSEQ,VALID)                                        
         LA    R8,ATABL(R8)        * YES + *                                    
         CLI   ATABD,ATEOTQ                                                     
         BNE   VACTYESX                                                         
         BAS   RE,DISACTS                                                       
         B     VALACTSN                                                         
         SPACE 3                                                                
VACTNO   TM    DSPINDS,DSPIDSP     * NO *                                       
         BO    VACTS6                                                           
         CLC   INPNOX,FHDA                                                      
         BE    VACTNOX                                                          
         GOTO1 RESETBIT,PARM,(ATSEQ,VALID)                                      
         B     VACTS6                                                           
*                                                                               
VACTNOX  GOTO1 RESETBIT,PARM,(ATSEQ,VALID)                                      
         LA    R8,ATABL(R8)        * NO + *                                     
         CLI   ATABD,ATEOTQ                                                     
         BNE   VACTNOX                                                          
         BAS   RE,DISACTS                                                       
         B     VALACTSN                                                         
         EJECT                                                                  
VACTHELP GOTO1 DISHELP,ATABD       * HELP *                                     
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
         NI    FHII+ACTINPLQ,FF-FHIIVA               TO NEXT INPUT              
         OI    FHOI+ACTINPLQ,FHOITR                                             
*                                                                               
VACTH4   GOTO1 DISAINP,PARM,FHD,ATABD                                           
         B     VALACTSN                                                         
         SPACE 1                                                                
         DROP  R8,R9                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ACTIONS LIST                                     *         
***********************************************************************         
         SPACE 1                                                                
DISACTS  NTR1  ,                                                                
         XC    SUBSTAB,SUBSTAB     CLEAR TAB TO NEXT SCREEN                     
*                                                                               
         MVC   DSPPAGE(1),PAGENO   DISPLAY PAGE NUMBER INFO.                    
         OI    DSPPAGE,C'0'                                                     
         MVC   DSPPAGE+2(1),NUMPAGES                                            
         OI    DSPPAGE+2,C'0'                                                   
         OI    DSPPAGEH+FHOID,FHOITR                                            
*                                                                               
         XR    R8,R8                                                            
         IC    R8,PAGENO                                                        
         MH    R8,=Y(ATABPAGE)                                                  
         LA    R8,ATAB-ATABPAGE(R8)                                             
         USING ATABD,R8            R8=A(ACTION LIST ENTRY)                      
*                                                                               
         LA    R2,ACTINPLQ         R2=L(ACTION/INPUT FIELDS)                    
         LA    R3,DSPACTLH         R3=A(LAST ACTION ON SCREEN)                  
         LA    R9,DSPACTH                                                       
         USING DSPACTH,R9          R9=A(FIRST ACTION ON DISPLAY)                
*                                                                               
DACTS2   OI    DSPINPH+FHOID,FHOITR  TRANSMIT FIELDS                            
         OI    DSPACTH+FHOID,FHOITR                                             
*                                                                               
         CLI   ATABD,ATEOTQ         TEST ANY MORE ACTIONS ON SCREEN             
         BNE   DACTS4                                                           
         XC    DSPINP,DSPINP       CLEAR                                        
         OI    DSPINPH+FHATD,FHATPR PROTECT                                     
         XC    DSPACT,DSPACT       CLEAR                                        
         B     DACTS10                                                          
*                                                                               
DACTS4   GOTO1 ADISDIC,PARM,(SAVOVS,L'DSPACT),(C'L',ATWRD)                      
         MVC   DSPACT,APWORK       COPY ACTION WORD                             
         GOTO1 DISAINP,PARM,DSPACTH,ATABD                                       
         NI    DSPINPH+FHATD,FF-FHATPR                                          
*                                                                               
         LA    R8,ATABL(R8)        BUMP R8 TO NEXT ACTION                       
*                                                                               
DACTS10  BXLE  R9,R2,DACTS2        BUMP R9 TO NEXT ACTION FIELD                 
*                                                                               
DISACTSX B     XIT                                                              
         DROP  R8,R9                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ACTION INPUT                                     *         
*                                                                     *         
* NTRY: P1=ACTION WORD/INPUT FIELD                                    *         
*       P2=A(ACTION TABLE ENTRY)                                      *         
***********************************************************************         
         SPACE 1                                                                
DISAINP  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
ACT      USING DSPACTH,R2          R2=A(ACTION FIELD)                           
         USING ATABD,R3            R3=A(ACTION TABLE ENTRY)                     
*                                                                               
         TM    ACT.DSPACTH+FHATD,FHATPR                                         
         BO    *+8                 ADJUST R2 IF IS THE INPUT FIELD              
         SH    R2,=Y(DSPINPH-DSPACTH)                                           
*                                                                               
         OI    ACT.DSPACTH+FHOID,FHOITR TRANSMIT                                
         OI    ACT.DSPINPH+FHOID,FHOITR                                         
         OI    ACT.DSPINPH+FHIID,FHIIVA SET INPUT VALIDATED                     
*                                                                               
         GOTO1 TESTBIT,PARM,(ATSEQ,VALID)  TEST ACTION IS VALID                 
         BZ    DAINP2                                                           
         MVC   ACT.DSPINP,INPYES                                                
         OI    ACT.DSPACTH+FHATD,FHATHI                                         
         OI    ACT.DSPINPH+FHATD,FHATHI                                         
         B     XIT                                                              
*                                                                               
DAINP2   MVC   ACT.DSPINP,INPNO    ACTION IS NOT VALID                          
         NI    ACT.DSPACTH+FHATD,FF-FHATHI                                      
         NI    ACT.DSPINPH+FHATD,FF-FHATHI                                      
         B     XIT                                                              
         SPACE 1                                                                
         DROP  ACT,R3                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET CURSOR INPUT FIELD                                   *         
*                                                                     *         
* NTRY: R1=A(OUTPUT FOR FIELD)                                        *         
* EXIT: CC=EQUAL IF CURSOR VALIDLY WITHIN ACTIONS SUB-SCREEN          *         
*       APCURSOR=A(INPUT FIELD)                                       *         
***********************************************************************         
         SPACE 1                                                                
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
SCURS12  LA    R0,DSPINPH          TEST CURSOR WITHIN SUB-SCREEN                
         CR    R2,R0                                                            
         BL    SETCURSN                                                         
*                                                                               
         OI    FHOI,FHOITR         SET INPUT FIELD                              
         NI    FHII,FF-FHIIVA                                                   
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+1)                                                   
         EX    RE,*+4                                                           
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
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY HELP FOR ACTION                                  *         
*                                                                     *         
* NTRY: R1=A(ACTION TABLE ENTRY)                                      *         
***********************************************************************         
         SPACE 1                                                                
DISHELP  NTR1  ,                                                                
         LR    R2,R1                                                            
         USING ATABD,R2            R2=A(ACTION ENTRY)                           
         XC    WORK,WORK                                                        
*                                                                               
         GOTO1 ADISDIC,PARM,(SAVOVS,HELPWRDL),(C'U',ATWRD)                      
         MVC   WORK+1(HELPWRDL),APWORK                                          
*                                                                               
         IC    RF,SAVOVS                                                        
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),ATDSC),ATWRD                                  
*                                                                               
         LA    RF,WORK+HELPWRDL    COPY DESCRIPTION TO LEFT OF WORD             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(L'APWORK,RF),APWORK                                            
*                                                                               
         MVC   SUBTEXT,WORK+1                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO DISPLAY SUB SCREEN MESSAGE                              *         
***********************************************************************         
         SPACE 1                                                                
SUBMSG   NTR1  ,                                                                
*                                                                               
         OC    SUBTEXT,SUBTEXT     TEST TEXT SET                                
         BZ    SMSG2                                                            
         MVC   DSPMSG,SUBTEXT                                                   
         B     SMSG20                                                           
*                                                                               
SMSG2    LA    R1,PARM                                                          
         USING GETTXTD,R1          R1=A(GETTXT PARAMETER LIST)                  
         XC    GTBLOCK,GTBLOCK                                                  
*                                                                               
         OC    SUBMSGNO,SUBMSGNO   TEST MESSAGE NUMBER SET                      
         BNZ   SMSG4                                                            
         MVI   GTMTYP,GTMINF       SET DEFAULT MESSAGE                          
         MVI   GTMSGNO1,CI#ALDIS                                                
         B     SMSG6                                                            
*                                                                               
SMSG4    MVC   GTMTYP,SUBMTYP      SET MESSAGE TYPE AND NUMBER                  
         MVC   GTMSGNO,SUBMSGNO                                                 
         CLI   SUBMSGNO,FF         TEST GENERAL SYSTEM MESSAGE                  
         BNE   *+12                                                             
         MVI   GTMSGNO,0                                                        
         MVI   GTMSYS,FF                                                        
*                                                                               
SMSG6    OC    SUBXTRA,SUBXTRA     TEST ANY TEXT TAGGED ON                      
         BZ    SMSG10                                                           
         TM    SUBXINDS,SUBXIMID   TEST TEXT ADDED IN MIDDLE                    
         BZ    SMSG8                                                            
         MVI   GTLTXT,L'SUBXTRA                                                 
         LA    RE,SUBXTRA                                                       
         STCM  RE,7,GTATXT                                                      
         B     SMSG10                                                           
*                                                                               
SMSG8    MVI   WORK,C'-'                                                        
         MVI   WORK+1,C' '                                                      
         MVC   WORK+2(L'SUBXTRA),SUBXTRA                                        
         MVI   GTLTXT,L'SUBXTRA+2                                               
         LA    RE,WORK                                                          
         STCM  RE,7,GTATXT                                                      
*                                                                               
SMSG10   LA    RE,DSPMSGH                                                       
         STCM  RE,7,GTAOUT                                                      
         GOTO1 VGETTXT                                                          
         DROP  R1                                                               
*                                                                               
SMSG20   OI    DSPMSGH+FHOID,FHOITR  TRANSMIT MESSAGE                           
*                                                                               
SUBMSGX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY         INITIALIZE RECORD KEY                        
         USING SARCREC,R2                                                       
         XC    SARCKEY,SARCKEY                                                  
         MVI   SARCTYP,SARCTYPQ                                                 
         MVI   SARCSUB,SARCSUBQ                                                 
*                                                                               
         CLI   LSTSYSH+FHILD,0     TEST SYSTEM NAME ENTERED                     
         BNE   VSEL2                                                            
         CLI   LSTPGMH+FHILD,0     PROGRAM NAME SHOULD BE EMPTY TOO             
         BE    VSEL4                                                            
         MVC   FVMSGNO,=AL2(CE#PFNVS)                                           
         B     VALSELX                                                          
*                                                                               
VSEL2    GOTO1 AVALOVPG,PARM,LSTSYSH,(0,LSTPGMH),0                              
         BNE   VALSELX                                                          
         MVC   SARCOVPG,APHALF                                                  
*                                                                               
VSEL4    MVC   SELSYS,SARCOVS                                                   
         MVC   SELPGM,SARCPGM                                                   
*                                                                               
         LA    R0,LSTACTH          SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM                                                        
         MVI   APPARM+4,LSTLINEN   SET NO. OF LINES                             
         LA    R0,LSTLINEL         SET LIST LINE LENGTH                         
         STCM  R0,3,APPARM+6                                                    
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
*                                                                               
VALSELX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   L     R2,AIOAREA1                                                      
         USING SARCREC,R2                                                       
         MVC   IOKEY,APRECKEY                                                   
*                                                                               
         TM    APINDS,APILRERD     TEST SEQUENTIAL READ BROKEN                  
         BZ    GSEL2                                                            
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETSELN                                                          
         B     GSEL4                                                            
*                                                                               
GSEL2    TM    APINDS,APILNSEQ     TEST START OF NEW SCREEN                     
         BO    GSEL4                                                            
         GOTO1 AIO,IOCONFIL+IOHI+IO1                                            
         BNE   GETSELN                                                          
         B     GSEL6                                                            
*                                                                               
GSEL4    GOTO1 AIO,IOCONFIL+IOSQ+IO1                                            
         BNE   GETSELN                                                          
*                                                                               
GSEL6    CLI   SARCTYP,SARCTYPQ    TEST RECORD TYPE                             
         BNE   GETSELN                                                          
         CLI   SARCSUB,SARCSUBQ                                                 
         BNE   GETSELN                                                          
*                                                                               
         CLI   SELSYS,0            MATCH ON SYSTEM/PROGRAM                      
         BE    GSEL10                                                           
         CLC   SARCOVS,SELSYS                                                   
         BNE   GETSELN                                                          
         CLI   SELPGM,0                                                         
         BE    GSEL10                                                           
         CLC   SARCPGM,SELPGM                                                   
         BNE   GETSELN                                                          
*                                                                               
GSEL10   MVC   APRECKEY(L'SARCKEY),SARCKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         USING SARCREC,R2                                                       
         GOTO1 FINDEL,SARCDELQ                                                  
         USING SARCDD,R3           R3=A(RECORD ELEMENT)                         
*                                                                               
         L     R4,APPARM                                                        
         USING LSTACTH,R4          R4=A(LIST/SELECT LINE)                       
*                                                                               
         GOTO1 ADISSYS,SARCOVS                                                  
         MVC   LSTLSYS,APWORK      SYSTEM NAME                                  
*                                                                               
         GOTO1 ADISPGM,PARM,(SARCOVS,SARCPGM)                                   
         MVC   LSTLPGM,APWORK      PROGRAM NAME                                 
*                                                                               
         GOTO1 ADISDIC,PARM,(SARCOVS,L'LSTLWRD),(C'U',SARCDWRD)                 
         MVC   LSTLWRD,APWORK      RECORD WORD                                  
*                                                                               
         IC    RF,SARCOVS                                                       
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),SARCDDSC),SARCDWRD                            
         MVC   LSTLDSC,APWORK      RECORD DESCRIPTION                           
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+3(1),SARCRCD                                                
         EDIT  (B4,FULL),(3,LTSLNUM),ALIGN=LEFT                                 
*                                                                               
DISSELX  B     XIT                                                              
         DROP  R2,R3,R4                                                         
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF ACTION RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         USING SARCREC,R2                                                       
*                                                                               
         GOTO1 ADISSYS,SARCOVS                                                  
         MVC   DSPSYS,APWORK       SYSTEM NAME                                  
*                                                                               
         GOTO1 ADISPGM,PARM,(SARCOVS,SARCPGM)                                   
         MVC   DSPPGM,APWORK       PROGRAM NAME                                 
*                                                                               
         MVI   DSPREC,FF           RECORD CODE                                  
         MVC   DSPREC+1(1),SARCRCD                                              
*                                                                               
DISKEYX  B     XIT                                                              
         DROP  R2                                                               
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
SETTWA   XC    SAVOVER(SAVCLRX),SAVOVER                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE ACTION TABLE                                             *         
*                                                                     *         
* NTRY: ATTRCD=ATTACHED RECORD TYPE                                   *         
***********************************************************************         
         SPACE 1                                                                
INITATAB NTR1  ,                                                                
         XC    VALID,VALID         CLEAR VALID BIT TABLE                        
         TM    SAVAPIND,APIOKADD                                                
         BO    IATAB04                                                          
         L     R3,AIOAREA1         SET BIT TABLE FROM SAMIXELD                  
         LA    R3,SARCDATA-SARCREC(R3)                                          
         USING SAMIXD,R3                                                        
         XR    RF,RF                                                            
IATAB02  CLI   SAMIXEL,0                                                        
         BE    IATAB04                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BE    *+12                                                             
         IC    RF,SAMIXLN                                                       
         BXH   R3,RF,IATAB02                                                    
         GOTO1 ABLDBITT,PARM,SAMIXD,VALID                                       
         DROP  R3                                                               
*                                                                               
IATAB04  XC    DSPATTW,DSPATTW     CLEAR ATTACHED DESCRIPTIONS                  
         OI    DSPATTWH+FHOID,FHOITR                                            
         XC    DSPATTD,DSPATTD                                                  
         OI    DSPATTDH+FHOID,FHOITR                                            
*                                                                               
         CLI   ATTRCD,0            TEST RECORD IS ATTACHED                      
         BE    IATAB10                                                          
*                                                                               
         CLC   ATTRCD,SAVRCD       ATTACHED RECORD TYPE                         
         BL    *+14                  MUST BE LESS THAN CURRENT TYPE             
         MVC   FVMSGNO,=AL2(CE#ARTLC)                                           
         B     IATABN                                                           
*                                                                               
         LA    R2,IOKEY            READ ATTACHED RECORD RECORD                  
         USING SARCREC,R2                                                       
         MVC   SARCKEY,SAVKEY                                                   
         MVC   SARCRCD,ATTRCD                                                   
         DROP  R2                                                               
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#ARRNF)                                           
         B     IATABN                                                           
*                                                                               
         GOTO1 FINDEL,SARCDELQ                                                  
         USING SARCDD,R3           DISPLAY ATTACHED DESCRIPTIONS                
         GOTO1 ADISDIC,PARM,(SAVOVS,L'DSPATTW),(C'U',SARCDWRD)                  
         MVC   DSPATTW,APWORK                                                   
         IC    RF,SAVOVS                                                        
         LA    RF,X'80'(RF)                                                     
         GOTO1 ADISTXT,PARM,((RF),SARCDDSC),SARCDWRD                            
         MVC   DSPATTD,APWORK                                                   
*                                                                               
         CLI   SARCDATT,0          ATTACHED RECORD CAN'T BE ATTACHED            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#ARRIA)                                           
         B     IATABN                                                           
         DROP  R3                                                               
*                                                                               
         GOTO1 FINDEL,SAMIXELQ     SET AVAILABLE BIT TABLE                      
         GOTO1 ABLDBITT,PARM,(R3),AVAIL                                         
         XC    VALID,EFFS          ADJUST VALID BIT TABLE                       
         NC    VALID,AVAIL                                                      
*                                                                               
IATAB10  LA    R2,IOKEY                                                         
         USING SAACREC,R2          R2=A(ACTION RECORD KEY)                      
         XC    SAACKEY,SAACKEY                                                  
         MVI   SAACTYP,SAACTYPQ                                                 
         MVI   SAACSUB,SAACSUBQ                                                 
         MVC   SAACOVPG,SAVOVPG                                                 
         L     R2,AIOAREA1                                                      
         L     R8,AATAB                                                         
         USING ATABD,R8            R8=A(ACTION TABLE ENTRY)                     
*                                                                               
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
IATAB12  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   IATAB18             TEST ACTION RECORD FOR PROGRAM               
         CLC   SAACKEY(SAACACT-SAACKEY),IOKEYSAV                                
         BNE   IATAB18                                                          
*                                                                               
         LA    RF,SAPGMACT         SEARCH ACTION CODE LIST FOR CODE             
         LR    R1,RF                                                            
         LA    R0,L'SAPGMACT                                                    
IATAB14  CLC   SAACACT,0(RF)                                                    
         BE    IATAB16                                                          
         LA    RF,1(RF)                                                         
         BCT   R0,IATAB14                                                       
         DC    H'0'                                                             
IATAB16  SR    RF,R1                                                            
         STC   RF,ATSEQ            SAVE SEQUENCE NUMBER                         
         CLI   ATTRCD,0            IF ATTACHED                                  
         BE    IATAB17               CHECK AVAILABILITY                         
         GOTO1 TESTBIT,PARM,(ATSEQ,AVAIL)                                       
         BZ    IATAB12                                                          
*                                                                               
IATAB17  GOTO1 FINDEL,SAACTELQ     SAVE ACTION CODE/DICTIONARY REF#             
         MVC   ATWRD,SAACTWRD-SAACTD(R3)                                        
         MVC   ATDSC,SAACTDSC-SAACTD(R3)                                        
*                                                                               
         LA    R8,ATABL(R8)                                                     
         B     IATAB12                                                          
*                                                                               
IATAB18  MVI   ATABD,ATEOTQ        SET END OF TABLE                             
         DROP  R8                                                               
*                                                                               
         C     R8,AATAB            TEST >0 ENTRIES                              
         BNE   IATAB20                                                          
         MVC   FVMSGNO,=AL2(CE#NADFP)                                           
         B     IATABN                                                           
*                                                                               
IATAB20  MVI   PAGENO,1            SET INITIAL PAGE INFO                        
         LA    R0,ATAB-ATABPAGE+ATABL                                           
         SR    R8,R0                                                            
         SRDL  R8,32                                                            
         LA    R0,ATABPAGE                                                      
         DR    R8,R0                                                            
         STC   R9,NUMPAGES                                                      
*                                                                               
IATABY   CR    RB,RB               ACTION TABLE SET UP                          
         B     XIT                                                              
IATABN   LTR   RB,RB               NO ACTIONS IN TABLE                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND AN ELEMENT                                          *         
*                                                                     *         
* NTRY: RECORD IN IOAREA1, R1=ELEMENT CODE                            *         
* EXIT: R3=A(ELEMENT), RF=L(ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
FINDEL   L     R3,AIOAREA1         SET R3 TO 1ST ELEMENT                        
         LA    R3,SARCDATA-SARCREC(R3)                                          
         XR    RF,RF                                                            
*                                                                               
FEL2     CLI   0(R3),0             TEST E-O-R                                   
         BE    FELLOVER                                                         
*                                                                               
         IC    RF,1(R3)            RF=LENGTH OF ELEMENT                         
         CLM   R1,1,0(R3)          MATCH ON ELEMENT CODE                        
         BER   RE                                                               
         BXH   R3,RF,FEL2          BUMP R3 TO NEXT ELEMENT                      
*                                                                               
FELLOVER SR    RE,RB                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO TEST/SET/RESET A BIT OF THE BIT TABLE                   *         
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
*                                                                               
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
*                                                                               
ACTINPLQ EQU   DSPACT2H-DSPACTH    LENGTH OF ACTION/INPUT FIELDS                
ACTINPSQ EQU   DSPBBARH-DSPACTH    LENGTH OF 1 PAGE OF ACTION/INPUTS            
ACTSDISQ EQU   ACTINPSQ/ACTINPLQ   NUMBER OF ACTION/INPUTS DISPLAYED            
ATABPAGE EQU   ATABL*ACTSDISQ      LENGTH OF 1 PAGE OF ACTLISTS                 
*                                                                               
HELPWRDL EQU   8                   LENGTH OF WORD FOR 'DISHELP' ROUTINE         
*                                                                               
FF       EQU   X'FF'                                                            
EFFS     DC    32X'FF'                                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* SEACSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSFBD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSDBD                                                       
         SPACE 1                                                                
         ORG   LSTLINE             * LIST LINE LAYOUT *                         
LSTLSYS  DS    CL7                 SYSTEM                                       
         DS    CL1                                                              
LSTLPGM  DS    CL7                 PROGRAM                                      
         DS    CL1                                                              
LSTLWRD  DS    CL8                 ACTION WORD                                  
         DS    CL1                                                              
LTSLNUM  DS    CL3                 ACTION NUMBER                                
         DS    CL1                                                              
LSTLDSC  DS    CL45                ACTION DESCRIPTION                           
*                                                                               
LSTLINEL EQU   LSTACT2H-LSTACTH    LIST LINE LENGTH                             
LSTLINES EQU   LSTFOOTH-LSTACTH    LIST LINES LENGTH                            
LSTLINEN EQU   LSTLINES/LSTLINEL   NO. OF LIST LINES                            
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ACTION CODE TABLE                                         *         
***********************************************************************         
         SPACE 1                                                                
ATABD    DSECT                                                                  
ATEOTQ   EQU   FF                  END-OF-TABLE INDICATOR                       
ATSEQ    DS    XL1                 ACTION CODE SEQUENCE NUMBER                  
ATWRD    DS    XL2                 ACTION WORD DICTIONARY REFERENCE#            
ATDSC    DS    XL2                 ACTION TEXT DESCRIPTION#                     
ATABL    EQU   *-ATABD                                                          
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE SAVED IN TWA                                        *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SAVOVER                                                          
         SPACE 1                                                                
SAVKEY   DS    XL(L'SARCKEY)       CURRENT RECORD RECORD KEY                    
*                                                                               
SAVOVPG  DS    0XL2                CURRENT OVERLAY SYSTEM/PROGRAM               
SAVOVS   DS    XL1                 CURRENT OVERLAY SYSTEM                       
SAVPGM   DS    XL1                 CURRENT PROGRAM                              
*                                                                               
SAVRCD   DS    XL1                 CURRENT RECORD TYPE                          
*                                                                               
SAVDATE  DS    CL(L'FVXTRA)        ACTIVITY DATE OF CURRENT RECORD              
SAVSEQNO DS    XL(L'SAACVSEQ)      SEQUENCE NUMBER OF CURRENT RECORD            
*                                                                               
SAVAPIND DS    XL1                 SAVED APINDS                                 
SAVAPACT DS    XL1                 SAVED APACTN                                 
*                                                                               
SUBSTAB  DS    CL2                 SUB-SCREEN TAB TO NEXT PAGE                  
*                                                                               
VALID    DS    XL32                VALID ACTIONS BIT TABLE                      
AVAIL    DS    XL32                AVAILABLE ACTIONS BIT TABLE                  
*                                                                               
DSCNO    DS    XL2                 RECORD TEXT DESCRIPTION #                    
WRDNO    DS    XL2                 RECORD DICTIONARY REFERENCE #                
ATTRCD   DS    XL1                 ATTACHED RECORD TYPE (OR 0)                  
*                                                                               
RCDINDS  DS    XL1                 * RECORD RECORD INDICATORS *                 
RCDISPGM EQU   X'80'               RECORD HAS SAME PROGRAM AS LAST              
RCDIREAD EQU   X'01'               READ RECORD FROM FILE                        
*                                                                               
DSPINDS  DS    XL1                 * DISPLAY INDICATORS *                       
DSPIKEEP EQU   X'80'               KEEP CURRENT DISPLAY FOR NEXT INPUT          
DSPIDONE EQU   X'40'               USER HAS FINISHED INPUT                      
DSPIINP  EQU   X'20'               USER HAS INPUT TO SUB-SCREEN                 
DSPIDSP  EQU   X'10'               DISPLAY FOR DISPLAY(NOT ADD/CHANGE)          
DSPIVNOS EQU   X'08'               VALIDATE NUMBERS ON SCREEN                   
*                                                                               
SAVCLRX  EQU   *-SAVOVER           CLEAR TWA UP TO HERE                         
*                                                                               
PAGENO   DS    XL1                 CURRENT DISPLAYED PAGE NUMBER                
NUMPAGES DS    XL1                 NUMBER OF ACTION PAGES                       
*                                                                               
SSAPGMEL DS    XL(SAPGMLNQ)        SAVED PROGRAM ELEMENT                        
*                                                                               
         ORG   SAVAREAX                                                         
ATAB     DS    (250)XL(ATABL)      ACTION TABLE                                 
ATABX    DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
WORK     DS    XL80                                                             
PARM     DS    6A                                                               
AATAB    DS    A                   A(ACTION TABLE)                              
WORKKEY  DS    XL(L'SARCKEY)                                                    
BITTABLE DS    XL32                                                             
*                                                                               
SELSYS   DS    XL1                 SYSTEM FOR LIST                              
SELPGM   DS    XL1                 PROGRAM FOR LIST                             
*                                                                               
*                                  * DATA FOR SUB-SCREEN MESSAGE *              
SUBMSGNO DS    XL2                 SUB-SCREEN MESSAGE NUMBER                    
SUBMTYP  DS    XL1                 SUB-SCREEN MESSAGE TYPE                      
SUBXTRA  DS    XL40                TEXT TO ADD TO SUB-SCREEN MESSAGE            
SUBXINDS DS    XL1                 EXTRA TEXT INDICATORS                        
SUBXIMID EQU   X'80'                 SUBXTRA IN MIDDLE, NOT AFTER TEXT          
SUBTEXT  DS    XL(L'DSPMSG)        SUB-SCREEN TEXT (IF SET BY USER)             
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SEACS04   01/25/18'                                      
         END                                                                    
