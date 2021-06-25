*          DATA SET RECNT56    AT LEVEL 056 AS OF 03/05/13                      
*PHASE T80256A,+0                                                               
         TITLE 'REPPAK CONTRACT - TAKEOVER DISPLAY/EDIT T80256'                 
*                                                                               
*********************************************************************           
*                                                                   *           
*        RECNT56 (T80256) --- TAKEOVER DISPLAY/EDIT                 *           
*                                                                   *           
* ---------------------------------------------------------------   *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 30JAN01 HWO IGNORE AGENCY OFFICE WHEN RETRIEVING DARE RECORD      *           
* 15SEP98 SKU ADD RADIO REPS TO DARE/TAKEOVER TABLE                 *           
* 30JUL98 SKU ADD STATION CALL LETTERS IN X'AD' KEY                 *           
* 22JAN97 SKU ORIGINAL DEVELOPMENT                                  *           
* 23OCT97 SKU BUG FIX OF MOVE HISTORY CLOBBERING WORK2              *           
* 18DEC97 SKU CANNOT TAKEOVER KATZ EDI ORDERS                       *           
*                                                                   *           
*                      *** END TOMBSTONE ***                        *           
*********************************************************************           
*                                                                               
T80256   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80256,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VSWITCH,CSWITCH                                                  
         DROP  RE                                                               
*                                                                               
         L     R2,4(R1)                                                         
         CLC   =C'DISP',0(R2)      DISPLAY TAKEOVER INFO?                       
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)      EDIT TAKEOVER INFO?                          
         BE    EDIT                                                             
         DC    H'0'                DIE OTHERWISE                                
         EJECT                                                                  
**********************************************************************          
* DISPLAY TAKEOVER INFO                                                         
**********************************************************************          
DISP     DS    0H                                                               
         LA    R2,TKOKNUMH                                                      
         OI    6(R2),X'40'+X'80'                                                
         OI    4(R2),X'20'         SET PREVALID                                 
*                                                                               
* SHOW EXISTING X'1C' AND X'1D' INFO                                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISPX                                                            
         USING RCONTKEL,R6                                                      
*                                                                               
* GET PREVIOUS CONTRACT NUMBER                                                  
*                                                                               
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONTKCN                                                
         EDIT  (P5,DUB+3),(8,TKOKNUM),ALIGN=LEFT                                
         OI    TKOKNUMH+6,X'80'    XMIT                                         
*                                                                               
* GET REP NAME                                                                  
*                                                                               
         XC    TKOFREP,TKOFREP                                                  
         MVC   TKOFREP(3),TWAFREP                                               
         OC    TKOFREP(3),MYSPACES                                              
         MVI   TKOFREP+4,C':'                                                   
*                                                                               
         LA    R5,REPIDS                                                        
*                                                                               
DISP10   CLC   TKOFREP(3),0(R5)                                                 
         BE    DISP20                                                           
         LA    R5,L'REPIDS(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DISP10                                                           
         MVC   TKOFREP+6(12),=C'NOT DARE REP'                                   
         B     DISP30                                                           
*                                                                               
DISP20   DS    0H                                                               
         MVC   TKOFREP+6(10),3(R5)                                              
*                                                                               
DISP30   DS    0H                                                               
         OI    TKOFREPH+6,X'80'    XMIT                                         
*&&DO                                                                           
*                                                                               
* DATE/TIME STAMP                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,RCONTKDT),(5,TKODATE)                             
*                                                                               
         GOTO1 HEXOUT,DMCB,RCONTKTM,WORK,3,=C'TOG'                              
         MVC   TKOTIME(2),WORK                                                  
         MVI   TKOTIME+2,C':'                                                   
         MVC   TKOTIME+3(2),WORK+2                                              
         MVI   TKOTIME+5,C':'                                                   
         MVC   TKOTIME+6(2),WORK+4                                              
         OI    TKODATEH+6,X'80'    XMIT                                         
         OI    TKOTIMEH+6,X'80'    XMIT                                         
*&&                                                                             
         DROP  R6                                                               
*                                                                               
* DARE ORDER NUMBER                                                             
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISPX                                                            
         USING RCONDREL,R6                                                      
         GOTO1 HEXOUT,DMCB,RCONDRLK,TKODARE,4,=C'TOG'                           
         OI    TKODAREH+6,X'80'    XMIT                                         
*                                                                               
DISPX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* EDIT TAKEOVER INFO                                                            
**********************************************************************          
EDIT     DS    0H                                                               
         LA    R2,TKOKNUMH                                                      
         TM    4(R2),X'20'         DATA CHANGED??                               
         BO    EXXMOD                                                           
***>>>                                                                          
         LA    R3,3                MUST BE NUMERIC                              
         TM    4(R2),X'08'                                                      
         BZ    ERROR                                                            
*                                                                               
         ZAP   DUB,=P'0'                                                        
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    ERROR               LENGTH ERROR                                 
         TM    4(R2),X'08'                                                      
         BZ    ERROR               NON-NUMERIC                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
*                                                                               
         SR    R0,R0                                                            
         CVB   R0,DUB                                                           
*                                                                               
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
*                                                                               
* SET UP FORMER REP'S CONTRACT KEY                                              
*                                                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   CCONNUM,WORK        NUMBER                                       
         MVO   WORK(5),DUB+3(5)                                                 
*                                                                               
         MVC   CONNUM,WORK                                                      
         LA    R3,743                                                           
         XC    KEY,KEY             ORIGINAL ORDER ALREADY IN USE?               
KEYD     USING RCONKEY,KEY                                                      
         MVI   KEYD.RCONTTYP,X'AD'                                              
         MVC   KEYD.RCONTREP,REPALPHA                                           
         MVC   KEYD.RCONTSTA,RCONKSTA                                           
         MVC   KEYD.RCONTOLD,CONNUM                                             
         DROP  KEYD                                                             
*                                                                               
*                                  DON'T CHECK FOR DELETED RECORDS              
***>>>   OI    DMINBTS,X'80'                                                    
         GOTO1 VHIGH                                                            
*                                                                               
* CHECK IF PASSIVE KEY EXISTS AS ACTIVE RECORD                                  
*                                                                               
         CLC   KEY(23),KEYSAVE                                                  
         BE    ERROR               ON FILE/ACTIVE: CAN'T BE REUSED.             
***>>>                                                                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        FIND DARE ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   EDIT10              NO DARE ELEMENT                              
         USING RCONDREL,R6                                                      
         LA    R3,756                                                           
         TM    RCONDRFG,X'04'      KATZ EDI ORDER                               
         BO    ERROR                                                            
         LA    R3,453              CONTRACT ALREADY LINKED                      
         TM    RCONDRFG,X'01'      TAKEOVER DARE ORDER?                         
         BZ    ERROR               NO                                           
         DROP  R6                                                               
*                                                                               
EDIT10   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'        FIND TAKEOVER DARE ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   EDIT20              NO TAKEOVER DARE ELEMENT                     
         USING RCONTKEL,R6                                                      
         MVC   CONNUM,RCONTKCN     YES - SAVE PREVIOUS CONTRACT NUM             
         MVC   REPOWER,RCONTKRP    SAVE PREVIOUS REP CODE                       
         DROP  R6                                                               
*                                                                               
         CLI   5(R2),0             USER REMOVED TAKEOVER LINK                   
         BNE   EDIT20                                                           
*                                                                               
         MVC   KEY+28(4),TWAKADDR     CONTRACT ADDRESS                          
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC   READ CONTRACT FOR UPDATE                  
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'1C',RCONREC)                                    
*                                  DELETE TAKEOVER DARE ELEMENT                 
         GOTO1 VDELELEM,DMCB,(X'1D',RCONREC)                                    
*                                  DELETE DARE AGENCY ORDER ELEMENT             
         GOTO1 VDELELEM,DMCB,(X'2A',RCONREC)                                    
*                                  DELETE MOVE HISTORY ELEMENT                  
*                                     (PROHIBITS 'TAKEOVER' MOVE)               
*                                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
* DELETE ORIGINAL PASSIVE KEYS, IF THEY EXIST                                   
*                                                                               
         XC    KEY,KEY             ORIGINAL PASSIVE FOR DARE TAKEOVER           
KEYD     USING RCONKEY,KEY                                                      
         MVI   KEYD.RCONTTYP,X'AD'                                              
         MVC   KEYD.RCONTREP,REPALPHA                                           
         MVC   KEYD.RCONTSTA,RCONKSTA                                           
         MVC   KEYD.RCONTOLD,CONNUM                                             
         MVC   KEYD.RCONTCON,RCONKCON                                           
         DROP  KEYD                                                             
*                                                                               
         OI    DMINBTS,X'80'                                                    
         GOTO1 VHIGH                                                            
*                                                                               
* CHECK IF PASSIVE KEY EXISTS                                                   
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EDIT15              NOT ON FILE                                  
         MVI   KEY+27,X'80'        ON FILE:  MARK KEY DELETED                   
         GOTO1 VWRITE              REWRITE                                      
*                                                                               
EDIT15   DS    0H                                                               
         XC    KEY,KEY                                                          
WORK2A   USING RCON9EK,KEY         ORIGINAL PASSIVE FOR MOVE HISTORY            
         MVI   WORK2A.RCON9ETP,X'9E'                                            
         MVC   WORK2A.RCON9ERP,REPALPHA                                         
         MVC   WORK2A.RCON9EST,RCONKSTA                                         
         MVC   WORK2A.RCON9ESR,REPOWER                                          
         MVC   WORK2A.RCON9EDT,=X'FFFF'                                         
         MVC   WORK2A.RCON9ESC,CONNUM                                           
         DROP  WORK2A                                                           
*                                                                               
         OI    DMINBTS,X'80'                                                    
         GOTO1 VHIGH                                                            
*                                                                               
* CHECK IF PASSIVE KEY EXISTS                                                   
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP                NOT ON FILE                                  
         MVI   KEY+27,X'80'        ON FILE:  MARK KEY DELETED                   
         GOTO1 VWRITE              REWRITE                                      
         B     DISP                                                             
*                                                                               
EDIT20   DS    0H                                                               
         LA    R3,3                MUST BE NUMERIC                              
         TM    4(R2),X'08'                                                      
         BZ    ERROR                                                            
*                                                                               
         ZAP   DUB,=P'0'                                                        
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    ERROR               LENGTH ERROR                                 
         TM    4(R2),X'08'                                                      
         BZ    ERROR               NON-NUMERIC                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
*                                                                               
         SR    R0,R0                                                            
         CVB   R0,DUB                                                           
*                                                                               
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
*                                                                               
* SET UP FORMER REP'S CONTRACT KEY                                              
*                                                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   CCONNUM,WORK        NUMBER                                       
         MVO   WORK(5),DUB+3(5)                                                 
*                                                                               
* DELETE ORIGINAL PASSIVE KEY IF IT EXISTS                                      
*                                                                               
         CLC   CONNUM,WORK         OLD NUM = NEW NUM?                           
         BE    EDIT25              YES                                          
*                                                                               
         XC    KEY,KEY                                                          
KEYD     USING RCONKEY,KEY                                                      
         MVI   KEYD.RCONTTYP,X'AD'                                              
         MVC   KEYD.RCONTREP,REPALPHA                                           
         MVC   KEYD.RCONTSTA,RCONKSTA                                           
         MVC   KEYD.RCONTOLD,CONNUM                                             
         MVC   KEYD.RCONTCON,RCONKCON                                           
         DROP  KEYD                                                             
*                                                                               
         OI    DMINBTS,X'80'                                                    
         GOTO1 VHIGH                                                            
*                                                                               
* CHECK IF PASSIVE KEY EXISTS                                                   
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EDIT23              NOT ON FILE                                  
         MVI   KEY+27,X'80'        ON FILE:  MARK DELETED                       
         GOTO1 VWRITE              REWRITE                                      
*                                                                               
EDIT23   EQU   *                                                                
         XC    KEY,KEY                                                          
WORK2A   USING RCON9EK,KEY         ORIGINAL PASSIVE FOR MOVE HISTORY            
         MVI   WORK2A.RCON9ETP,X'9E'                                            
         MVC   WORK2A.RCON9ERP,REPALPHA                                         
         MVC   WORK2A.RCON9EST,RCONKSTA                                         
         MVC   WORK2A.RCON9ESR,REPOWER                                          
         MVC   WORK2A.RCON9EDT,=X'FFFF'                                         
         MVC   WORK2A.RCON9ESC,CONNUM                                           
         DROP  WORK2A                                                           
*                                                                               
         OI    DMINBTS,X'80'                                                    
         GOTO1 VHIGH                                                            
*                                                                               
* CHECK IF PASSIVE KEY EXISTS                                                   
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EDIT25              NOT ON FILE                                  
         MVI   KEY+27,X'80'        ON FILE:  MARK KEY DELETED                   
         GOTO1 VWRITE              REWRITE                                      
*                                                                               
*                                                                               
EDIT25   DS    0H                                                               
         MVC   CONNUM,WORK        NUMBER                                        
*                                                                               
         XC    WORK2,WORK2                                                      
WKD      USING RCONTKEL,WORK2                                                   
         MVI   WKD.RCONTKCD,X'1C'                                               
         MVI   WKD.RCONTKLN,RCONTKLQ                                            
*                                                                               
* ADD PASSIVE KEY FOR TAPE UPDATE LATER ON                                      
*                                                                               
         XC    KEY,KEY                                                          
KEYD     USING RCONKEY,KEY                                                      
         MVI   KEYD.RCONTTYP,X'AD'                                              
         MVC   KEYD.RCONTREP,REPALPHA                                           
         MVC   KEYD.RCONTSTA,RCONKSTA                                           
         MVC   KEYD.RCONTOLD,CONNUM                                             
         MVC   KEYD.RCONTCON,RCONKCON                                           
         DROP  KEYD                                                             
*                                                                               
         OI    DMINBTS,X'88'                                                    
         GOTO1 VHIGH                                                            
*                                                                               
* CHECK IF PASSIVE KEY EXISTS                                                   
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    EDIT30                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VADD                                                             
         B     EDIT40                                                           
*                                                                               
EDIT30   DS    0H                                                               
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VWRITE                                                           
*                                                                               
EDIT40   DS    0H                                                               
         MVC   WKD.RCONTKCN,CONNUM                                              
         BAS   RE,GETREPID                                                      
         BNZ   EDIT50                                                           
*                                                                               
         BAS   RE,PROCFREP         SWITCH/PROCESS FORMER REP CONTRACT           
*                                                                               
EDIT50   DS    0H                                                               
         BAS   RE,ADDELM           ADD TAKEOVER ELEMENT                         
*                                                                               
         B     DISP                                                             
**********************************************************************          
* READ REP ID FROM CONTROL FILE                                                 
**********************************************************************          
GETREPID NTR1                                                                   
         LA    R3,573                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(3),TWAFREP                                                
*                                                                               
         CLC   =C'EJO',TWAFREP     SPECIAL FOR TEST REPS                        
         BNE   GETRID03                                                         
         MVC   KEY+15(4),=C'EJOR'                                               
         B     GETRID30                                                         
*                                                                               
GETRID03 DS    0H                  SPECIAL FOR TEST REPS                        
         CLC   =C'ROB',TWAFREP     SPECIAL FOR TEST REPS                        
         BNE   GETRID05                                                         
         MVC   KEY+15(3),=C'ROB'                                                
         B     GETRID30                                                         
*                                                                               
GETRID05 DS    0H                  SPECIAL FOR TEST REPS                        
         CLC   =C'RPD',TWAFREP                                                  
         BNE   GETRID08                                                         
         MVC   KEY+15(7),=C'REPDEMO'                                            
         B     GETRID30                                                         
*                                                                               
GETRID08 DS    0H                                                               
         CLC   =C'SJR',TWAFREP                                                  
         BE    GETRID30                                                         
         CLC   =C'REP',TWAFREP                                                  
         BE    GETRID30                                                         
*                                                                               
* KATZ USES DIFFERENT OFFICE CODES                                              
*                                                                               
         CLC   =C'KAM',KEY+15                                                   
         BE    GETRID10                                                         
         CLC   =C'KCO',KEY+15                                                   
         BE    GETRID10                                                         
         CLC   =C'KNA',KEY+15                                                   
         BNE   GETRID15                                                         
*                                                                               
GETRID10 DS    0H                                                               
         MVC   KEY+18(2),=C'CR'    KATZ USES DIFFERENT OFFICE CODES             
         CLC   =C'CA',RCONKOFF     FOR CR, CL, AND DN                           
         BE    GETRID30                                                         
         MVC   KEY+18(2),=C'CL'                                                 
         CLC   =C'CV',RCONKOFF                                                  
         BE    GETRID30                                                         
         MVC   KEY+18(2),=C'DN'                                                 
         CLC   =C'DV',RCONKOFF                                                  
         BE    GETRID30                                                         
         B     GETRID20                                                         
*                                                                               
* SELTEL USES DIFFERENT OFFICE CODES                                            
*                                                                               
GETRID15 DS    0H                                                               
         CLC   =C'SEL',KEY+15                                                   
         BNE   GETRID20                                                         
         MVC   KEY+18(2),=C'NC'    SELTEL USES DIFFERENT OFFICE CODES           
         CLC   =C'CA',RCONKOFF     FOR NC, CL, AND DN                           
         BE    GETRID30                                                         
         MVC   KEY+18(2),=C'CL'                                                 
         CLC   =C'CV',RCONKOFF                                                  
         BE    GETRID30                                                         
         MVC   KEY+18(2),=C'DN'                                                 
         CLC   =C'DV',RCONKOFF                                                  
         BE    GETRID30                                                         
*                                                                               
GETRID20 DS    0H                                                               
         MVC   KEY+18(2),RCONKOFF                                               
*                                                                               
GETRID30 DS    0H                                                               
         OC    KEY+15(10),MYSPACES SPACE PAD                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                      
*                                                                               
         CLI   8(R1),0                                                          
         BNE   GETRNO                                                           
*                                                                               
         L     R6,AIO2                                                          
         CLC   KEY(25),0(R6)                                                    
         BNE   GETRNO                                                           
*                                                                               
         LA    R6,28(R6)                                                        
GETRID40 CLI   0(R6),X'06'         GET REP CODE                                 
         BE    GETRID50                                                         
         BH    GETRID45                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETRID40                                                         
GETRID45 EQU   *                                                                
         B     GETRNO                                                           
*                                                                               
GETRID50 MVC   WKD.RCONTKRP,2(R6)  SAVE FORMER REP CODE                         
         MVC   REPOWER,2(R6)       SAVE FORMER REP CODE                         
*                                                                               
GETRID53 DS    0H                                                               
         CLI   0(R6),X'21'         GET REP FACPAK SYSTEM NUMBER                 
         BNE   GETRID55                                                         
         BH    GETRNO                                                           
         CLI   2(R6),X'08'         GET REP FACPAK SYSTEM NUMBER                 
         BE    GETRID60                                                         
*                                                                               
GETRID55 DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETRID53                                                         
         B     GETRNO                                                           
*                                                                               
GETRID60 DS    0H                                                               
         MVC   REPSYS#,3(R6)                                                    
*                                                                               
GETRYES  SR    RC,RC                                                            
GETRNO   LTR   RC,RC                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* SWITCH TO SOURCE REP FILE AND READ CONTRACT/DARE RECORDS                      
**********************************************************************          
PROCFREP NTR1                                                                   
         GOTO1 VSWITCH,DMCB,(REPSYS#,X'FFFFFFFF'),0                             
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    PROCF10             YES - NOW READ CONTRACT/DARE RECORDS         
         CLI   4(R1),2             SYSTEM NOT OPENED?                           
         BE    *+6                                                              
         DC    H'0'                OTHERWISE DEATH                              
*                                                                               
         LA    R3,658                                                           
         B     PROCFER                                                          
*                                                                               
PROCF10  DS    0H                                                               
         XC    RCONREC(32),RCONREC                                              
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,WKD.RCONTKRP                                            
         MVC   RCONPCON,CCONNUM    NUMBER                                       
         MVC   KEY,RCONREC                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH                                                            
*                                                                               
* MIGHT NOT BE A DDS REP, EVEN THOUGH THE ID RECORD EXISTS                      
* IF NO CONTRACT FOUND, JUST RECORD NUMBER AND EXIT                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PROCFOK                                                          
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 VGETREC,DMCB,RCONREC                                             
         TM    RCONCNTL,X'80'                                                   
         BO    PROCFOK                                                          
*                                                                               
         LA    R3,659              K NOT A DARE ORDER                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        RECORD NUMBER AND EXIT                       
         BAS   RE,GETEL                                                         
         BNE   PROCFOK                                                          
*                                                                               
         LA    R3,430              MISSING EQUIVALENCY CODE                     
         BAS   RE,GETDARE                                                       
         BNZ   PROCFER                                                          
         L     R6,AIO2                                                          
         USING RDARREC,R6          ID OF SEND AND RETURN TO SENDER INFO         
         MVC   WKD.RCONTKRC,RDARSNDR                                            
         MVC   WKD.RCONTKRT,RDARRTS                                             
         DROP  R6                                                               
*                                                                               
         B     PROCFOK                                                          
*                                                                               
PROCFER  DS    0H                  ERROR, SWITCH BACK TO REP FIRST              
*        GOTO1 VSWITCH,DMCB,(X'08',X'FFFFFFFF'),0                               
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    ERROR               NO? DIE!                                     
         DC    H'0'                                                             
*                                                                               
PROCFOK  DS    0H                  NO ERROR, SWITCH BACK TO REP                 
*        GOTO1 VSWITCH,DMCB,(X'08',X'FFFFFFFF'),0                               
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    EXXMOD              NO? DIE!                                     
         DC    H'0'                                                             
         EJECT                                                                  
**********************************************************************          
* RETRIEVE DARE RECORD AND PUT IN TO AIO2                                       
* IGNORING AGENCY OFFICE                                                        
**********************************************************************          
GETDARE  NTR1                                                                   
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BNE   GETDNO                                                           
         GOTO1 VGETREC,DMCB,IOAREA                                              
         CLC   RAGY2DAR,MYSPACES   ANY CODE ASSIGNED?                           
         BE    GETDNO                                                           
         OC    RAGY2DAR,RAGY2DAR                                                
         BZ    GETDNO                                                           
         MVI   DARETYPE,X'51'      SET RECORD TYPE                              
         LA    R5,KEY              CHECK IF DARE AGENCY ORDER EXISTS            
         USING RDARKEY,R5                                                       
GETD05   EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   RDARKTYP(1),DARETYPE                                             
         MVC   RDARKREP,RCONKREP                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,MYSPACES                                                
         OC    RAGY2DAR,RAGY2DAR     NULL EQUIVALENCY CODE?                     
         BZ    GETD15                                                           
         MVC   RDARKAGY,RAGY2DAR      EQUIVALENCY CODE                          
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R4,RAGY2DAR         CHECK UP TO 4 AGENCY EQUIVALENTS             
         LA    R3,4                                                             
         B     GETD10                                                           
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
GETD08   CLC   RDARKAOF,PRVKEY.RDARKAOF  SAME OFFICE?                           
         DROP  PRVKEY                                                           
         BNE   GETD09                NO -- DON'T INREMENT                       
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
GETD09   XC    RDARKORD(7),RDARKORD  CLEAR FELDS AFTER AGENCY OFFICE            
*                                                                               
GETD10   DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   GETD11                                                           
         XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
         MVC   RDARKORD,RCONDRLK     MOVE IN ORDER # FOR RDHI                   
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   GETD11                                                           
         CLC   RDARKORD,RCONDRLK     SAME ORDER NUMBER?                         
         BNE   GETD08                NO -- SKIP READ                            
         CLI   RDARKRT,X'10'         AGENCY HEADER?                             
         BE    GETD20                YES -- DARE RECORD BUILT...                
         B     GETD15                                                           
         DROP  R6                                                               
*                                                                               
GETD11   CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BL    GETD15              HAVE MULTI-DARE AGENCY ASSIGNMENTS           
         MVC   KEY,KEYSAVE                                                      
*                                                                               
GETD12   LA    R4,5(R4)                                                         
         OC    0(3,R4),0(R4)         NULL EQUIVALENCY CODE?                     
         BZ    GETD15                YES                                        
         CLC   RDARKAGY,0(R4)        SAME EQUIVALENCY CODE?                     
         BNE   *+12                                                             
         BCT   R3,GETD12             CHECK NEXT EQUIVALENCY CODE                
         B     GETD15                                                           
*                                                                               
         MVC   RDARKAGY(5),0(R4)      EQUIVALENCY CODE                          
         XC    RDARKAOF(9),RDARKAOF  CLEAR FIELDS AFTER AGENCY CODE             
         BCT   R3,GETD10                                                        
*                                                                               
GETD15   EQU   *                                                                
         CLI   DARETYPE,X'51'      ONLY PROCESSING 51 RECORDS:                  
*                                     ORDER MUST BE CONFIRMED                   
***>>>   CLI   DARETYPE,X'41'      BOTH DARE TYPES DONE?                        
         BE    GETDNO              YES                                          
         MVI   DARETYPE,X'41'      NO  - DO TYPE 41                             
         B     GETD05                                                           
*                                                                               
GETD20   DS    0H                                                               
         GOTO1 VGETREC,DMCB,AIO2                                                
*                                                                               
GETDYES  SR    RC,RC                                                            
GETDNO   LTR   RC,RC                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* ADD TAKEOVER ELEMENT                                                          
**********************************************************************          
ADDELM   NTR1                                                                   
         MVC   KEY+28(4),TWAKADDR     CONTRACT ADDRESS                          
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC   READ CONTRACT FOR UPDATE                  
*                                                                               
* DATE/TIME STAMP                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,WKD.RCONTKDT)                               
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,WKD.RCONTKTM                                                
*                                                                               
         MVC   DUB(6),WKD.RCONTKRP SAVE REP/CON#                                
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'1C',RCONREC)                                    
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
         XC    TWAELEM,TWAELEM                                                  
WORK2A   USING RCONMMEL,TWAELEM                                                 
         MVI   WORK2A.RCONMMCD,X'2A'                                            
         MVI   WORK2A.RCONMMLN,RCONMMLQ                                         
         MVC   WORK2A.RCONMMOR,DUB                                              
*                                  LOAD ORIGINAL REP                            
         MVC   WORK2A.RCONMMOC,DUB+2                                            
*                                  LOAD ORIGINAL CONTRACT NUMBER                
         MVC   WORK2A.RCONMMDT,=X'FFFF'                                         
*                                  LOAD INDICATOR DATE FOR 'TKO ACTION'         
*                                                                               
         DROP  WORK2A                                                           
         GOTO1 VDELELEM,DMCB,(X'2A',RCONREC)                                    
         GOTO1 VADDELEM,DMCB,RCONREC,TWAELEM                                    
*                                                                               
         OC    WKD.RCONTKRT,WKD.RCONTKRT                                        
         BNZ   ADDELM05            DELETE X'1D' ELEMENT FOR NON-DDS REP         
         GOTO1 VDELELEM,DMCB,(X'1D',RCONREC)                                    
         B     ADDELM20                                                         
*                                                                               
         DROP  WKD                                                              
*                                                                               
ADDELM05 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDELM10                                                         
*                                                                               
         L     R5,AIO2                                                          
         USING RCONDREL,R6                                                      
         USING RDARREC,R5                                                       
         MVC   RCONDRLK,RDARKORD                                                
         B     ADDELM20                                                         
         DROP  R5,R6                                                            
*                                                                               
ADDELM10 DS    0H                                                               
         L     R6,AIO2                                                          
         USING RDARREC,R6                                                       
*                                                                               
         XC    WORK2,WORK2                                                      
WKD      USING RCONDREL,WORK2                                                   
         MVI   WKD.RCONDRCD,X'1D'                                               
         MVI   WKD.RCONDRLN,RCONDL2Q                                            
         MVC   WKD.RCONDRLK,RDARKORD                                            
         MVI   WKD.RCONDRFG,X'81'      FLAG AS TAKEOVER                         
*                                                                               
* SET MANUAL REVISION FOR CONFIRMATED CONTRACTS. THIS IS TO ENSURE              
* THAT THE USERS WILL DO MANUAL REVISION FOR THE FIRST TIME AFTER A             
* TKO. THIS IS DUE TO THE ABSENCE OF BUY LINK INFORMATION AVAILABLE             
* TO DO AUTOMATIC REVISION                                                      
*                                                                               
         CLI   RCONMOD,X'FF'                                                    
         BE    ADDELM15                                                         
         MVI   WKD.RCONDRF2,X'04'  SET MANUAL REVISION                          
*                                                                               
ADDELM15 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
ADDELM20 DS    0H                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
*                                                                               
* ADD PASSIVE KEY FOR TAKEOVER LOCKOUT                                          
*                                                                               
         XC    KEY,KEY                                                          
WORK2A   USING RCON9EK,KEY         ORIGINAL PASSIVE FOR MOVE HISTORY            
         MVI   WORK2A.RCON9ETP,X'9E'                                            
         MVC   WORK2A.RCON9ERP,REPALPHA                                         
         MVC   WORK2A.RCON9EST,RCONKSTA                                         
         MVC   WORK2A.RCON9ESR,REPOWER                                          
         MVC   WORK2A.RCON9EDT,=X'FFFF'                                         
         MVC   WORK2A.RCON9ESC,CONNUM                                           
         DROP  WORK2A                                                           
*                                                                               
         OI    DMINBTS,X'88'                                                    
         GOTO1 VHIGH                                                            
*                                                                               
* CHECK IF PASSIVE KEY EXISTS                                                   
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    ADDELM30                                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VADD                                                             
         B     ADDELM40                                                         
*                                                                               
ADDELM30 DS    0H                                                               
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VWRITE                                                           
*                                                                               
ADDELM40 DS    0H                                                               
ADDELMX  DS    0H                                                               
         B     EXXMOD                                                           
         DROP  WKD,R6                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDARETAB                                                      
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTD7D                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
***** LOCAL STORAGE                                                             
GENOLD   DSECT                                                                  
         ORG   LOCALVAR                                                         
VSWITCH  DS    A                                                                
REPSYS#  DS    X                                                                
CCONNUM  DS    XL4                 CONTRACT NUMBER IN 9'S COMPLEMENT            
CONNUM   DS    XL4                                                              
REPOWER  DS    CL2                 PREVIOUS REP CODE                            
DARETYPE DS    XL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056RECNT56   03/05/13'                                      
         END                                                                    
