*          DATA SET RELFMQ     AT LEVEL 117 AS OF 05/01/02                      
*PHASE T80400A,*                                                                
*INCLUDE INVDAY                                                                 
*INCLUDE INVLST                                                                 
*INCLUDE DEMTIME                                                                
*INCLUDE PAVSTA                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
         TITLE 'RELFM00 - T80400 - REPPAK FILE MAINT - BASE'                    
*                                                                               
*****************************************************************               
*                                                               *               
*- RELFM00 (PHASE T80400) - REP FILE MAINTENANCE BASE           *               
*                                                               *               
*****************************************************************               
*  UPDATE HISTORY:                                              *               
*                                                               *               
*  ../../..  ???  HISTORY LOST                                  *               
*                                                               *               
*  07/21/89  PJS  ADDED NEW RECORD: 'COMMENT' (OR 'CMT')        *               
*                 FOR NEW X'2E' STANDARD COMMENT RECORD.        *               
*                 CODE ADDED TO RELFM01 OVERLAY                 *               
*                                                               *               
*  08/04/89  PJS  1) LIMIT KEY INPUT TO 14 CHARS ON CMT REC.    *               
*                 2) ADDED MASTER REP SECURITY                  *               
*                                                               *               
*  08/11/89  SNS  CHECK FOR EXISTENCE OF STATION RECORD IF      *               
*                 ADDING AN INVENTORY FOR THAT STATION          *               
*                                                               *               
*  08/14/89  PJS  PREVENT 'CHA' TO NON-EXISTENT RECORDS         *               
*                                                               *               
*  AUG23/89  PJS  CHANGE DATAMGR CALLS TO 'KEYSAVE,KEY'         *               
*                 FORMAT FOR INTEREP                            *               
*                                                               *               
*  11/13/89  PJS  AT FLACT3, IF RECORD CODE MATCHES TABLE       *               
*                 ENTRY BUT ACTION BITS DO NOT MATCH, FLAG      *               
*                 AS ERROR INSTEAD OF LOOKING FOR NEXT TABLE    *               
*                 ENTRY.  PREVENTS 'COM,ADD' DUMP (COMPETITION  *               
*                 VS COMMENT)                                   *               
*                                                               *               
*  11/21/89  PJS  CHANGES FOR MODIFIED BUDGET RECORD            *               
*                                                               *               
*  12/11/89  PJS  CHANGE CONTRACT FIX ACTION TO USE ITS OWN     *               
*                 SCREEN (E1) INSTEAD OF SHARING BUDGET SCREEN  *               
*                                                               *               
*  27DEC89   EFJ  DON'T ALLOW STATION RECORD TO BE ADDED IF     *               
*                 SWITCH RECORD EXISTS                          *               
*                                                               *               
*  01/11/90  PJS  ADDED DUMP RECORD FOR DDS TERMINALS.          *               
*                                                               *               
*  01/15/90  PJS  ADDED 'PRO' RECORD                            *               
*                                                               *               
*  01/16/90  PJS  FINDSTA RTN: PUT GRHIGH BACK TO HIGH.         *               
*                 CHANGE 'RR=RB' TO 'RR=BASERELO'               *               
*                                                               *               
*  01/24/90  PJS  ADDED POINTPERSON RECORD (ID = X'31')         *               
*                                                               *               
*  01/25/90  PJS  ADDED CONTRACT TYPE REC (ID = X'32')          *               
*                                                               *               
*  01/27/90  PJS  SAVE SUBSIDIARY REP LIST (SEE GETREP)         *               
*                                                               *               
*  09MAY90   EFJ  FIX STUPID BUGS FROM 27DEC89 FIX              *               
*                                                               *               
*  AUG02/90  BU   ADD REP PROFILE FOR STATIONS.                 *               
*                                                               *               
*  AUG24/90  BU   ADD 'CONTRACT TYPE' TO KEY SELECTION FOR      *               
*                 BUDGET RECORDS.                               *               
*                                                               *               
*  OCT09/90 (MRR) --- REMOVE REPORT AND ERROR 'RECORDS'         *               
*                                                               *               
*  OCT17/90 (BU ) --- ADD ACTION OF 'ALLOC' FOR BUDGET RECORDS  *               
*                                                               *               
*  FEB26/91 (BU ) --- NO CONTRACT TYPE FOR BUDGET/ALLOCATION    *               
*                     DISPLAY REQUEST                           *               
*                                                               *               
*  06MAR91  (EFJ) --- CHANGE FOR NEW FORMAT OF COMMENT REC      *               
*                                                               *               
*  23APR91  (EFJ) --- CHANGE RECUP TO CALL LOCALUP TO STOP      *               
*                      DUMPING ON REC FULL.                     *               
*                                                               *               
*  MAY23/91 (MRR) --- ADD SPOTPAK REP DATA TO TWA               *               
*                                                               *               
*  JUL03/91 (MRR) --- CHANGE REP AND PROD RECORDS TO OV 8       *               
*                                                               *               
*  AUG26/91 (MRR) --- LOOK AT MULTIPLE COMPETITIVE ELEMENTS     *               
*                      IN THE STATION RECORD FOR DEMO CLEARANCE *               
*                                                               *               
*  DEC02/91 (SKU) --- FIXKEY DEFAULTS TO 8-CHAR INPUT           *               
*                                                               *               
*  OCT22/92 (BU ) --- PROHIBIT ACCESS TO BUDGET RECORDS FOR     *               
*                     COMBO STATIONS AFTER 1992                 *               
*                                                               *               
*  NOV02/92 (BU ) --- NEW OFFICE BUDGET FACILITY                *               
*                                                               *               
*  SEP14/93 (BU ) --- SALESPERSON MERGER SETUP                  *               
*                                                               *               
*  DEC03/93 (BU ) --- REMOVE SALESPERSON MERGER STUFF, ADD      *               
*                     SALESPERSON LEAVE DATE.                   *               
*                                                               *               
*  JAN27/94 (BU ) --- ADD DEVELOPMENTAL S/P + BUS DEV CONTYPE.  *               
*                                                               *               
*  MAR09/94 (BU ) --- ADD DEVSAL/DEVTYP TO MASTER ACCESS LIST   *               
*                                                               *               
*  OCT07/94 (BU ) --- NEW OFFICE=TEAM UPDATE PROCESSING         *               
*                                                               *               
*  FEB09/95 (BU ) --- AUTO-ASSIGN OF AGY/ADV CODES.             *               
*                                                               *               
*  SEP21/95 (BU ) --- KATZ UPGRADES                             *               
*                                                               *               
*  OCT26/95 (SKU) --- ADD GLOBBER CALL FROM CONTRACT FOR KATZ   *               
*                     TO AUTOMATICALLY BRING UP THE AGENCY REC  *               
*                                                               *               
*                     ***  END TOMBSTONE  ***                   *               
*****************************************************************               
         SPACE                                                                  
*                                                                               
*- ERROR MESSAGES                                                               
NOACCMSG EQU   55                  NO ACCESS FOR MASTER RECS                    
DISONLY  EQU   123                 DISPLAY-ONLY ACCESS                          
         SPACE 2                                                                
T80400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1000,T80400,R9,RR=R7,CLEAR=YES                                   
         USING GENOLD,RC                                                        
         USING T804FFD,RA                                                       
         BAS   RE,INITL                                                         
         ST    R7,BASERELO         SAVE RELOCATION FACTOR                       
         SPACE 1                                                                
         ST    RB,BASERB                                                        
         ST    R9,BASER9                                                        
         ST    R1,SYSFAC                                                        
*                                                                               
         MVC   LOCALUP,VRECUP                                                   
         LA    RF,MYRECUP                                                       
         ST    RF,VRECUP                                                        
*                                                                               
*- CHECK FOR DDS TUBE ASKING FOR DUMP.                                          
         LR    RF,RA                                                            
         USING TWAD,RF                                                          
         CLI   TWAOFFC,C'*'        DDS?                                         
         BNE   NODUMP                                                           
         CLC   =C'DUMP',LFMREC                                                  
         BNE   NODUMP                                                           
         DC    H'0'                U ASKED FOR IT, U GOT IT!                    
NODUMP   EQU   *                                                                
         DROP  RF                                                               
*                                                                               
*  IF NOT ALREADY DONE, READ IN REP RECORD FOR PGM PROFILE                      
         CLI   SVPGP#,RREPQLFM                                                  
         BE    MAIN100             IN TWA FROM PRIOR HIT                        
*                                                                               
**       LR    R8,RA                                                            
**       USING TWAD,R8                                                          
**       CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
**       BNE   MAIN010                                                          
**       MVI   SVPGP#,RREPQLFM                                                  
**       MVC   SVPGPBIT,=8X'FF'                                                 
**       B     MAIN100                                                          
**       DROP  R8                                                               
*                                                                               
MAIN010  EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),REPALPHA                                               
         GOTO1 READ                                                             
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                REP RECORD NOT ON FILE?   HOW?               
*                                                                               
         GOTO1 GETREC                                                           
         XC    SVPGMPRF,SVPGMPRF   ASSUME NO ELEMENT                            
         MVI   SVPGP#,RREPQLFM                                                  
*                                                                               
         L     RE,AIOAREA          RECORD IS HERE                               
         SR    RF,RF                                                            
         ICM   RF,3,RREPLEN                                                     
         AR    RF,RE                                                            
         MVI   0(RF),0             FORCE 0 AT END OF RECORD                     
*                                                                               
         LA    RE,34(RE)           A(1ST ELEMENT)                               
MAIN020  EQU   *                                                                
         CLI   0(RE),0             END OF RECORD W/O MATCH?                     
         BE    MAIN100                                                          
*                                                                               
         CLI   0(RE),X'04'         PROGRAM PROFILE ELEMENT?                     
         BE    MAIN040                                                          
*                                                                               
         ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         B     MAIN020                                                          
*                                                                               
*   FIND LFM PROGRAM UNIT WITHIN PROGRAM PROFILE ELEMENT                        
MAIN040  EQU   *                                                                
         USING RREPPGMP,RE                                                      
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
         DROP  RE                                                               
*                                                                               
MAIN050  CLI   0(RE),RREPQLFM      LOOKING FOR LFM                              
         BE    MAIN060                                                          
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,MAIN050                                                       
         B     MAIN100                                                          
*                                                                               
MAIN060  MVC   SVPGMPRF,0(RE)      SAVE UNIT IN TWA                             
*                                                                               
MAIN100  EQU   *                                                                
         SPACE                                                                  
         L     R7,16(R1)           A(COMFACS)                                   
         ST    R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         SPACE 1                                                                
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R1,DMCB             A(GETFACT BLOCK)                             
         MVC   DUB,4(R1)           DATE                                         
         GOTO1 VDATCON,DMCB,(4,DUB),(3,TODAY)                                   
         DROP  R7                                                               
         SPACE 1                                                                
         LA    RE,GETEL            COMMON ROUTINES                              
         ST    RE,VGETEL                                                        
         LA    RE,FOUTBLK                                                       
         ST    RE,VFOUTBLK                                                      
         LA    RE,MOVEREC                                                       
         ST    RE,VMOVEREC                                                      
         LA    RE,ADDELEM                                                       
         ST    RE,VADDELEM                                                      
         LA    RE,DELELEM                                                       
         ST    RE,VDELELEM                                                      
         LA    RE,MRKGET                                                        
         ST    RE,VMRKGET                                                       
         LA    RE,STAGET                                                        
         ST    RE,VSTAGET                                                       
         LA    RE,MKTNME                                                        
         ST    RE,VMKTNME                                                       
         EJECT                                                                  
         LA    R3,CLIST                                                         
         LA    R5,VBOOKVAL         GET ADDRESSES OF COMMON ROUTINES             
         L     RF,VCALLOV                                                       
         MVC   DMCB+4(3),=X'D9000A'                                             
GETC     MVC   DMCB+7(1),0(R3)                                                  
         XC    DMCB(4),DMCB                                                     
         XC    DMCB+8(4),DMCB+8                                                 
         BASR  RE,RF               TO CALLOV                                    
         MVC   0(4,R5),DMCB                                                     
         LA    R5,4(R5)                                                         
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   GETC                                                             
         B     GETC2                                                            
         SPACE 1                                                                
CLIST    DC    AL1(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)           
         DC    X'FF'                                                            
         SPACE 1                                                                
GETC2    DS    0H                  DEMO MODULE ADDRESSES                        
         MVI   DMCB+7,X'E0'        DEMOCON                                      
         GOTO1 (RF),(R1),0                                                      
         MVC   VDEMOCON,DMCB                                                    
         MVI   DMCB+7,X'26'        DEFINE                                       
         GOTO1 (RF),(R1),0                                                      
         MVC   VDEFINE,DMCB                                                     
         SPACE 1                                                                
GETV     DS    0H                                                               
         L     RE,=V(HRTOQH)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VHRTOQH                                                       
         L     RE,=V(INVLST)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VINVLST                                                       
         L     RE,=V(INVDAY)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VINVDAY                                                       
         L     RE,=V(PAVSTA)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VPAVSTA                                                       
         SPACE 1                                                                
         XC    LFMMSG,LFMMSG       CLEAR LAST MESSAGE                           
         FOUT  LFMMSG                                                           
*                                                                               
*- GET REP & MASTER REP INFO & SAVE IN TWA                                      
         GOTO1 =A(GETREP),DMCB,(RC),RR=BASERELO                                 
*                                                                               
* CHECK FOR GLOBBER LOADER VARIABLE FROM CONTRACT                               
* THIS IS INSTALLED FOR KATZ SINCE SOME OF THEIR AGENCY RECORDS DO NOT          
* HAVE ADDRESSES                                                                
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',LFMKEY,8,GLRAGY                               
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   MAIN200                                                          
         GOTO1 (RF),DMCB,=C'DELE',,,GLRAGY                                      
*                                                                               
         XC    LFMREC,LFMREC       MANUALLY STUFF AGENCY REC                    
         MVC   LFMREC(3),=C'AGY'                                                
         MVI   LFMRECH+5,3                                                      
         XC    LFMACT,LFMACT       AND ACTION CHA                               
         MVC   LFMACT(3),=C'CHA'                                                
         MVI   LFMACTH+5,3                                                      
*                                                                               
         LA    RE,8                FIND ACTUAL AGENCY FIELD LENGTH              
         LA    RF,LFMKEY+7                                                      
*                                                                               
MAIN180  DS    0H                                                               
         CLI   0(RF),0             NULL IS THE SENTINEL                         
         BNE   MAIN190                                                          
         BCTR  RF,0                                                             
         BCT   RE,MAIN180                                                       
*                                                                               
MAIN190  DS    0H                                                               
         STC   RE,LFMKEYH+5                                                     
*                                                                               
MAIN200  DS    0H                                                               
         LA    R2,LFMRECH                                                       
         LA    R3,INVERR                                                        
         TM    4(R2),X'20'                                                      
         BO    FEDT0020                                                         
* SET TO EDIT ACTION AND KEY                                                    
         NI    LFMACTH+4,X'DF'                                                  
         NI    LFMKEYH+4,X'DF'                                                  
         XC    SVDATA,SVDATA                                                    
         XC    BAREA,BAREA                                                      
         CLI   5(R2),3                                                          
         BL    ERRORALT                                                         
         CLI   5(R2),8                                                          
         BH    ERRORALT                                                         
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                SET FOR EX                                   
         SPACE 1                                                                
         LA    R5,RECLIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,BASERELO                                                      
         LA    R5,6(R5)                                                         
         SPACE 1                                                                
         EX    RE,FLCLC                                                         
         BE    FIELDEDT                                                         
         BXLE  R5,R6,*-8                                                        
         B     ERRORALT                                                         
FLCLC    CLC   8(0,R2),0(R5)       EXECUTED                                     
         SPACE 1                                                                
FIELDEDT MVC   BREC,8(R5)          SAVE REC CODE                                
         SPACE 1                                                                
         USING TWAD,R4                                                          
         LR    R4,RA                                                            
         CLI   TWAOFFC,C'*'        DDS                                          
         BE    FEDT0010                                                         
         CLI   BREC,1                                                           
         BE    ERRORALT                                                         
         CLI   BREC,X'0E'                                                       
         BE    ERRORALT                                                         
         CLI   BREC,X'70'                                                       
         BE    ERRORALT                                                         
         CLI   BREC,X'12'                                                       
         BNE   FEDT0010                                                         
         TM    TWAAUTH,X'80'                                                    
         B     FEDT0010                                                         
         BNO   ERRORALT                                                         
         SPACE 1                                                                
FEDT0010 OI    4(R2),X'20'         SET VALIDATED BIT                            
         EJECT                                                                  
* VALIDATE ACTION                                                               
         SPACE 1                                                                
FEDT0020 LA    R2,LFMACTH                                                       
         LA    R3,INVERR                                                        
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    FEDT0040                                                         
         SPACE 1                                                                
         CLI   BACT,C'D'           OLD ACTION - DISPLAY                         
         BE    FEDT0030                                                         
         CLI   BACT,C'A'                   OR - ADD                             
         BE    FEDT0030                                                         
         NI    LFMKEYH+4,X'DF'     IF NOT EDIT KEY                              
         BAS   RE,FLACT            EDIT ACTION                                  
         B     FEDT0040                                                         
         SPACE 1                                                                
FEDT0030 BAS   RE,FLACT            EDIT ACTION                                  
         CLI   BACT,C'C'           NEW ACTION - CHANGE                          
         BE    FEDT0040                                                         
         CLI   BACT,C'X'                   OR - DELETE                          
         BE    FEDT0040                                                         
         NI    LFMKEYH+4,X'DF'                                                  
         B     FEDT0040                                                         
         EJECT                                                                  
* SUBROUTINE TO EDIT ACTION                                                     
         SPACE 1                                                                
FLACT    CLI   5(R2),3                                                          
         BL    ERRORALT                                                         
         CLI   5(R2),8                                                          
         BH    ERRORALT                                                         
         SR    R8,R8                                                            
         IC    R8,5(R2)                                                         
         BCTR  R8,0                                                             
         SPACE 1                                                                
         L     R5,=A(ACTLIST)                                                   
         A     R5,BASERELO         ADD RELOCATION FACTOR                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,BASERELO                                                      
         LA    R5,6(R5)                                                         
         SPACE 1                                                                
         EX    R8,FLCLC                                                         
         BE    FLACT2                                                           
         BXLE  R5,R6,*-8                                                        
         B     ERRORALT                                                         
         SPACE 1                                                                
FLACT2   MVC   BACT,8(R5)          ACTION CODE                                  
         CLI   BACT,C'B'           BUDGET RUN?                                  
         BNE   FLACT2A             NO  - SKIP FLAG SETTING                      
         MVC   BUDACT(3),=C'BUD'   SET FLAG FOR BUDGET                          
FLACT2A  EQU   *                                                                
         SR    R8,R8                                                            
         IC    R8,9(R5)            ACTION BIT                                   
         SPACE 1                                                                
*                                                                               
*- VALIDATE RECORD AGAINST RECORD TABLE.                                        
*  CHANGED TO LOOK AT 3-8 BYTES USER INPUT, NOT JUST 1ST 3.                     
*                                                                               
         L     R5,=A(RECLIST)                                                   
         A     R5,BASERELO         ADD RELOCATION FACTOR                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,BASERELO                                                      
         LA    R5,6(R5)                                                         
*                                                                               
         ZIC   RF,LFMRECH+5        SUBTRACT 1 FROM LENGTH                       
         BCTR  RF,0                FOR THE 'EX'                                 
         SPACE 1                                                                
FLACT3   EQU   *                                                                
         EX    RF,FLACTCLC         MATCH ON RECORD TYPE?                        
         BNE   FLACT4                                                           
         EX    R8,TSTBYT           TEST ACTION BIT IN RECLIST                   
         BO    FLACTX              VALID ACTION                                 
*                                                                               
         CLC   =C'COM',0(R5)       IF COMPETITION OR COMMENT, OUT.              
         BE    ERRORALT                                                         
FLACT4   BXLE  R5,R6,FLACT3                                                     
         B     ERRORALT                                                         
         SPACE 1                                                                
TSTBYT   TM    9(R5),0                                                          
         SPACE                                                                  
FLACTCLC CLC   LFMREC(0),0(R5)     SCREEN -VS- REC TBL COMPARE                  
         SPACE 1                                                                
FLACTX   EQU   *                                                                
         CLI   BACT,C'B'           BUDGET RUN?                                  
         BE    FLACTX1             YES - CONTINUE TO TEST ACTION                
         OI    4(R2),X'20'         SET VALIDATED BIT                            
FLACTX1  EQU   *                                                                
         MVC   BOVLY,10(R5)        OVERLAY NUM                                  
         MVC   BSCRN,11(R5)        SCREEN                                       
         MVC   BSVKEY,12(R5)       SAVE KEY EDIT INDEX                          
         BR    RE                                                               
         EJECT                                                                  
* KEY VALIDATION                                                                
         SPACE 1                                                                
FEDT0040 EQU   *                                                                
         GOTO1 =A(MASTACC),DMCB,(RC),RR=BASERELO MASTER REC ACCESS?             
         BZ    FEDT0050            ACCESS GRANTED                               
*                                                                               
         L     R3,DMCB             MESSAGE CODE                                 
         B     ERRORALT            & EXIT                                       
*                                                                               
FEDT0050 EQU   *                                                                
         CLI   BACT,C'A'           'ADD' ACTION?                                
         BNE   FEDT0055            NO                                           
         TM    SVPGPBIT,X'01'      YES - AUTO-ASSIGN AGY/ADV CODES?             
         BNO   FEDT0055            NO                                           
         CLI   BREC,X'08'          YES - ADVERTISER RECORD?                     
         BE    FEDT0180            YES - SKIP KEY VALIDATION                    
         CLI   BREC,X'0A'          NO  - AGENCY RECORD?                         
         BE    FEDT0180            YES - SKIP KEY VALIDATION                    
FEDT0055 EQU   *                                                                
         LA    R2,LFMKEYH                                                       
         TM    4(R2),X'20'         TEST VALIDATED                               
         BZ    FEDT0070                                                         
         SPACE 1                                                                
         CLI   BREC,X'12'          FOR INV DON'T                                
         BNE   FEDT0060            TURN ON KEY VALID                            
         B     FEDT0180                                                         
         SPACE 1                                                                
* KEY VALID BUT CHECK DUP KEY IF ADD                                            
FEDT0060 CLI   BACT,C'A'                                                        
         BE    FEDT0090                                                         
*                                                                               
*- IF ACTION IS CHANGE, MAKE SURE WE REALLY HAVE A RECORD                       
         CLI   BACT,C'C'                                                        
         BNE   FEDT0170                                                         
         CLC   BSVDA,=XL4'00'      DO WE HAVE A RECORD (DISK ADDRESS)           
         BNE   FEDT0170                                                         
         SPACE 1                                                                
* KEY NOT VALIDATED                                                             
         SPACE 1                                                                
FEDT0070 LA    R3,MSSNGERR                                                      
         XC    BKEY,BKEY                                                        
         CLI   5(R2),0                                                          
         BE    ERRORALT                                                         
         MVC   BKEY(1),BREC        SET REC CODE IN KEY                          
         MVI   BFMTSW,0            RESET FORMAT SWITCH                          
*                                                                               
         LA    R0,INDICES                                                       
         L     RE,=A(INDLIST)                                                   
         A     RE,BASERELO         ADD RELOCATION FACTOR                        
         CLC   BSVKEY,0(RE)        TEST MATCH ON INDEX NUMBER                   
         BE    FEDT0080                                                         
         LA    RE,L'INDLIST(RE)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
FEDT0080 SR    RF,RF                                                            
         ICM   RF,7,1(RE)          GET DISP TO KEY ROUTINE                      
         A     RF,BASERB                                                        
         BR    RF                                                               
         EJECT                                                                  
* KEY VALIDATION ROUTINES RETURN HERE                                           
         SPACE 1                                                                
FEDT0090 MVC   KEY,BKEY                                                         
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,HIGH                                                          
         NI    DMINBTS,X'F7'                                                    
*                                                                               
*            BUDGETS ONLY!!!!!!!                                                
*                                                                               
*  FOR BUDGETS, AN ACTION OF 'BUDGET' WILL PERMIT THE USER TO PUMP              
*  IN SEVERAL SCREENS RUNNING, WITHOUT HAVING TO DISPLAY/CHANGE EACH            
*  TIME.  IF BACT = B, BACT IS FORCED TO A IF KEY IS NOT FOUND, OR              
*  C IF KEY IS FOUND.  AT WRAPUP, BACT IS RESET TO B.                           
*  ADDITIONALLY, THE FORMAT SWITCH IS OVERRIDDEN AFTER THE SCREEN IS            
*  PUT UP.  ONCE THE SCREEN IS PUT UP, THE USER CAN ENTER SCREENS               
*  CONSECUTIVELY.  EACH WILL BE CONSIDERED AN 'ADD'.                            
         CLI   BACT,C'B'           BUDGET RUN?                                  
         BNE   FEDT0120            NO                                           
         MVI   BACT,C'A'           'ADD' IF KEY NOT FOUND                       
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   FEDT0100            NO  - DO 'ADD'                               
         MVI   BACT,C'C'           'CHANGE ACTION                               
FEDT0100 EQU   *                                                                
         CLI   BUDSCRN,C'Y'        BUDGET SCREEN UP?                            
         BNE   FEDT0110            NO  -                                        
         MVI   BFMTSW,1            YES - TURN OFF 'FORMAT' PASS                 
         B     FEDT0120                                                         
FEDT0110 EQU   *                                                                
         MVI   BUDSCRN,C'Y'        TURN ON FLAG                                 
FEDT0120 EQU   *                                                                
         CLI   BACT,C'A'           TEST ADD                                     
         BE    FEDT0150                                                         
* ACTION IS DISPLAY OR CHANGE                                                   
         LA    R3,ERRNF                                                         
         CLI   BACT,C'L'           'ALLOC' REQUEST?                             
         BNE   FEDT0130            NO                                           
         CLI   OFFALLOC,C'Y'       YES - OFFICE ENTERED?                        
         BE    FEDT0130            YES - CHECK ENTIRE KEY                       
         CLC   KEYSAVE(25),KEY     NO  - DON'T COMPARE OFFICE                   
         BNE   ERRORALT            NOT FOUND                                    
         B     FEDT0140            FOUND                                        
FEDT0130 EQU   *                                                                
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ERRORALT                                                         
FEDT0140 EQU   *                                                                
         LA    R3,ERRDEL                                                        
         TM    KEY+27,X'80'                                                     
         BO    ERRORALT                                                         
         MVC   BSVDA,KEY+28        SAVE DISK ADDRESS                            
         B     FEDT0170                                                         
* ACTION IS ADD                                                                 
FEDT0150 CLC   KEYSAVE(27),KEY     TEST FOR DUP                                 
         BNE   FEDT0160                                                         
         LA    R3,ERRDUP                                                        
         TM    KEY+27,X'80'        TEST REC DELETED                             
         BZ    ERRORALT            NO - SEND DUP REC MSG                        
         LA    R3,ERRDEL           ELSE RE-ADDING DELETE                        
         B     ERRORALT                                                         
         EJECT                                                                  
FEDT0160 DS    0H                                                               
         CLI   KEY,X'02'                                                        
         BNE   FEDT0170            ONLY CHECK FOR STA REC ADDS                  
         GOTO1 =A(CKSWI),DMCB,(RC),RR=BASERELO                                  
         BZ    FEDT0170            NO ERROR                                     
         MVC   LFMMSG(L'SWIEXIST),SWIEXIST                                      
         FOUT  LFMMSG                                                           
         MVI   ERRAREA,X'FF'                                                    
         B     EXITALT                                                          
FEDT0170 OI    LFMKEYH+4,X'20'     SET KEY VALID                                
*                                                                               
FEDT0180 EQU   *                                                                
         CLI   BACT,C'D'           DISPLAY ACTION?                              
         BE    FEDT0185                                                         
         TM    SVPGPBIT+1,X'40'    AGENCY PROFILE BIT                           
         BNO   FEDT0185            NOT ON                                       
         CLI   BSCRN,X'F5'         AGENCY SCREEN?                               
         BNE   FEDT0184            NO  -  CHECK ADV.                            
         CLI   BACT,C'C'           YES - CHANGE ACTION?                         
         BE    FEDT0185            YES - DON'T PROHIBIT FOR ALL-NUMS            
         LA    R2,LFMKEYH          KEY FIELD ON SCREEN FOR 4 DIG NUM            
         TM    5(R2),X'02'         LENGTH MUST BE 2                             
         BL    FEDT0182            ERR   AT LEAST 2 CHARACTERS                  
         TM    4(R2),X'08'         NUMERIC ONLY                                 
         BNO   FEDT0185            CONTINUE                                     
                                                                                
FEDT0182 DS    0H                                                               
         MVC   LFMMSG(L'AGEQER),AGEQER                                          
         B     MYERR                                                            
FEDT018V DS    0H                  ADV ERR                                      
         MVC   LFMMSG(L'AGEQERV),AGEQERV                                        
         B     MYERR                                                            
***********************************************************                     
*                                  ERR   S/B ALPHA OR A/N                       
AGEQER   DC    CL35' KEY MUST CONTAIN AT LEAST 1 ALPHA '                        
                                                                                
AGEQERR  DC    CL25' S/B  BE 4 DIGIT NUMERIC '                                  
AGEQERV  DC    CL25' S/B  BE 2 ALPHA CHARACTERS'                                
                                                                                
                                                                                
MYERR    MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
                                                                                
FEDT0183 DS    0H                                                               
         MVC   LFMMSG(L'AGEQERR),AGEQER                                         
         B     MYERR                                                            
                                                                                
FEDT0184 EQU   *                   ADVERTISER                                   
         CLI   BSCRN,X'F7'                                                      
         BNO   FEDT0185                                                         
         LA    R2,LFMKEYH          KEY FIELD ON SCREEN FOR 2 ALPHA              
         TM    5(R2),X'02'         LENGTH MUST BE 2                             
         BL    FEDT0182            ERR   AT LEAST 2 CHARACTERS                  
         TM    4(R2),X'08'         NUMERIC ONLY                                 
         BO    FEDT018V            ERR  S/B ALPHA                               
*                                  FALL THROUGH                                 
*                                  ORIGINAL FEDT0180 IS BELOW                   
FEDT0185 CLC   BSVSCRN,BSCRN       TEST HAVE RIGHT SCREEN                       
         BE    FEDT0260            YES                                          
         MVC   BSVSCRN,BSCRN       SAVE NEW SCREEN                              
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D90804'                                             
         MVC   DMCB+7(1),BSCRN                                                  
         GOTO1 VCALLOV,DMCB,LFMLAST                                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         CLI   BACT,C'T'                                                        
         BE    FEDT0250                                                         
         CLI   BACT,C'M'                                                        
         BE    FEDT0250                                                         
         CLI   BACT,C'L'           'ALLOCATE' ACTION FOR BUDGETS?               
         BE    FEDT0280            YES - TREAT AS DISPLAY!                      
         CLI   BSCRN,X'E8'         TEST FOR ESTIMATE SCREEN                     
         BE    FEDT0250                                                         
         CLI   BSCRN,X'E7'         TEST FOR BUDGET SCREEN                       
         BE    FEDT0190            YES                                          
         CLI   BSCRN,X'F4'         TEST FOR OFFICE BUDGET SCREEN                
         BNE   FEDT0240                                                         
FEDT0190 EQU   *                                                                
         CLI   BREC,X'8C'          IS IT CONTRACT FIX                           
         BE    FEDT0240            IF YES,SKIP MONTH HEADLINES                  
         SPACE 1                                                                
*                                                                               
*- SEED MONTH LITERALS IN (OFFICE) BUDGET SCREEN, STARTING W/REP                
*  FISCAL YEAR START MONTH.                                                     
*                                                                               
         LA    R2,BUDIHEDH         1ST MONTH FIELD: BUDGET SCREEN               
         CLI   BSCRN,X'F4'         TEST FOR OFFICE BUDGET SCREEN                
         BNE   FEDT0200                                                         
         LA    R2,OBDIHEDH         1ST MONTH FLD: OFFICE BUDGET SCRN            
FEDT0200 EQU   *                                                                
         SPACE 1                                                                
         L     R6,=A(MONTHS)                                                    
         A     R6,BASERELO         ADD RELOCATION FACTOR                        
         LA    R4,12               SET UP BCT                                   
FEDT0210 CLC   0(1,R6),STARTMO                                                  
         BE    FEDT0220                                                         
         CLI   0(R6),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                PROBLEM WITH START MONTH                     
         LA    R6,4(R6)                                                         
         B     FEDT0210                                                         
         SPACE 1                                                                
FEDT0220 MVC   8(3,R2),1(R6)       FILL IN MONTH                                
         FOUT  (R2)                                                             
         ZIC   R5,0(R2)                                                         
         AR    R2,R5               POINT TO STATION BUDGET                      
         CLI   BSCRN,X'F4'         OFFICE BUDGET SCREEN?                        
         BNE   FEDT0230            NO                                           
         ZIC   R5,0(R2)                                                         
         AR    R2,R5               POINT TO FORECAST BUDGET                     
FEDT0230 EQU   *                                                                
         ZIC   R5,0(R2)                                                         
         AR    R2,R5               POINT TO NEXT MONTH                          
         LA    R6,4(R6)                                                         
         BCT   R4,FEDT0220                                                      
         SPACE 1                                                                
FEDT0240 CLI   BACT,C'A'           TEST ADD                                     
         BNE   FEDT0280                                                         
         TM    SVPGPBIT+1,X'40'                                                 
         BO    FEDT0249           PROFILE BIT IS  "ON"                          
         CLI   BSCRN,X'F5'       IS THIS THE AGENCY SCREEN ?                    
         BNE   FEDT0241                                                         
*                                 THE PROFILE BIT IS NOT ON                     
         LA    R2,LAGHAGQH                                                      
         OI    1(R2),X'0C'        LOW INTENSITY                                 
         LA    R2,LAGAGEQH                                                      
         OI    1(R2),X'20'        DATA FIELD PROTECTED                          
         B     FEDT0249                                                         
*                                                                               
FEDT0241 DS   0H                                                                
         CLI   BSCRN,X'F7'         ADVERTISER SCREEN?                           
         BNE   FEDT0242            NO                                           
         TM    SVPGPBIT+1,X'40'                                                 
         BO    FEDT0249            PROFILE BIT IS  "ON"                         
*                                  THE PROFILE BIT IS NOT ON                    
         LA    R2,LADHAVQH                                                      
         OI    1(R2),X'0C'         LOW INTENSITY                                
         B     FEDT0249                                                         
FEDT0242 EQU   *                                                                
         CLI   BSCRN,X'F9'       IS THIS THE S/P SCREEN ?                       
         BNE   FEDT0249                                                         
         TM    SVPGPBIT+1,X'40'                                                 
         BO    FEDT0249           PROFILE BIT IS  "ON"                          
*                                 THE PROFILE BIT IS NOT ON                     
         LA    R2,LF9HSPQH                                                      
         OI    1(R2),X'0C'        LOW INTENSITY                                 
         LA    R2,LF9SPEQH                                                      
         OI    1(R2),X'20'        DATA FIELD PROTECTED                          
         B     FEDT0249                                                         
                                                                                
*                                                                               
FEDT0249 DS   0H                                                                
         SPACE 1                                                                
FEDT0250 MVC   LFMMSG(23),=C'** ENTER RECORD DATA **'                           
         LA    R2,LFMLAST                                                       
         SPACE 1                                                                
         SR    R0,R0               POINT TO FIRST DATA FIELD                    
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         PROTECTED                                    
         BO    *-10                                                             
         B     EXITALT                                                          
         SPACE 1                                                                
FEDT0260 CLI   BACT,C'A'           TEST ADD                                     
         BNE   FEDT0270                                                         
         MVI   BFMTSW,1            SET FOR EDIT                                 
         B     FEDT0280                                                         
         SPACE 1                                                                
FEDT0270 CLI   BACT,C'D'           TEST DISPLAY                                 
         BNE   FEDT0280                                                         
         CLI   BSCRN,X'EE'         CHANGE SCREEN                                
         BNE   *+8                                                              
         MVI   BFMTSW,0                                                         
         CLI   BSCRN,X'EB'         RATIONAL                                     
         BNE   *+8                                                              
         MVI   BFMTSW,0                                                         
         CLI   BSCRN,X'E8'         ESTIMATE                                     
         BNE   *+8                                                              
         MVI   BFMTSW,0            ALWAYS FORMAT FOR DISPLAY                    
         CLI   BFMTSW,0            FOR DISPLAY SWITCH MUST BE FMT               
         BE    FEDT0280                                                         
         LA    R3,DSPLERR          CHANGED DATA ON DISPLAY SCREEN               
         LA    R2,LFMACTH                                                       
         B     ERRORALT                                                         
         EJECT                                                                  
*                                                                               
FEDT0280 EQU   *                                                                
         TM    SVPGPBIT+1,X'40'                                                 
         BO    FEDT0288           PROFILE BIT IS  "ON"                          
         CLI   BSCRN,X'F5'       IS THIS THE AGENCY SCREEN ?                    
         BNE   FEDT0285                                                         
*                                 THE PROFILE BIT IS NOT ON                     
         LA    R2,LAGHAGQH                                                      
         OI    1(R2),X'0C'        LOW INTENSITY                                 
         LA    R2,LAGAGEQH                                                      
         OI    1(R2),X'20'        DATA FIELD PROTECTED                          
         B     FEDT0288                                                         
*                                                                               
FEDT0285 DS   0H                                                                
         CLI   BSCRN,X'F7'         ADVERTISER SCREEN?                           
         BNE   FEDT0287            NO                                           
         TM    SVPGPBIT+1,X'40'                                                 
         BO    FEDT0288            PROFILE BIT IS  "ON"                         
*                                  THE PROFILE BIT IS NOT ON                    
         LA    R2,LADHAVQH                                                      
         OI    1(R2),X'0C'         LOW INTENSITY                                
         B     FEDT0288                                                         
FEDT0287 EQU   *                                                                
         CLI   BSCRN,X'F9'       IS THIS THE S/P SCREEN ?                       
         BNE   FEDT0288                                                         
         TM    SVPGPBIT+1,X'40'                                                 
         BO    FEDT0288           PROFILE BIT IS  "ON"                          
*                                 THE PROFILE BIT IS NOT ON                     
         LA    R2,LF9HSPQH                                                      
         OI    1(R2),X'0C'        LOW INTENSITY                                 
         LA    R2,LF9SPEQH                                                      
         OI    1(R2),X'20'        DATA FIELD PROTECTED                          
         B     FEDT0288                                                         
                                                                                
*                                                                               
FEDT0288 DS   0H                                                                
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(1),BOVLY                                                    
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    REC(256),REC                                                     
         MVC   REC(27),BKEY                                                     
         XC    REC(256),REC                                                     
         MVC   REC(27),BKEY                                                     
         SPACE                                                                  
         L     RF,DMCB             GET OVERLAY ADDRESS                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
         SPACE 1                                                                
         CLI   ERRAREA,0                                                        
         BNE   EXMODALT                                                         
         SPACE 1                                                                
         EJECT                                                                  
         SPACE 1                                                                
         CLI   BSCRN,X'E7'         BUDGET SCREEN IN PROCESS?                    
         BNE   FEDT0290            NO  - SKIP NEXT TESTS                        
         CLI   NOERRMSG,C'Y'       'ERROR' MESSAGE FROM OVERLAY?                
         BE    FEDT0290            YES - DON'T OVERWRITE                        
         MVC   LFMMSG(60),SPACES   SPACE OUT LINE                               
FEDT0290 CLI   BACT,C'A'           TEST ADD                                     
         BNE   FEDT0300                                                         
         TM    SVPGPBIT,X'01'      AUTO-ASSIGN FOR ADV/AGY?                     
         BNO   FEDT0297            NO                                           
         CLI   BREC,X'08'          YES - ADVERT ADD?                            
         BE    FEDT0293            YES                                          
         CLI   BREC,X'0A'          NO  - AGENCY ADD?                            
         BNE   FEDT0297                                                         
FEDT0293 EQU   *                                                                
         MVC   LFMMSG+18(22),=C'  KEY AUTO-ASSIGNED **'                         
FEDT0297 EQU   *                                                                
         LA    R2,LFMRECH                                                       
         MVC   LFMMSG(18),=C'** RECORD ADDED **'                                
         B     EXITALT                                                          
         SPACE 1                                                                
FEDT0300 CLI   BACT,C'C'           TEST CHANGE                                  
         BNE   FEDT0340                                                         
         CLI   BFMTSW,0            WAS THIS A FMT                               
         BNE   FEDT0330            NO                                           
         MVI   BFMTSW,1            SET FOR EDIT NEXT TIME                       
         CLI   BSCRN,X'E7'         BUDGET SCREEN IN PROCESS?                    
         BNE   FEDT0310            NO  - SKIP NEXT TESTS                        
         CLI   NOERRMSG,C'Y'       'ERROR' MESSAGE FROM OVERLAY?                
         BE    FEDT0320            YES - DON'T OVERWRITE                        
FEDT0310 EQU   *                                                                
         MVC   LFMMSG(24),=C'** ENTER AMENDED DATA **'                          
FEDT0320 EQU   *                                                                
         LA    R2,LFMLAST                                                       
         SPACE 1                                                                
         SR    R0,R0               POINT TO FIRST DATA FIELD                    
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         PROTECTED                                    
         BO    *-10                                                             
         B     EXITALT                                                          
         SPACE 1                                                                
FEDT0330 MVC   LFMMSG(29),=C'** RECORD HAS BEEN CHANGED **'                     
         LA    R2,LFMACTH                                                       
         B     EXITALT                                                          
         SPACE 1                                                                
FEDT0340 CLI   BACT,C'D'                                                        
         BNE   FEDT0370                                                         
         CLI   BSCRN,X'E7'         BUDGET SCREEN IN PROCESS?                    
         BNE   FEDT0350            NO  - SKIP NEXT TESTS                        
         CLI   NOERRMSG,C'Y'       'ERROR' MESSAGE FROM OVERLAY?                
         BE    FEDT0360            YES - DON'T OVERWRITE                        
FEDT0350 EQU   *                                                                
         MVC   LFMMSG(32),=C'** REQUESTED RECORD DISPLAYED **'                  
FEDT0360 EQU   *                                                                
         LA    R2,LFMACTH                                                       
         MVI   BFMTSW,1                                                         
         B     EXITALT                                                          
         SPACE 1                                                                
FEDT0370 CLI   BACT,C'T'           TRANSFER                                     
         BNE   FEDT0380                                                         
         MVI   BFMTSW,0                                                         
         MVC   LFMMSG(29),=C'DATA TRANSFERRED - ENTER NEXT'                     
         LA    R2,LFMACTH                                                       
         B     EXITALT                                                          
         SPACE 1                                                                
FEDT0380 CLI   BACT,C'M'                                                        
         BNE   FEDT0390                                                         
         MVC   BFMTSW,0                                                         
         MVC   LFMMSG(L'MATMSG),MATMSG                                          
         B     EXMODALT                                                         
MATMSG   DC    CL58'SUGGESTED MATCHES DISPLAYED. AMEND OR HIT ENTER TO X        
               ACCEPT.'                                                         
         SPACE 1                                                                
FEDT0390 CLI   BACT,C'X'           DELETE                                       
         BNE   FEDT0400                                                         
         LA    R2,LFMACTH                                                       
         MVC   LFMMSG(14),=C'RECORD DELETED'                                    
         CLI   BFMTSW,1            WAS THIS EDIT MODE                           
         BE    EXITALT             THEN ITS DELETED                             
         SPACE 1                                                                
         MVC   LFMMSG(38),=C'RECORD DISPLAYED - NOW YOU MAY DELETE '            
         MVI   BFMTSW,1            NEXT TIME I'LL DO IT                         
         B     EXITALT                                                          
FEDT0400 CLI   BACT,C'L'                                                        
         BNE   FEDT0410                                                         
         MVC   LFMMSG(32),=C'** REQUESTED RECORD DISPLAYED **'                  
         LA    R2,LFMACTH                                                       
         MVI   BFMTSW,0            ALWAYS SETS FRMT SWITCH TO ZERO              
         B     EXITALT                                                          
         SPACE 1                                                                
FEDT0410 LA    R2,LFMACTH                                                       
         B     EXITALT                                                          
         EJECT                                                                  
*                                                                               
*- PROFILE RECORD USES SAME KEY AS REP.                                         
PROFKEY  EQU   *                                                                
*                                                                               
REPKEY   LA    R3,REPERR                                                        
         CLI   5(R2),2                                                          
         BNE   ERRORALT                                                         
REPK2    MVC   BKEY+25(2),8(R2)                                                 
         B     FEDT0090                                                         
         EJECT                                                                  
STAKEY   MVC   BKEY+20(2),REPALPHA                                              
         OC    BKEY+22(5),SPACES                                                
         LA    R3,STAERR                                                        
         LA    R4,8(R2)                                                         
         LA    R5,4                                                             
         LA    R1,BKEY+22                                                       
         BAS   RE,TESTAN           TEST AND MOVE FIELD                          
         TM    BYTE,X'04'          TEST ALPHA                                   
         BZ    ERRORALT                                                         
         CH    R5,=H'3'            AT LEAST 3 CHARS                             
         BL    ERRORALT                                                         
         CLI   0(R4),C'-'          CHECK FOR - TO NAME BAND                     
         BNE   STAK2A                                                           
         CLI   1(R4),C'T'                                                       
         BE    STAK2               IF TV LEAVE BAND BLANK                       
         MVC   BKEY+26(1),1(R4)    MOVE BAND                                    
         CLI   1(R4),C'A'                                                       
         BE    STAK2                                                            
         CLI   1(R4),C'F'                                                       
         BE    STAK2                                                            
         CLI   1(R4),C'C'          COMBINED STATION                             
         BE    STAK2                                                            
         B     ERRORALT                                                         
STAK2    LA    R5,2(R5)                                                         
STAK2A   STC   R5,BYTE                                                          
         CLC   BYTE,5(R2)          THIS SHOULD BE INPUT LEN                     
         BNE   ERRORALT                                                         
         B     FEDT0090                                                         
         EJECT                                                                  
RGNKEY   MVC   BKEY+23(2),REPALPHA                                              
         LA    R3,RGNERR                                                        
         CLI   5(R2),2                                                          
         BNE   ERRORALT                                                         
         TM    4(R2),X'0C'                                                      
         BZ    ERRORALT                                                         
         MVC   BKEY+25(2),8(R2)                                                 
         B     FEDT0090                                                         
         SPACE 2                                                                
OFCKEY   MVC   BKEY+23(2),REPALPHA                                              
         LA    R3,OFCERR                                                        
         CLI   5(R2),2                                                          
         BH    ERRORALT                                                         
         TM    4(R2),X'0C'                                                      
         BZ    ERRORALT                                                         
         MVC   BKEY+25(2),8(R2)                                                 
         OC    BKEY+25(2),SPACES                                                
         B     FEDT0090                                                         
         SPACE 2                                                                
TMKEY    MVC   BKEY+23(2),REPALPHA                                              
         LA    R3,TMERR                                                         
         CLI   5(R2),2                                                          
         BH    ERRORALT                                                         
         MVC   BKEY+25(2),8(R2)                                                 
         OC    BKEY+25(2),SPACES                                                
         B     FEDT0090                                                         
         SPACE 2                                                                
SLKEY    MVC   BKEY+22(2),REPALPHA                                              
         LA    R3,SLERR                                                         
         CLI   5(R2),2                                                          
         BL    ERRORALT                                                         
         CLI   5(R2),3                                                          
         BH    ERRORALT                                                         
****     TM    4(R2),X'0C'         DEACTIVATE NUM/ALPHA TEST                    
****     BNO   ERRORALT                                                         
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVSL                                                          
         OC    BKEY+24(3),SPACES                                                
         B     FEDT0090                                                         
MVSL     MVC   BKEY+24(0),8(R2)                                                 
         EJECT                                                                  
ADVKEY   LA    R3,ADVERR                                                        
         CLI   5(R2),4                                                          
         BH    ERRORALT                                                         
         CLI   5(R2),2                                                          
         BL    ERRORALT                                                         
         SPACE 1                                                                
         SPACE 1                                                                
ADVK4    DS    0H                                                               
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVADV                                                         
         OC    BKEY+21(4),SPACES                                                
         MVC   BKEY+25(2),REPALPHA                                              
         B     FEDT0090                                                         
MVADV    MVC   BKEY+21(0),8(R2)                                                 
         EJECT                                                                  
         SPACE 2                                                                
PRDKEY   LA    R3,ADVERR                                                        
         OC    BKEY+18(7),SPACES                                                
         MVC   BKEY+25(2),REPALPHA                                              
         LA    R1,BKEY+18          EDIT ADV                                     
         LA    R4,8(R2)                                                         
         LA    R5,4                                                             
         BAS   RE,TESTAN                                                        
         CH    R5,=H'2'            AT LEAST 2 CHARS                             
         BL    ERRORALT                                                         
         LA    R5,1(R5)                                                         
         STH   R5,HALF             SAVE LN + STOP CHAR                          
         LA    R3,PRDERR                                                        
         LA    R1,BKEY+22          EDIT PRD                                     
         LA    R4,1(R4)                                                         
         LA    R5,3                                                             
         BAS   RE,TESTAN                                                        
         CH    R5,=H'2'            AT LEAST 2 CHARS                             
         BL    ERRORALT                                                         
         AH    R5,HALF             GET TOTAL INPUT LEN                          
         STC   R5,BYTE                                                          
         CLC   BYTE,5(R2)          DO WE HAVE ALL THE INPUT                     
         BNE   ERRORALT                                                         
* VERIFY ADVTSR ON FILE                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,8                                                            
         MVC   KEY+21(4),BKEY+18   ADV                                          
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    PRDKEYX                                                          
         MVC   KEYSAVE+25(2),=C'ZZ' SEE IF WE HAVE DEFAULT                      
         CLC   KEYSAVE(27),KEY                                                  
         BE    PRDKEYX                                                          
         MVC   KEY,KEYSAVE                                                      
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    PRDKEYX                                                          
         LA    R3,ADVNFERR                                                      
         B     ERRORALT                                                         
         SPACE 1                                                                
PRDKEYX  B     FEDT0090                                                         
         EJECT                                                                  
AGYKEY   LA    R3,AGYERR                                                        
         OC    BKEY+19(6),SPACES                                                
         MVC   BKEY+25(2),REPALPHA                                              
         LA    R1,BKEY+19          EDIT AGENCY CODE                             
         LA    R4,8(R2)                                                         
         LA    R5,4                                                             
         BAS   RE,TESTAN                                                        
         CH    R5,=H'2'            AT LEAST 2 CHARS                             
         BL    ERRORALT                                                         
         SPACE 1                                                                
         SPACE 1                                                                
AGYK1B   DS    0H                                                               
         CLI   0(R4),C' '          CHECK FOR STOP CHAR                          
         BNH   AGYK2               NO                                           
         LA    R5,1(R5)                                                         
         STH   R5,HALF             SAVE INPUT LEN SO FAR                        
         LA    R3,AGOFCERR                                                      
         LA    R1,BKEY+23                                                       
         LA    R4,1(R4)                                                         
         LA    R5,2                                                             
         BAS   RE,TESTAN                                                        
         TM    BYTE,X'0C'                                                       
         BZ    ERRORALT                                                         
         LTR   R5,R5               AT LEAST 1 CHAR                              
         BZ    ERRORALT                                                         
         SPACE 1                                                                
         AH    R5,HALF             GET TOTAL INPUT LEN                          
AGYK2    STC   R5,BYTE                                                          
         CLC   BYTE,5(R2)          DO WE HAVE ALL INPUT                         
         BNE   ERRORALT                                                         
         SPACE 1                                                                
         CLC   BKEY+23(2),SPACES   IS THIS A DEFAULT REC                        
         BE    FEDT0090            YES                                          
* NO - MAKE SURE DEFAULT IS THERE                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),BKEY                                                     
         MVC   KEY+23(2),SPACES                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    FEDT0090                                                         
         LA    R3,AGOFCER1                                                      
         B     ERRORALT                                                         
         SPACE 2                                                                
GRPKEY   MVC   BKEY+23(2),REPALPHA                                              
         LA    R3,GRPERR                                                        
         CLI   5(R2),2                                                          
         BH    ERRORALT                                                         
         MVC   BKEY+25(2),8(R2)                                                 
         OC    BKEY+25(2),SPACES                                                
         B     FEDT0090                                                         
         SPACE 2                                                                
CLSKEY   MVC   BKEY+23(2),REPALPHA                                              
         LA    R3,CLSERR                                                        
CLSKEY2  CLI 5(R2),2                                                            
         BH    ERRORALT                                                         
         MVC   BKEY+25(2),8(R2)                                                 
         OC    BKEY+25(2),SPACES                                                
         B     FEDT0090                                                         
         SPACE 2                                                                
CTGKEY   MVC   BKEY+23(2),REPALPHA                                              
         LA    R3,CTGERR                                                        
         B     CLSKEY2                                                          
         EJECT                                                                  
RPTKEY   LA    R3,RPTERR                                                        
         LA    R1,BKEY+21          EDIT REPORT                                  
         LA    R4,8(R2)                                                         
         LA    R5,2                                                             
         BAS   RE,TESTAN                                                        
         TM    BYTE,X'08'                                                       
         BZ    ERRORALT                                                         
         CH    R5,=H'2'                                                         
         BNE   ERRORALT                                                         
         SPACE 1                                                                
         LA    R3,REPERR                                                        
         LA    R1,BKEY+23          EDIT REP                                     
         LA    R4,1(R4)                                                         
         LA    R5,2                                                             
         BAS   RE,TESTAN                                                        
         LR    RE,R4               ALLOW TCH REP                                
         SH    RE,=H'2'            BACK UP TO FIRST INPUT CHAR                  
         CLI   0(RE),C'T'          IF FIRST CHAR T, NO A/N TEST                 
         BE    *+12                                                             
         TM    BYTE,X'04'                                                       
         BZ    ERRORALT                                                         
         CH    R5,=H'2'                                                         
         BNE   ERRORALT                                                         
         LA    R3,OFCERR                                                        
         SPACE 1                                                                
         LA    R1,BKEY+25          EDIT OFFICE                                  
         LA    R4,1(R4)                                                         
         LA    R5,2                                                             
         BAS   RE,TESTAN                                                        
         CH    R5,=H'2'                                                         
         BNE   ERRORALT                                                         
         B     FEDT0090                                                         
         EJECT                                                                  
BUDKEY   EQU   *                                                                
         MVI   BCONTYP,C' '        SET BUDGET CONTRACT TYPE                     
         MVC   BKEY+16(2),REPALPHA        REP                                   
         SPACE 1                                                                
         LA    R1,BKEY+18          EDIT YEAR                                    
         LA    R3,2                INVALID INPUT FIELD (YEAR)                   
         LA    R4,8(R2)                                                         
         LA    R5,2                                                             
         BAS   RE,TESTAN                                                        
         SPACE 1                                                                
         TM    BYTE,X'08'          MUST BE NUMERIC                              
         BZ    ERRORALT                                                         
         CH    R5,=H'2'            MUST BE 2 CHARACTERS                         
         BNE   ERRORALT                                                         
         SPACE 1                                                                
         LA    R5,1(R5)                                                         
         STH   R5,HALF             SAVE LENGTH + STOP CHARACTER                 
         SPACE 1                                                                
         LA    R3,STAERR                                                        
         LA    R1,BKEY+20          EDIT STATION                                 
         OC    BKEY+20(5),SPACES                                                
         LA    R4,1(R4)                                                         
         LA    R5,4                                                             
         BAS   RE,TESTAN                                                        
         SPACE 1                                                                
         TM    BYTE,X'04'          MUST BE ALPHA                                
         BZ    ERRORALT                                                         
         CH    R5,=H'3'            AT LEAST 3 CHARACTERS                        
         BL    ERRORALT                                                         
         CLI   0(R4),C'-'          CHECK FOR - TO MOVE BAND                     
         BNE   BUDK20                                                           
         SPACE 1                                                                
         CLI   1(R4),C'T'                                                       
         BE    BUDK15              IF TV,LEAVE BAND BLANK                       
         SPACE 1                                                                
         MVC   BKEY+24(1),1(R4)    MOVE BAND                                    
         CLI   1(R4),C'A'                                                       
         BE    BUDK15                                                           
         CLI   1(R4),C'F'                                                       
         BE    BUDK15                                                           
         CLI   1(R4),C'C'          COMBINED STATION                             
         BNE   ERRORALT                                                         
*                                                                               
*   CHECK COMBO STATIONS FOR DATE IN REQUEST LINE.  IF AFTER 1992,              
*       PROHIBIT ENTRY TO PROGRAM. (1993-2079 WILL BE TESTED)                   
*     R2  =  FIELD HEADER ADDRESS.  1ST TWO POSITIONS HAVE ALREADY              
*            BEEN VALIDATED AS YEAR OF DATA                                     
*                                                                               
         LA    RF,8(R2)            A(DATA WITHIN FIELD)                         
         CLI   0(RF),C'9'          1990 DECADE?                                 
         BL    BUDK10              NO  - COULD BE 2000-2020                     
         CLC   0(2,RF),=C'93'      YES - 1993 AND LATER PROHIBITED              
         BL    BUDK15              ACCEPTED                                     
         B     BUDK12              PROHIBITED - DISPLAY ERROR                   
*                                                                               
*   NEXT TEST IS FOR 2000 - 2079.  IF DATE ENTERED IS IN 80 DECADE,             
*      IT WILL BE ACCEPTED.                                                     
*                                                                               
BUDK10   EQU   *                                                                
         CLC   0(2,RF),=C'80'      2000 - 2079 TEST:  PROHIBITED                
         BNL   BUDK15              ACCEPTED                                     
BUDK12   EQU   *                                                                
         MVC   LFMMSG(L'NOCOMBO),NOCOMBO                                        
         FOUT  LFMMSGH                                                          
         LA    R2,LFMACTH                                                       
         B     EXITALT                                                          
         SPACE 1                                                                
BUDK15   LA    R5,2(R5)                                                         
         LA    R4,2(R4)                                                         
         SPACE 1                                                                
*  VERIFY STATION ON FILE                                                       
BUDK20   XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),BKEY+20                                                
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ERRORALT                                                         
         CLI   BACT,C'L'           'ALLOC' REQUEST?                             
         BNE   BUDK22              NO                                           
         MVI   OFFALLOC,C'Y'       SET OFFICE ALLOCATION FLAG TO 'YES'          
BUDK22   EQU   *                                                                
         CLI   0(R4),C' '          CHECK FOR STOP CHARACTER                     
         BH    BUDK25              NO STOP CHARACTER                            
         CLI   BACT,C'L'           'ALLOCATE' REQUEST?                          
         BNE   BUDK24              NO  - TEST NEEDED                            
         MVI   OFFALLOC,C'N'       SET OFFICE ALLOCATION FLAG TO 'NO'           
         B     BUDK35                                                           
BUDK24   EQU   *                                                                
         BAS   R8,BUDCHEK          MANDATORY OFFICE/CONTRACT TYPE?              
         B     BUDK35              RETURN                                       
         SPACE 1                                                                
BUDK25   EQU   *                                                                
         LA    R5,1(R5)                                                         
         AH    R5,HALF                                                          
         STH   R5,HALF                                                          
         SPACE 1                                                                
*  EDIT OFFICE OR TEAM                                                          
         LA    R3,156              INVALID OFFICE OR TEAM                       
         LA    R1,BKEY+25                                                       
         OC    BKEY+25(2),SPACES                                                
         LA    R4,1(R4)                                                         
         LA    R5,2                MAX 2 CHARACTERS                             
         BAS   RE,TESTAN                                                        
         SPACE 1                                                                
*  VERIFY OFFICE OR TEAM ON FILE                                                
         XC    KEY,KEY                                                          
         MVI   KEY,4               CHECK OFFICE                                 
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),BKEY+25                                                
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    BUDK30              VALID OFFICE                                 
         SPACE 1                                                                
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY,5               CHECK TEAM                                   
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ERRORALT            NEITHER TEAM NOR OFFICE IS VALID             
         SPACE 1                                                                
BUDK30   AH    R5,HALF                                                          
         STC   R5,BYTE                                                          
         STH   R5,HALF             ALSO STORE HALFWORD                          
         SPACE 1                                                                
         CLC   BYTE,5(R2)          DO WE HAVE ALL THE INPUT                     
         BNE   BUDK32              NO  - CONTINUE TO CHECK                      
*                                                                               
*   IF ALL INPUT HAS BEEN CHECKED, TWO CONDITIONS NEED TESTING:                 
*      1.  IF 'ALLOCATE' REQUEST, WHATEVER HAS BEEN INPUT IS                    
*          SUFFICIENT - NO FURTHER CHECK NEEDED                                 
*      2.  IF NOT 'ALLOCATE,' MUST SEE IF OFFICE/CONTRACT TYPE                  
*          WERE MANDATORY.  IF YES, ERROR HAS OCCURRED.                         
*                                                                               
         CLI   BACT,C'L'           'ALLOCATE' REQUEST?                          
         BE    BUDK35              YES - NO TEST NEEDED                         
         BAS   R8,BUDCHEK          MANDATORY OFFICE/CONTRACT TYPE?              
         B     BUDK35              RETURN                                       
BUDK32   EQU   *                                                                
         CLI   BACT,C'L'           'ALLOCATE' REQUEST?                          
         BNE   BUDK32D             NO                                           
         MVC   LFMMSG(L'CTNP),CTNP MSG 'CONTRACT TYPE NOT PERMITTED'            
         FOUT  LFMMSGH                                                          
         LA    R2,LFMACTH          SET CURSOR POSITION                          
         B     EXITALT             EXIT                                         
BUDK32D  EQU   *                                                                
         LA    R1,BCONTYP          NO  - CHECK FOR CONTRACT TYPE                
         LA    R5,1                SET LENGTH + UPDATE LENGTH COUNT             
         AH    R5,HALF                                                          
         STH   R5,HALF             ADD STOP CHARACTER COUNT                     
         STC   R5,BYTE             ANYTHING AFTER STOP CHARACTER?               
         CLC   BYTE,5(R2)                                                       
         BE    ERRORALT            NO  - TERMINATE                              
         LA    R4,1(R4)            A(NEXT INPUT CHARACTER)                      
         BAS   RE,TESTAN PUTS NEXT CHAR IN (R1)                                 
         AH    R5,HALF                                                          
         STC   R5,BYTE                                                          
         CLC   BYTE,5(R2)          DO WE HAVE ALL THE INPUT?                    
         BNE   ERRORALT                                                         
*                                                                               
*  VERIFY CONTRACT TYPE ENTERED FOR VALIDITY, GET DESCRIPTION                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'           SET CODE FOR CTY RECORD                      
         MVC   KEY+24(2),REPALPHA                                               
         MVC   KEY+26(1),BCONTYP   LOAD CONTRACT TYPE ENTERED                   
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    BUDK33              RECORD FOUND - GET DESCRIPTION               
         MVC   LFMMSG(L'CTNR),CTNR MESSAGE 'CONTRACT TYPE N.G.'                 
         FOUT  LFMMSGH                                                          
         LA    R2,LFMACTH          SET CURSOR POSITION                          
         B     EXITALT             EXIT                                         
*                                                                               
CTNR     DC    CL39'CONTRACT TYPE INVALID - PLEASE RE-ENTER'                    
CTNP     DC    CL39'CONTRACT TYPE NOT PERMITTED            '                    
*                                                                               
BUDK33   DS    0H                                                               
         BAS   RE,GETREC           RETRIEVE CONTRACT TYPE RECORD                
         MVC   BCONDESC,RCTYDESC   SAVE CONTRACT TYPE DESC                      
         SPACE 1                                                                
* GET FISCAL START MONTH FROM REP RECORD                                        
         SPACE 1                                                                
BUDK35   EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,1                                                            
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ERRORALT                                                         
         SPACE 1                                                                
         BAS   RE,GETREC                                                        
         GOTO1 VGETEL,DMCB,(X'01',AIOAREA),DMCB+8                               
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         L     R5,DMCB+8                                                        
         USING RREPELEM,R5                                                      
         MVC   STARTMO,RREPFMON                                                 
         DROP  R5                                                               
         B     FEDT0090                                                         
         SPACE 4                                                                
*                                                                               
*   CHECK PROFILE TO DETERMINE IF OFFICE AND CONTRACT TYPE ARE                  
*        MANDATORY.  IF YES, ERROR OUT.                                         
*                                                                               
BUDCHEK  EQU   *                                                                
         LA    R5,SVPGPBIT         PROGRAM PROFILE BITS                         
         TM    0(R5),X'10'         3RD BIT ON = MANDATORY FIELDS                
         BNO   CB099               NOT REQUIRED                                 
         LR    R5,RA                                                            
         USING TWAD,R5                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    CB099               YES - IGNORE TEST                            
         DROP  R5                                                               
         MVC   LFMMSG(L'NOC),NOC   MESSAGE 'OFFICE/CONTYPE REQUIRED'            
         FOUT  LFMMSGH                                                          
         LA    R2,LFMACTH          SET CURSOR                                   
         B     EXITALT             EXIT                                         
         SPACE 2                                                                
NOC      DC    CL39'OFFICE AND CONTRACT TYPE CODES REQUIRED'                    
NOCOMBO  DC    CL39'COMBO STATION PROHIBITED AFTER 1992    '                    
         SPACE 2                                                                
CB099    DS    0H                                                               
         BR    R8                  RETURN                                       
         EJECT                                                                  
OBUDKEY  EQU   *                                                                
         GOTO1 =A(OFFBUDKY),DMCB,(RC),RR=BASERELO                               
         BZ    FEDT0090            NO ERROR - RETURN                            
         L     R3,FULL             RESET ERROR PASS-BACK CODE                   
         LTR   R3,R3               ANY PASS-BACK CODE?                          
         BNZ   ERRORALT            YES - USE IT                                 
         B     EXITALT             NO  - ERROR MESSAGE RETURN                   
         EJECT                                                                  
*  BUILD CONTRACT KEY (8C)                                                      
FIXKEY   MVC   BKEY+21(2),REPALPHA                                              
         SPACE 1                                                                
*  GET 9'S COMPLEMENT OF CONTRACT                                               
         ZAP   DUB(5),=P'99999999'                                              
         CLI   5(R2),7             PAD WITH ZEROES IF LESS THAN 8 CHARS         
         BH    FIXKEY50                                                         
         MVC   WORK(8),=8C'0'      PADDED CONT # WILL BE HERE                   
         LA    R1,8                                                             
         ZIC   RF,5(R2)                                                         
         SR    R1,RF               FIND NO. OF ZERO TO PAD ON LEFT              
         LA    RF,WORK                                                          
         LA    RF,0(R1,RF)                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)       RIGHT JUSTIFY THE CONT #                     
         MVC   8(8,R2),WORK        MOVE CONT # BACK                             
         OI    6(R2),X'80'         XMIT                                         
         XC    WORK(8),WORK        CLEAR WORK                                   
FIXKEY50 PACK  WORK(5),8(8,R2)     TAKE FIRST 8 CHARS                           
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         SPACE 1                                                                
         MVC   BKEY+23(4),WORK                                                  
         B     FEDT0090                                                         
         EJECT                                                                  
* DEMO MENU KEY CAN BE 2 ALPHA/NUMERIC CHARACTERS                               
         SPACE 1                                                                
DMENKEY  MVC   BKEY+23(2),REPALPHA       REP                                    
         LA    R3,2                      INVALID INPUT FIELD                    
         CLI   5(R2),2                                                          
         BNE   ERRORALT                                                         
         MVC   BKEY+25(2),8(R2)                                                 
         B     FEDT0090                                                         
         SPACE 2                                                                
OWNKEY   MVC   BKEY+22(2),REPALPHA                                              
         LA    R3,2                INVALID INPUT FIELD                          
         CLI   5(R2),3                                                          
         BNE   ERRORALT                                                         
         MVC   BKEY+24(3),8(R2)                                                 
         B     FEDT0090                                                         
         SPACE 2                                                                
*                                                                               
*- STANDARD COMMENT KEY                                                         
CMTKEY   MVC   BKEY+15(2),REPALPHA REP CODE                                     
         MVC   BKEY+17(2),=X'FFFF' OFF CODE (USED IN SFM)                       
         MVC   BKEY+19(8),8(R2)    COMMENT CODE                                 
         OC    BKEY+19(8),SPACES   PAD W/BLANKS                                 
*                                                                               
         LA    R3,INVERR                                                        
         CLI   5(R2),8                                                          
         BH    ERRORALT            TOO MANY CHARS IN KEY                        
         B     FEDT0090                                                         
         SPACE 2                                                                
*                                                                               
*- POINT PERSON KEY                                                             
PTPKEY   MVC   BKEY+22(2),REPALPHA   REP CODE                                   
         MVC   BKEY+24(3),8(R2)      POINT PERSON INITIALS.                     
         OC    BKEY+24(3),SPACES     PAD W/BLANKS                               
*                                                                               
         LA    R3,INVERR                                                        
         CLI   5(R2),3                                                          
         BH    ERRORALT            TOO MANY CHARS IN KEY                        
         B     FEDT0090                                                         
         SPACE 2                                                                
*                                                                               
*- CONTRACT TYPE KEY                                                            
CTYKEY   MVC   BKEY+24(2),REPALPHA   REP CODE                                   
         MVC   BKEY+26(1),8(R2)      TYPE CODE                                  
*                                                                               
         LA    R3,INVERR                                                        
         CLI   5(R2),1                                                          
         BH    ERRORALT            TOO MANY CHARS IN KEY                        
*                                                                               
         CLI   8(R2),C'*'          ASTERISK NOT ALLOWED                         
         BE    ERRORALT                                                         
         B     FEDT0090                                                         
         EJECT                                                                  
*                                                                               
*- BUSINESS DEVELOPMENT CONTRACT TYPE KEY                                       
DCTKEY   MVC   BKEY+23(2),REPALPHA   REP CODE                                   
         MVC   BKEY+25(2),8(R2)      TYPE CODE                                  
         OC    BKEY+25(2),SPACES     SET X'00' TO X'40'                         
*                                                                               
         LA    R3,INVERR                                                        
         CLI   5(R2),2                                                          
         BH    ERRORALT            TOO MANY CHARS IN KEY                        
*                                                                               
         CLI   8(R2),C'*'          ASTERISK NOT ALLOWED                         
         BE    ERRORALT                                                         
         B     FEDT0090                                                         
         EJECT                                                                  
*  VERIFY EOM (END OF ACCOUNTING MONTH) KEY                                     
*     INPUT IS YEAR, I.E. 84                                                    
         SPACE 1                                                                
EOMKEY   DS    0H                                                               
         MVC   BKEY+24(2),REPALPHA REP                                          
         LA    R3,2                                                             
         CLI   5(R2),2             MUST BE 2 CHARACTERS                         
         BNE   ERRORALT                                                         
         SPACE 1                                                                
         LA    R3,3                                                             
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERRORALT                                                         
         SPACE 1                                                                
         PACK  DUB,8(2,R2)                                                      
         CVB   R5,DUB                                                           
         STC   R5,BKEY+26                                                       
         SPACE 1                                                                
         B     FEDT0090                                                         
         EJECT                                                                  
ERRKEY   LA    R3,INVERR                                                        
         CLI   5(R2),3                                                          
         BH    ERRORALT                                                         
         TM    4(R2),X'08'                                                      
         BZ    ERRORALT                                                         
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERRORALT                                                         
         CH    R0,=H'255'                                                       
         BH    ERRORALT                                                         
         STC   R0,BKEY+26                                                       
         MVI   BKEY+25,8           SET SYS NUMBER                               
         B     FEDT0090                                                         
         EJECT                                                                  
INVKEY   LA    R3,INVERR                                                        
         CLC   LFMREC(3),=C'TEX'                                                
         BNE   *+10                                                             
         CLC   8(3,R2),=C'LIB'                                                  
         BNE   *+8                                                              
         B     FEDT0180                                                         
         SPACE 1                                                                
         GOTO1 VPAVSTA,DMCB,8(R2),WORK                                          
         CLI   DMCB+4,X'FF'                                                     
         BE    ERRORALT                                                         
         CLC   WORK(4),=C'WKAQ'    TEST FOR SPECIAL BLAIR STATION               
         BE    FEDT0180                                                         
         GOTO1 VSTAGET,DMCB,WORK,REC                                            
         LA    R3,ERRNF                                                         
         CLI   DMCB+4,X'FF'             VALID STATION FIELD                     
         BE    ERRORALT                                                         
* STATION RECORD MUST EXIST                                                     
         LA    R3,112             STATION RECORD NOT ON FILE                    
         GOTO1 =A(FINDSTAT),DMCB,(RC),RR=BASERELO                               
         BNZ   ERRORALT                                                         
         B     FEDT0180            DONT CHECK FOR DUPS                          
         EJECT                                                                  
*              THIS ROUTINE DELETES ALL ELEMENTS WITH GIVEN CODE                
*              PARAMETER 1 =       BYTE  0   = ELEMENT CODE TO DELETE           
*                                  BYTES 1-3 = A(RECORD)                        
DELELEM  NTR1  BASE=BASERB                                                      
         L     R9,BASER9                                                        
*                                                                               
         L     R2,0(R1)            A(RECORD)                                    
         MVC   BYTE2,0(R1)         ELEM CODE                                    
*                                                                               
*DEL100   MVC   HALF,27(R2)         REC LEN                                     
*         LH    R5,HALF                                                         
*         LA    R5,0(R5,R2)         REC END                                     
DEL100   LA    R5,34(R2)           POINT TO FIRST ELEMENT                       
         SR    R4,R4                                                            
DEL130   CLI   0(R5),0                                                          
         BE    DEL150                                                           
         IC    R4,1(R5)                                                         
         AR    R5,R4                                                            
         B     DEL130                                                           
DEL150   BCTR  R5,R0               FOR BXLE                                     
         SR    R4,R4                                                            
*                                                                               
         LA    R3,34(R2)           1ST ELEM                                     
         CLC   0(1,R3),BYTE2       ELEM CODE MATCH?                             
         BE    DELETEL                                                          
         IC    R4,1(R3)            ELEM LEN                                     
*                                                                               
         BXLE  R3,R4,*-14          LOOP THRU ELEMENTS                           
         B     EXXMOD                                                           
*                                                                               
DELETEL  GOTO1 VRECUP,DMCB+12,(2,(R2)),(R3),(R3)  DELETE ELEMENT                
         B     DEL100                                                           
         EJECT                                                                  
*              PARAMETER 1 =       A(RECORD)                                    
*              PARAMETER 2 =       A(ELEMENT TO BE INSERTED)                    
*              ELEMENT IS ADDED IMMEDIATELY BEFORE HIGHER ELEM OR END           
ADDELEM  NTR1  BASE=BASERB                                                      
         L     R9,BASER9                                                        
         L     R2,0(R1)            A(RECORD)                                    
         L     R6,4(R1)            A(ELEMENT)                                   
*         MVC   HALF,27(R2)         REC LEN                                     
*         LH    R5,HALF                                                         
*         LA    R5,0(R5,R2)         REC END                                     
         LA    R5,34(R2)           POINT TO FIRST ELEMENT                       
         SR    R4,R4                                                            
ADD130   CLI   0(R5),0                                                          
         BE    ADD150                                                           
         IC    R4,1(R5)                                                         
         AR    R5,R4                                                            
         B     ADD130                                                           
ADD150   BCTR  R5,R0               FOR BXLE                                     
         SR    R4,R4                                                            
*                                                                               
         LA    R3,34(R2)           FIRST ELEM                                   
*                                                                               
         CLC   0(1,R6),0(R3)       NEW ELEM CODE V REC ELEM CODE                
         BL    *+12                                                             
         IC    R4,1(R3)            ELEM LEN                                     
*                                                                               
         BXLE  R3,R4,*-14          LOOP THRU ELEMENTS                           
*                                                                               
         GOTO1 VRECUP,DMWORK+84,(2,(R2)),(R6),(R3)  ADD ELEMENT                 
         B     EXXMOD                                                           
         EJECT                                                                  
*              PARAMETER 1 =       A(FROM RECORD AREA)                          
*              PARAMETER 2 =       A(TO RECORD AREA)                            
MOVEREC  NTR1  BASE=BASERB                                                      
         L     R9,BASER9                                                        
*                                                                               
         L     R2,0(R1)            FROM REC                                     
         L     R3,4(R1)            TO REC                                       
*                                                                               
         MVC   HALF,27(R2)         FROM REC LEN                                 
         LH    R5,HALF                                                          
         LA    R4,0(R5,R2)                                                      
         MVI   0(R4),0             FOR RECORDS ADDED BY HELLO                   
MOVE100  LTR   R5,R5                                                            
         BZ    MOVEXIT                                                          
*                                                                               
         CH    R5,=H'250'                                                       
         BNH   MOVEREST                                                         
         MVC   0(250,R3),0(R2)                                                  
         LA    R2,250(R2)                                                       
         LA    R3,250(R3)                                                       
         SH    R5,=H'250'                                                       
         B     MOVE100                                                          
MOVEREST BCTR  R5,R0                                                            
         EX    R5,MOVEVAR                                                       
MOVEXIT  L     R6,4(R1)                                                         
         LH    R5,27(R6)                                                        
         AR    R6,R5                                                            
         MVI   0(R6),0                                                          
         B     EXXMOD                                                           
MOVEVAR  MVC   0(0,R3),0(R2)                                                    
         EJECT                                                                  
*        P1=A(RECORD)    BYTE 0 = ELEM CODE SOUGHT BY USER (1ST ELEM            
*                                 ONLY RETURNED) SET TO X'FF' IF NONE           
*        P2=A(3 FULL WORD AREA FOR REGISTERS 3-5 FOR BXLE IN USER)              
GETEL    NTR1  BASE=BASERB                                                      
         L     R9,BASER9                                                        
         L     R2,0(R1)            A(RECORD)                                    
         L     R6,4(R1)            A(STORE AREA)                                
*                                                                               
*         MVC   HALF,27(R2)         LENGTH                                      
*         LH    R5,HALF                                                         
*         LA    R5,0(R2,R5)         REC END                                     
*                                                                               
         LA    R5,34(R2)           POINT TO FIRST ELEMENT                       
         SR    R4,R4                                                            
GET130   CLI   0(R5),0                                                          
         BE    GET150                                                           
         IC    R4,1(R5)                                                         
         AR    R5,R4                                                            
         B     GET130                                                           
GET150   BCTR  R5,R0               BXLE                                         
         SR    R4,R4                                                            
*                                                                               
         LA    R3,34(R2)           1ST ELEM                                     
*                                                                               
GETEL100 CLC   0(1,R1),0(R3)       ELEM CODE?                                   
         BNE   GETEL200                                                         
         STM   R3,R5,0(R6)         BXLE FOR USER                                
GETELXIT B     EXXMOD                                                           
GETEL200 IC    R4,1(R3)            ELEM LEN                                     
         BXLE  R3,R4,GETEL100                                                   
         MVI   0(R1),X'FF'         NOT FOUND INDICATOR                          
         B     GETELXIT                                                         
         EJECT                                                                  
*              PARAMETER 1 =       A(FIRST FIELD)                               
*                        2 =       A(END-ADDR) EX=BUYLAST                       
*                                                                               
FOUTBLK  NTR1  BASE=BASERB                                                      
         L     R9,BASER9                                                        
         L     R2,0(R1)            1ST FIELD                                    
         L     R3,4(R1)            LAST FIELD                                   
*                                                                               
         SR    RE,RE                                                            
FOUT100  IC    RE,0(R2)            LEN                                          
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    FOUT200                                                          
*                                                                               
         LR    RF,RE                                                            
         SH    RF,=H'10'                                                        
         MVI   8(R2),C' '                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   9(0,R2),8(R2)                                                    
         FOUT  (R2)                                                             
*                                                                               
FOUT200  LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3               LAST?                                        
         BL    FOUT100                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
*              PUT OUT MARKET NAME                                              
*              PARA 1    BYTE 1-3  A(STATION,MEDIA)                             
*              PARA 2    BYTE 1-3  A(OUTPUT FIELD HEADER)                       
         SPACE 1                                                                
MKTNME   NTR1  BASE=BASERB                                                      
         L     R9,BASER9                                                        
         LM    R7,R8,0(R1)                                                      
         CLC   0(4,R7),=C'WKAQ'    TEST FOR SPECIAL BLAIR STATION               
         BE    MKTNAME4                                                         
         GOTO1 VMRKGET,DMCB,(R7),IOAREA                                         
         LA    R3,ERRNF                                                         
         CLI   DMCB+4,X'FF'                                                     
         BE    ERRORALT                                                         
         SPACE 1                                                                
         USING DMKEY,R4                                                         
         LA    R4,IOAREA                                                        
         MVC   INVMKT,DMRMKT       SAVE MARKET NUMBER                           
         LA    R4,DMFRSTEL                                                      
         SPACE 1                                                                
         USING DMELEM,R4                                                        
MKTNME1  CLI   0(R4),0                                                          
         BE    EXXMOD                                                           
         CLI   0(R4),1             FIND NAME ELEMENT                            
         BE    MKTNME2                                                          
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     MKTNME1                                                          
         SPACE 1                                                                
MKTNME2  ZIC   R5,0(R8)                                                         
         SH    R5,=H'9'            LENGTH OF OUTPUT FIELD                       
         ZIC   R3,DMLEN                                                         
         SH    R3,=H'5'            LENGTH OF NAME                               
         BNP   EXXMOD                                                           
         CR    R3,R5               LENGTH OF NAME VS. LENGTH OF FIELD           
         BL    *+6                                                              
         LR    R3,R5               TAKE SMALLEST                                
         EX    R3,*+8                                                           
         B     EXXMOD                                                           
         MVC   8(0,R8),DMMNAME     MARKET NAME TO SCREEN                        
         SPACE 1                                                                
MKTNAME4 ZIC   R5,0(R8)                                                         
         SH    R5,=H'8'                                                         
         LA    R3,L'MKTN           LENGTH OF NAME                               
         CR    R5,R3               LENGTH OF FIELD VS. LENGTH OF NAME           
         BL    *+6                                                              
         LR    R5,R3                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8              MARKET IS SAN JUAN,P.R.                      
         B     EXXMOD                                                           
         MVC   8(0,R8),MKTN                                                     
         SPACE 1                                                                
MKTN     DC    CL14'SAN JUAN, P.R.'                                             
         DROP  R4                                                               
         EJECT                                                                  
*              GET A PAV MARKET RECORD                                          
*                                                                               
*              PARA 1    BYTE 1-3  A(STATION,MEDIA)                             
*              PARA 2    BYTE 1-3  A(AN IOAREA)                                 
*                                  BYTE 0 = FF IF NOT FOUND                     
MRKGET   NTR1  BASE=BASERB                                                      
         L     R9,BASER9                                                        
         LM    R2,R3,0(R1)                                                      
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         LA    R8,TRDEMOB                                                       
         USING DEMOD,R8                                                         
         ST    R1,MGPLIST                                                       
         STM   R2,R3,MGPARMS                                                    
         SPACE 1                                                                
         GOTO1 VSTAGET             GET THE STATION RECORD                       
         CLI   4(R1),X'FF'                                                      
         BE    MRKERR                                                           
         SPACE 1                                                                
         MVI   DBFUNCT,DBGETMK     READ MARKET RECORD                           
         ST    R3,DBAREC                                                        
         MVC   DBSELRMK,DBACTRMK   USE ACTUAL VALUE FROM STATION READ           
         SPACE 1                                                                
         GOTO1 CDEMAND,DMCB,DBLOCK,0                                            
         CLI   DBERROR,0                                                        
         BNE   MRKERR              COULD NOT FIND MARKET RECORD                 
         L     R1,MGPLIST                                                       
         STM   R2,R3,0(R1)                                                      
         B     EXXMOD                                                           
         SPACE 1                                                                
MRKERR   DS    0H                                                               
         L     R1,MGPLIST                                                       
         STM   R2,R3,0(R1)                                                      
         MVI   4(R1),X'FF'                                                      
         B     EXXMOD                                                           
         DROP  R7,R8                                                            
         EJECT                                                                  
*              GET A PAV STATION RECORD                                         
*                                                                               
*              PARA 1    BYTE 1-3  A(STATION,MEDIA)                             
*              PARA 2    BYTE 1-3  A(AN IOAREA)                                 
*                                  BYTE 0 = FF IF NOT FOUND                     
*                                                                               
STAGET   NTR1  BASE=BASERB                                                      
         L     R9,BASER9                                                        
         LM    R2,R3,0(R1)                                                      
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         LA    R8,TRDEMOB                                                       
         USING DEMOD,R8                                                         
         ST    R1,SGPLIST                                                       
         STM   R2,R3,SGPARMS                                                    
         MVC   SGSVIO,AIOAREA      SAVE VALUE OF I/O AREA POINTER               
         SPACE 1                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         ST    R3,DBAREC           USE P2 TO SET I/O AREA                       
         MVI   DBFUNCT,DBVLST      VALIDATE STATION CALL LETTERS                
         ST    R7,DBCOMFCS                                                      
         MVC   DBSELSTA,0(R2)      P1 POINTS TO STATION/MEDIA                   
         MVC   DBSELMED,5(R2)                                                   
         MVI   DBSELSRC,C'A'                                                    
         SPACE 1                                                                
STAGET1  DS    0H                                                               
         GOTO1 CDEMAND,DMCB,DBLOCK,0                                            
         CLI   DBERROR,0                                                        
         BNE   STAGET2             DID NOT FIND STATION RECORD                  
         MVC   AIOAREA,SGSVIO      RESTORE I/O POINTER                          
         L     R1,SGPLIST                                                       
         STM   R2,R3,0(R1)         RESTORE PARAM LIST                           
         B     EXXMOD                                                           
         SPACE 1                                                                
STAGET2  DS    0H                                                               
         CLI   DBSELSRC,C'A'                                                    
         BNE   *+12                                                             
         MVI   DBSELSRC,C'N'       LOOK FOR NSI                                 
         B     STAGET1                                                          
         SPACE 1                                                                
         CLI   DBSELSRC,C'N'                                                    
         BNE   *+12                                                             
         MVI   DBSELSRC,C'S'       LOOK FOR SRC                                 
         B     STAGET1                                                          
         SPACE 1                                                                
STAGET3  DS    0H                                                               
         XC    KEY,KEY             READ REP FILE FOR STATION                    
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),0(R2)     CALL LETTERS                                 
         CLI   KEY+26,C'T'                                                      
         BNE   *+8                                                              
         MVI   KEY+26,C' '                                                      
         SPACE 1                                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE     NOT A REP STATION                            
         BNE   STAGETR                                                          
         SPACE 1                                                                
STAGET4  DS    0H                                                               
         ST    R3,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         MVC   AIOAREA,SGSVIO      RESTORE I/O POINTER                          
         GOTO1 VGETEL,DMCB,(X'02',(R3)),DMCB+8                                  
         CLI   DMCB,X'FF'                                                       
         BE    STAGETR                                                          
         SPACE 1                                                                
         L     R6,DMCB+8           COMPETING ELEMENT                            
STAGET4A DS    0H                                                               
         MVI   DBSELSRC,C'A'       READ FOR A COMPETING STATION                 
         MVC   DBSELSTA,2(R6)      TO GET AT THE MARKET                         
         CLI   DBSELSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         SPACE 1                                                                
STAGET5  DS    0H                                                               
         GOTO1 CDEMAND,DMCB,DBLOCK,0                                            
         CLI   DBERROR,0                                                        
         BNE   STAGET6             DID NOT FIND STATION                         
         L     R1,SGPLIST                                                       
         STM   R2,R3,0(R1)                                                      
         B     EXXMOD                                                           
         SPACE 1                                                                
STAGET6  DS    0H                                                               
         CLI   DBSELSRC,C'A'                                                    
         BNE   STAGET6A                                                         
         MVI   DBSELSRC,C'N'       LOOK FOR NSI                                 
         B     STAGET5                                                          
STAGET6A DS    0H                                                               
         CLI   DBSELSRC,C'N'                                                    
         BNE   STAGET7                                                          
         MVI   DBSELSRC,C'S'       LOOK FOR SRC                                 
         B     STAGET5                                                          
STAGET7  DS    0H                                                               
         ZIC   RF,1(R6)            LOOK FOR ANOTHER STATION                     
         AR    R6,RF                                                            
         CLI   0(R6),X'02'                                                      
         BE    STAGET4A                                                         
         SPACE 1                                                                
STAGETR  DS    0H                                                               
         L     R1,SGPLIST                                                       
         STM   R2,R3,0(R1)                                                      
         MVI   4(R1),X'FF'                                                      
         B     EXXMOD                                                           
         DROP  R7,R8                                                            
         EJECT                                                                  
* VALIDATE FIELD IS ALPHA/NUMERIC                                               
* FIELD IS DELIMITED BY BLANK,COMMA,OR DASH                                     
* R4 HAS FIELD ADDRESS. ON EXIT HAS STOP CHAR ADDRESS                           
* R5 HAS MAX LENGTH.    ON EXIT HAS CHAR COUNT.                                 
* R1 HAS 'TO' ADDRESS.  ON EXIT HAS NEXT CHAR ADDRESS                           
*                                                                               
TESTAN   MVI   BYTE,X'0C'          SET VALID A (X'04') AND N (X'08')            
         LA    R0,1(R5)            GET MAX LEN+1                                
TESTAN1  CLI   0(R4),C' '                                                       
         BE    TESTANX                                                          
         CLI   0(R4),0                                                          
         BE    TESTANX                                                          
         CLI   0(R4),C','                                                       
         BE    TESTANX                                                          
         CLI   0(R4),C'-'                                                       
         BE    TESTANX                                                          
         CLI   0(R4),C'A'                                                       
         BL    TESTAN2                                                          
         CLI   0(R4),C'Z'                                                       
         BNH   TESTAN4                                                          
TESTAN2  NI    BYTE,X'08'          FIELD NOT ALPHA                              
         CLI   0(R4),C'0'                                                       
         BL    TESTAN4                                                          
         CLI   0(R4),C'9'                                                       
         BNH   TESTAN6                                                          
TESTAN4  NI    BYTE,X'04'          FIELD NOT NUMERIC                            
TESTAN6  MVC   0(1,R1),0(R4)                                                    
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,TESTAN1                                                       
         B     ERRORALT                                                         
TESTANX  BCTR  R0,0                ADJUST COUNT                                 
         SR    R5,R0               GIVES CHARACTER COUNT                        
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE REGENINT                                                       
         EJECT                                                                  
*                                                                               
*  ALTERNATE EXITS SET UP FOR 'BUDGET' ACTION RESET                             
*                                                                               
ERRORALT EQU   *                                                                
         CLC   BUDACT(3),=C'BUD'                                                
         BNE   ERROR                                                            
         XC    BUDACT(3),BUDACT    BLANK OUT FIELD                              
         MVI   BACT,C'B'                                                        
         B     ERROR                                                            
         SPACE 3                                                                
EXITALT  EQU   *                                                                
         CLC   BUDACT(3),=C'BUD'                                                
         BNE   EXIT                                                             
         XC    BUDACT(3),BUDACT    BLANK OUT FIELD                              
         MVI   BACT,C'B'                                                        
         B     EXIT                                                             
         SPACE 3                                                                
EXMODALT EQU   *                                                                
         CLC   BUDACT(3),=C'BUD'                                                
         BNE   EXXMOD                                                           
         XC    BUDACT(3),BUDACT    BLANK OUT FIELD                              
         MVI   BACT,C'B'                                                        
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*        LOACAL RECUP                                                           
*                                                                               
MYRECUP  NTR1                                                                   
*                                                                               
*- FIND MYSELF IN CORE                                                          
         BASR  R7,0                                                             
         USING MYRCUP10,R7                                                      
MYRCUP10 EQU   *                                                                
*                                                                               
*- BACK UP REGISTER STACK UNTIL I FIND A CONTRACT PHASE                         
         LR    R2,RD                                                            
MYRCUP20 L     R2,4(R2)            BACK POINTER                                 
         CLI   0(R2),C'0'                                                       
         BNE   MYRCUP20                                                         
         CLI   1(R2),C'4'          FILE PROGRAM = C'04'                         
         BNE   MYRCUP20                                                         
*                                                                               
         DROP  R7                                                               
         L     RC,68(R2)           RESTORE RC                                   
         L     RA,60(R2)           RESTORE RA                                   
         L     R9,BASER9           RESTORE R9                                   
         L     RB,BASERB           RESTORE RB                                   
*                                                                               
         MVI   8(R1),C'R'                                                       
         GOTO1 LOCALUP                                                          
         CLI   8(R1),0                                                          
         BNE   EXXMOD                                                           
         USING T804FFD,RA                                                       
         MVC   LFMMSG(34),=C'RECORD FULL - CHANGE NOT PROCESSED'                
         OI    LFMMSGH+6,X'80'                                                  
         DC    H'0',C'$ABEND'      UNWIND TRANSACTION                           
         DS    0F                                                               
LOCALUP  DS    A                                                                
*                                                                               
       ++INCLUDE RGENEROL                                                       
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
       ++INCLUDE RELFMINC                                                       
*                                                                               
SWIEXIST DC    C'* ERROR * SWITCH RECORD EXISTS FOR STATION'                    
*                                                                               
         EJECT                                                                  
*        RECLIST ENTRIES ARE      0-7   REC NAME                                
*                                 8     REC CODE                                
*                                 9     VALID ACTIONS                           
*                                       X'80' ADD                               
*                                       X'40' CHANGE                            
*                                       X'20' DISPLAY                           
*                                       X'08' TRANSFER                          
*                                       X'04' DELETE                            
*                                       X'02' BUDGET (SPECIAL)                  
*                                       X'01' BUDGET (SPECIAL)                  
*                                 10    OVERLAY NUMBER                          
*                                 11    SCREEN NUMBER                           
*                                 12    KEY EDIT ROUTINE INDEX                  
         CNOP  2,4                                                              
RECLIST  DC    H'13'               <-- ENTRY LENGTH                             
         DC    A(RECLISTX-1)                                                    
         SPACE 1                                                                
         DC    CL8'REPREC  ',X'01',X'E0',X'08',X'FE',AL1(NDXREP)                
         DC    CL8'STATION ',X'02',X'E0',X'14',X'FD',AL1(NDXSTA)                
         DC    CL8'COMPETIN',X'02',X'60',X'14',X'E9',AL1(NDXSTA)                
         DC    CL8'REGION  ',X'03',X'E0',X'01',X'FC',AL1(NDXRGN)                
         DC    CL8'OFFICE  ',X'04',X'E0',X'02',X'FB',AL1(NDXOFC)                
         DC    CL8'TEAM    ',X'05',X'E0',X'02',X'FA',AL1(NDXTM)                 
         DC    CL8'OFFTEAM ',X'02',X'60',X'14',X'DC',AL1(NDXSTA)                
         DC    CL8'SALESMAN',X'06',X'E0',X'02',X'F9',AL1(NDXSL)                 
         DC    CL8'GROUP   ',X'07',X'E0',X'02',X'F2',AL1(NDXGRP)                
         DC    CL8'ADVERTSR',X'08',X'E0',X'03',X'F7',AL1(NDXADV)                
         DC    CL8'PRODUCT ',X'09',X'E0',X'08',X'F6',AL1(NDXPRD)                
         DC    CL8'AGENCY  ',X'0A',X'E0',X'03',X'F5',AL1(NDXAGY)                
         DC    CL8'AGY     ',X'0A',X'E0',X'03',X'F5',AL1(NDXAGY)                
         DC    CL8'CLASS   ',X'0D',X'E0',X'04',X'F1',AL1(NDXCLS)                
         DC    CL8'CATEGORY',X'0F',X'E0',X'04',X'F0',AL1(NDXCTG)                
         DC    CL8'INVENTOR',X'12',X'E4',X'05',X'EF',AL1(NDXINV)                
         DC    CL8'INVENTOR',X'12',X'08',X'07',X'ED',AL1(NDXINV)                
         DC    CL8'DEMOS   ',X'12',X'60',X'06',X'EE',AL1(NDXINV)                
         DC    CL8'NDEM    ',X'12',X'60',X'20',X'EE',AL1(NDXINV)                
         DC    CL8'ESTIMATE',X'12',X'64',X'0B',X'E8',AL1(NDXINV)                
         DC    CL8'BUDGET  ',X'13',X'E2',X'13',X'E7',AL1(NDXBUD)                
         DC    CL8'BUDGET  ',X'13',X'01',X'13',X'F3',AL1(NDXBUD)                
         DC    CL8'OBUDGET ',X'19',X'E0',X'19',X'F4',AL1(NDXOBUD)               
         DC    CL8'FIX     ',X'8C',X'60',X'13',X'E1',AL1(NDXFIX)                
         DC    CL8'EOM     ',X'18',X'E0',X'13',X'E6',AL1(NDXEOM)                
         DC    CL8'DMENU   ',X'23',X'E4',X'13',X'EA',AL1(NDXDMEN)               
         DC    CL8'OWNER   ',X'2A',X'E0',X'01',X'E5',AL1(NDXOWN)                
*                                                                               
         DC    CL8'COMMENT ',X'2E',X'E4',X'01',X'E4',AL1(NDXCMT)                
         DC    CL8'CMT     ',X'2E',X'E4',X'01',X'E4',AL1(NDXCMT)                
*                                                                               
         DC    CL8'PROFILE ',X'01',X'60',X'01',X'E0',AL1(NDXPROF)               
*                                                                               
         DC    CL8'POINTPER',X'31',X'E0',X'01',X'E3',AL1(NDXPTP)                
*                                                                               
         DC    CL8'DEVSALES',X'3A',X'E0',X'02',X'DD',AL1(NDXSL)                 
*                                                                               
         DC    CL8'DEVTYPE ',X'3B',X'E0',X'01',X'DE',AL1(NDXDCT)                
*                                                                               
         DC    CL8'CONTYPE ',X'32',X'E0',X'01',X'DF',AL1(NDXCTY)                
*                                                                               
RECLISTX EQU   *                                                                
         SPACE 2                                                                
*        ACTLIST ENTRIES                                                        
*                                 0-7   ACTION                                  
*                                 8     ACTION CODE                             
*                                 9     ACTION BIT                              
         CNOP  2,4                                                              
ACTLIST  DC    H'10'                                                            
         DC    A(ACTLISTX-1)                                                    
         SPACE 1                                                                
         DC    CL8'ADD     ',C'A',X'80'                                         
         DC    CL8'CHANGE  ',C'C',X'40'                                         
         DC    CL8'DISPLAY ',C'D',X'20'                                         
         DC    CL8'TRANSFER',C'T',X'08'                                         
         DC    CL8'DELETE  ',C'X',X'04'                                         
         DC    CL8'BUDGET  ',C'B',X'02'                                         
         DC    CL8'ALLOCATE',C'L',X'01'                                         
ACTLISTX EQU   *                                                                
         SPACE 2                                                                
* TABLE OF KEY ROUTINE INDICES AND THEIR BRANCH DISPLACEMENTS                   
*                                                                               
INDLIST  DS    0CL4                                                             
         DC    AL1(NDXREP),AL3(REPKEY-T80400)                                   
         DC    AL1(NDXSTA),AL3(STAKEY-T80400)                                   
         DC    AL1(NDXRGN),AL3(RGNKEY-T80400)                                   
         DC    AL1(NDXOFC),AL3(OFCKEY-T80400)                                   
         DC    AL1(NDXTM),AL3(TMKEY-T80400)                                     
         DC    AL1(NDXSL),AL3(SLKEY-T80400)                                     
         DC    AL1(NDXGRP),AL3(GRPKEY-T80400)                                   
         DC    AL1(NDXADV),AL3(ADVKEY-T80400)                                   
         DC    AL1(NDXPRD),AL3(PRDKEY-T80400)                                   
         DC    AL1(NDXAGY),AL3(AGYKEY-T80400)                                   
         DC    AL1(NDXCLS),AL3(CLSKEY-T80400)                                   
         DC    AL1(NDXCTG),AL3(CTGKEY-T80400)                                   
         DC    AL1(NDXINV),AL3(INVKEY-T80400)                                   
         DC    AL1(NDXBUD),AL3(BUDKEY-T80400)                                   
         DC    AL1(NDXOBUD),AL3(OBUDKEY-T80400)                                 
         DC    AL1(NDXFIX),AL3(FIXKEY-T80400)                                   
         DC    AL1(NDXEOM),AL3(EOMKEY-T80400)                                   
         DC    AL1(NDXDMEN),AL3(DMENKEY-T80400)                                 
         DC    AL1(NDXOWN),AL3(OWNKEY-T80400)                                   
         DC    AL1(NDXCMT),AL3(CMTKEY-T80400)                                   
         DC    AL1(NDXPROF),AL3(PROFKEY-T80400)                                 
         DC    AL1(NDXPTP),AL3(PTPKEY-T80400)                                   
         DC    AL1(NDXCTY),AL3(CTYKEY-T80400)                                   
         DC    AL1(NDXDCT),AL3(DCTKEY-T80400)                                   
INDICES  EQU   (*-INDLIST)/L'INDLIST                                            
         EJECT                                                                  
MONTHS   DC    X'01',CL3'JAN'                                                   
         DC    X'02',CL3'FEB'                                                   
         DC    X'03',CL3'MAR'                                                   
         DC    X'04',CL3'APR'                                                   
         DC    X'05',CL3'MAY'                                                   
         DC    X'06',CL3'JUN'                                                   
         DC    X'07',CL3'JUL'                                                   
         DC    X'08',CL3'AUG'                                                   
         DC    X'09',CL3'SEP'                                                   
         DC    X'10',CL3'OCT'                                                   
         DC    X'11',CL3'NOV'                                                   
         DC    X'12',CL3'DEC'                                                   
         DC    X'01',CL3'JAN'                                                   
         DC    X'02',CL3'FEB'                                                   
         DC    X'03',CL3'MAR'                                                   
         DC    X'04',CL3'APR'                                                   
         DC    X'05',CL3'MAY'                                                   
         DC    X'06',CL3'JUN'                                                   
         DC    X'07',CL3'JUL'                                                   
         DC    X'08',CL3'AUG'                                                   
         DC    X'09',CL3'SEP'                                                   
         DC    X'10',CL3'OCT'                                                   
         DC    X'11',CL3'NOV'                                                   
         DC    X'12',CL3'DEC'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
*                                                                               
* CKSWI: CHECK IF A SWITCH REC WITH THIS STATION AS DEST STATION                
*        ALREADY EXISTS.  IF SO, EXIT WITH ERROR MESSAGE                        
*                                                                               
CKSWI    NMOD1 0,**CKSWI*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   NEWCODE,LFMKEY                                                   
         OC    NEWCODE,SPACES                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'28'                                                        
         MVC   KEY+13(2),REPALPHA                                               
         BAS   RE,HIGH                                                          
CK10     CLC   KEY(15),KEYSAVE                                                  
         BNE   CKX                                                              
         TM    KEY+18,X'02'                                                     
         BZ    CK20                                                             
         BAS   RE,GETREC                                                        
         GOTO1 VGETEL,DMCB,(X'01',AIOAREA),DMCB+8                               
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         L     R5,DMCB+8                                                        
         USING RSWIELEM,R5                                                      
         CLC   NEWCODE,RSWINEW                                                  
         BE    CKERR                                                            
CK20     MVC   KEY,KEYSAVE                                                      
         BAS   RE,SEQ                                                           
         B     CK10                                                             
         DROP  R5                                                               
*                                                                               
CKERR    LTR   RB,RB               SET CC <> 0 FOR ERROR EXIT                   
         B     *+6                                                              
CKX      SR    R3,R3               SET CC = 0 FOR NORMAL EXIT                   
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
NEWCODE  DS    CL5                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*- GETREP -- 1ST TIME IN ONLY, SET UP REP & MASTER REP SAVE FIELDS              
*                                                                               
*  INPUT: P1 = A(WORK AREA)  (RC)                                               
GETREP   NMOD1 0,T80400                                                         
         L     RC,0(R1)            ESTABLISH WORK AREA ADDRESSABILITY           
*                                                                               
         CLI   BACT,0              0 ON 1ST TIME.                               
         BNZ   GREP020                                                          
         XC    SVREP,SVREP         START CLEAN                                  
*                                                                               
GREP020  CLC   SVREPREP,REPALPHA                                                
         BE    GREPEXT             ALREADY HAVE REP INFO.                       
*                                                                               
*- ATTEMPT TO READ IN REP RECORD. (MAY NOT BE THERE IF ADDING REP)              
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           ID                                           
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,GRHIGH                                                        
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GREPEXT             REP INFO NOT AVAILABLE                       
*                                                                               
         BAS   RE,GRGETREC           READ IN RECORD                             
*                                                                               
*- SAVE CURRENT REP FIELDS                                                      
         MVC   SVREPREP,RREPKREP   REP CODE                                     
         MVC   SVREPDA,KEY+30      DISK ADDRESS                                 
         MVC   SVREPMST,RREPMAST   MASTER REP CODE                              
         CLC   SVREPMST,GRSPACE                                                 
         BNE   GREP030                                                          
         MVC   SVREPMST,GRZERO     TREAT BLANK AS 0                             
*                                                                               
*- IF THIS REP IS A SUBSIDIARY REP, READ IN MASTER & SAVE ACCESS INFO           
*  IF THIS REP IS THE MASTER, WE ALREADY HAVE RECORD READ IN.                   
GREP030  CLC   SVREPMST,GRZERO                                                  
         BE    GREPEXT             NOT MASTER OR SUBSID                         
         CLC   SVREPMST,GRFF                                                    
         BE    GREP035             THIS IS THE MASTER                           
*                                                                               
         MVC   KEY+25(2),SVREPMST  MASTER CODE TO KEY                           
         BAS   RE,GRHIGH                                                        
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GREPEXT             MASTER NOT FOUND?                            
         BAS   RE,GRGETREC                                                      
*                                                                               
*- SAVE MASTER REP INFO                                                         
*        X'02' SUBSIDIARY REP ELEMENT (IN SVREPSUB)                             
*        X'03' MASTER ACCESS PROFILES (IN SVREPMAC)                             
*                                                                               
GREP035  EQU   *                                                                
         XC    SVREPSUB,SVREPSUB                                                
         XC    SVREPMAC,SVREPMAC                                                
*                                                                               
         MVC   SVREPMDA,KEY+30     MASTER REC DISK ADDRESS                      
         LA    RF,RREPELEM                                                      
GREP040  EQU   *                                                                
         ZIC   RE,1(RF)            ELEMENT LENGTH TO MOVE                       
*                                                                               
         LA    R1,SVREPSUB         SUBSIDIARY REP LIST                          
         CLI   0(RF),X'02'                                                      
         BE    GREP060                                                          
*                                                                               
         LA    R1,SVREPMAC         MASTER ACCESS PROFILES                       
         CLI   0(RF),X'03'                                                      
         BE    GREP060                                                          
*                                                                               
         CLI   0(RF),X'05'         SPOTPAK ENCODING                             
         BNE   GREP070                                                          
         MVC   SVREPSPW(2),2(RF)   LOAD POWER CODE                              
         MVC   SVREPSSY(1),3(RF)   LOAD SPOTPAK SYSTEM CODE                     
         B     GREP080                                                          
*                                                                               
GREP060  BCTR  RE,0                MOVE ELEMENT TO SAVE AREA                    
         EX    RE,GREP065                                                       
         B     GREP070                                                          
*                                                                               
GREP065  MVC   0(0,R1),0(RF)       * EXECUTED MOVE *                            
*                                                                               
GREP070  EQU   *                                                                
         ZIC   RE,1(RF)            GET LENGTH OF ELEMENT AGAIN                  
         AR    RF,RE               NEXT ELEMENT                                 
         CLI   0(RF),0                                                          
         BNE   GREP040             LOOP BACK IF NOT END OF RECORD               
*                                                                               
GREP080  EQU   *                                                                
*                                                                               
GREPEXT  XIT1                      ALL DONE.                                    
         SPACE 2                                                                
GRSPACE  DC    CL10' '                                                          
GRZERO   DC    XL10'00'                                                         
GRFF     DC    X'FFFFFFFF'                                                      
CDMRDHI  DC    C'DMRDHI'                                                        
CGETREC  DC    C'GETREC'                                                        
REPDIR   DC    C'REPDIR'                                                        
REPFILE  DC    C'REPFILE'                                                       
         DS    0H                  ALIGNMENT                                    
         SPACE 2                                                                
*                                                                               
*- GRHIGH -- READ DIRECTORY                                                     
GRHIGH   LR    R5,RE                                                            
         MVC   COMMAND,CDMRDHI                                                  
         IC    R4,DMINBTS                                                       
         IC    R3,TERMNAL                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),REPDIR,KEYSAVE,KEY         X        
               ((R3),GRZERO(4)),0                                               
         B     GRDMCHEK                                                         
         SPACE 2                                                                
*                                                                               
*- GRGETREC -- READ FILE                                                        
GRGETREC LR    R5,RE                                                            
         MVC   COMMAND,CGETREC                                                  
         IC    R4,DMINBTS                                                       
         IC    R3,TERMNAL                                                       
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),REPFILE,                   X        
               KEY+28,RREPREC,((R3),DMWORK),0                                   
*                                                                               
GRDMCHEK MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZR   R5                                                               
         DC    H'0'                DATA MANAGER ERROR                           
         EJECT                                                                  
*                                                                               
*- MASTACC -- DETERMINE IF SUBSIDIARY REP HAS ACCESS TO MASTER RECORDS          
*                                                                               
*  RETURN CODE: 0 = ACCESS, NON-0 = ACCESS DENIED.                              
*                                                                               
MASTACC  NMOD1 0,T80400                                                         
         L     RC,0(R1)                                                         
*                                                                               
         CLC   SVREPMST,MAZERO                                                  
         BE    MACCACC             ACCESS                                       
         CLC   SVREPMST,MAFF                                                    
         BE    MACCACC             MASTER ALWAYS HAS ACCESS                     
*                                                                               
*- RECORD IN LIST OF MASTER RECORDS?                                            
         BAS   RE,MACC020 X'ID',AL1(ACCESS DISP)                                
         DC    X'0A',AL1(RREPAAGY) AGENCY                                       
         DC    X'08',AL1(RREPAADV) ADVERTISER                                   
         DC    X'09',AL1(RREPAPRD) PRODUCT                                      
         DC    X'0D',AL1(RREPACLS) CLASS                                        
         DC    X'0F',AL1(RREPACTG) CATAGORY                                     
         DC    X'31',AL1(RREPAPNT) POINT PERSON                                 
         DC    X'07',AL1(RREPAGRP) GROUP                                        
         DC    X'05',AL1(RREPATEM) TEAM (DIVISION)                              
         DC    X'2A',AL1(RREPAOWN) OWNER                                        
         DC    X'3A',AL1(RREPADSL) DEVELOPMENT SALESPERSON                      
         DC    X'3B',AL1(RREPADTY) DEVELOPMENT CONTRACT TYPE                    
         DC    H'0'                EOT                                          
*                                                                               
MACC020  CLI   0(RE),0                                                          
         BE    MACCACC             REC NOT IN LIST. ACCESS                      
         CLC   BREC,0(RE)                                                       
         BE    MACC040             REC FOUND IN LIST...CHECK ACCESS             
         LA    RE,2(RE)            NEXT ENTRY                                   
         B     MACC020                                                          
*                                                                               
*- FIND ACCESS PROFILE                                                          
MACC040  ZIC   RF,1(RE)            DISPLACEMENT INTO ELEMENT                    
         LA    RE,SVREPMAC         ACCESS ELEMENT                               
         AR    RE,RF                                                            
         CLI   0(RE),C'Y'          FULL ACCESS?                                 
         BE    MACCACC                                                          
         LA    R3,NOACCMSG                                                      
         CLI   0(RE),C'N'          NO ACCESS?                                   
         BE    MACCNO                                                           
*                                                                               
*- ONLY THING LEFT IS DISPLAY ONLY.                                             
*  GRANT ACCESS IF ACTION IS DISP, DENY OTHERWISE                               
         LA    R3,DISONLY                                                       
         CLI   BACT,C'D'                                                        
         BE    MACCACC             ACCESS                                       
*                                                                               
MACCNO   ST    R3,DMCB             PASS BACK MSG IN P1                          
         LTR   R3,R3               SET NON-0 CC                                 
         B     MACCEXT                                                          
MACCACC  CR    R0,R0               SET 0 CC (ACCESS)                            
MACCEXT  XIT1                                                                   
         SPACE 2                                                                
MAZERO   DC    X'0000'                                                          
MAFF     DC    X'FFFF'                                                          
         EJECT                                                                  
*  CHECKS TO MAKE SURE STATION RECORD EXISTS FOR                                
*  THE INVENTORY YOU ARE TRYING TO ATTACH TO                                    
*  RETURN CODE: 0 = STATION RECORD EXISTS, NON-0 = DOESN'T EXIST.               
*                                                                               
FINDSTAT NMOD1 0,**FSTATN                                                       
         L     RC,0(R1)                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),WORK                                                   
         CLI   KEY+26,C'T'        TELEVISON BLANK IT OUT                        
         BNE   FINDS50                                                          
         MVI   KEY+26,C' '                                                      
         B     FINDS70                                                          
FINDS50  CLI   KEY+26,X'F0'                                                     
         BL    FINDS70                                                          
         CLI   KEY+26,X'F9'                                                     
         BH    FINDS70                                                          
* IT IS NUMERIC -THEREFORE STATILITE STATION -BLANK IT OUT                      
         MVI   KEY+26,C' '                                                      
FINDS70  GOTO1 HIGH                  ! CAN'T BE GRHIGH !                        
         CLC   KEY(27),KEYSAVE                                                  
         BE    FXGOOD                                                           
*                                                                               
FXBAD    LTR   RB,RB                                                            
         B     FXIT                                                             
*                                                                               
FXGOOD   SR    R0,R0                                                            
*                                                                               
FXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DS    0H                                                               
OFFBUDKY NMOD1 0,*OBUD*                                                         
         L     RC,0(R1)            RESET A(WORK SPACE)                          
         MVC   BKEY+17(2),REPALPHA        REP                                   
         SPACE 1                                                                
         LA    R1,BKEY+19          EDIT YEAR                                    
         LA    R3,2                SET EDIT ERROR TYPE                          
         LA    R4,8(R2)                                                         
         LA    R5,2                                                             
         BAS   RE,TESTAN                                                        
         TM    BYTE,X'08'          MUST BE NUMERIC                              
         BZ    OFBD0070                                                         
         CH    R5,=H'2'            MUST BE 2 CHARACTERS                         
         BNE   OFBD0070                                                         
         LA    R5,1(R5)            ADD 1 FOR STOP CHARACTER                     
         STH   R5,HALF             SAVE LENGTH + STOP CHARACTER                 
         OI    0(R4),X'40'         'OR' IN A SPACE                              
         CLI   0(R4),C' '          CHECK FOR STOP CHARACTER (SPACE)             
         BNH   OFBD0070            SPACE: KEY INCOMPLETE - ERROR                
*  EDIT OFFICE                                                                  
         LA    R3,156              INVALID OFFICE                               
         LA    R1,BKEY+21                                                       
         OC    BKEY+21(2),SPACES                                                
         LA    R4,1(R4)            BUMP TO NEXT SUBFIELD OF KEY                 
         LA    R5,2                OFFICE HAS 2 CHARACTERS MAX                  
         BAS   RE,TESTAN                                                        
         SPACE 1                                                                
*  VERIFY OFFICE ON FILE                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,4               CHECK OFFICE                                 
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),BKEY+21                                                
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   OFBD0070            OFFICE INVALID - ERROR                       
         BAS   RE,LOADTABL         LOAD OPTION TEST TABLE                       
*                                    **SEE NOTE AT THAT ROUTINE                 
         AH    R5,HALF             INCREMENT TOTAL LENGTH OF DATA               
         STC   R5,BYTE                                                          
         STH   R5,HALF             STORE IT BACK                                
         BAS   RE,CHEKTEAM         CHECK TEAM PRESENCE/ABSENCE                  
         BNZ   OFBD0060            ERROR: TEAM NEEDED/MISSING,                  
*                                     DEPENDING ON OFFICE                       
         OI    0(R4),X'40'         'OR' IN A SPACE                              
         CLI   0(R4),C' '          CHECK FOR STOP CHARACTER (SPACE)             
         BE    OFBD0020            NO STOP CHARACTER: NO TEAM                   
*                                  STOP CHARACTER:  VALIDATE TEAM               
         LA    R5,1                ADD 1 FOR STOP CHARACTER                     
         AH    R5,HALF                                                          
         STC   R5,BYTE                                                          
         STH   R5,HALF             STORE IT BACK                                
         LA    R1,BKEY+23          A(TEAM FIELD IN KEY)                         
         OC    BKEY+23(2),SPACES                                                
         LA    R4,1(R4)            BUMP TO NEXT SUBFIELD OF KEY                 
         LA    R5,2                TEAM HAS 2 CHARACTERS MAX                    
         BAS   RE,TESTAN                                                        
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,5               CHECK TEAM                                   
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),BKEY+23                                                
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   OFBD0070            TEAM NOT VALID                               
         SPACE 1                                                                
         AH    R5,HALF                                                          
         STC   R5,BYTE                                                          
         STH   R5,HALF             ALSO STORE HALFWORD                          
         SPACE 1                                                                
         BAS   RE,CHEKSGRP         CHECK SUBGROUP PRESENCE/ABSENCE              
         BNZ   OFBD0060            ERROR: SUBGROUP NEEDED/MISSING,              
*                                     DEPENDING ON OFFICE                       
         OI    0(R4),X'40'         'OR' IN A SPACE                              
         CLI   0(R4),C' '          CHECK FOR STOP CHARACTER (SPACE)             
         BE    OFBD0020            NO STOP CHARACTER: NO SUBGROUP               
*                                  STOP CHARACTER:  VALIDATE SUBGRP             
         LA    R5,1                ADD 1 FOR STOP CHARACTER                     
         AH    R5,HALF                                                          
         STC   R5,BYTE                                                          
         STH   R5,HALF             STORE IT BACK                                
         LA    R1,BKEY+25          A(SUBGROUP FIELD IN KEY)                     
         OC    BKEY+25(2),SPACES                                                
         LA    R4,1(R4)            BUMP TO NEXT SUBFIELD OF KEY                 
         LA    R5,2                TEAM HAS 2 CHARACTERS MAX                    
         BAS   RE,TESTAN                                                        
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,7               CHECK SUBGROUP                               
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),BKEY+25   INSERT GROUP/SUBGROUP                        
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    OFBD0010                                                         
         MVC   LFMMSG(L'NOSUBGRP),NOSUBGRP                                      
*                                  INVALID SUBGROUP SUBMITTED                   
         FOUT  LFMMSGH                                                          
         LA    R2,LFMACTH          SET CURSOR POSITION                          
         B     OFBD0060            EXIT                                         
OFBD0010 EQU   *                                                                
         AH    R5,HALF                                                          
         STC   R5,BYTE                                                          
         STH   R5,HALF             ALSO STORE HALFWORD                          
         SPACE 1                                                                
OFBD0020 EQU   *                                                                
         CLC   BYTE,5(R2)          DO WE HAVE ALL THE INPUT                     
         BE    OFBD0030            YES - VALIDATION COMPLETED                   
         MVC   LFMMSG(L'TOOMUCH),TOOMUCH                                        
*                                  TO MANY FIELDS ON KEY                        
         FOUT  LFMMSGH                                                          
         LA    R2,LFMACTH          SET CURSOR POSITION                          
         B     OFBD0060            EXIT                                         
* GET FISCAL START MONTH FROM REP RECORD                                        
         SPACE 1                                                                
OFBD0030 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,1                                                            
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   OFBDDUMP                                                         
         BAS   RE,GETREC                                                        
         GOTO1 VGETEL,DMCB,(X'01',AIOAREA),DMCB+8                               
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
OFBDDUMP EQU   *                                                                
         DC    H'0'                                                             
         L     R5,DMCB+8                                                        
         USING RREPELEM,R5                                                      
         MVC   STARTMO,RREPFMON                                                 
         DROP  R5                                                               
         SR    R0,R0               SET CC = ZERO:  NO ERROR                     
         XC    FULL,FULL           WIPE OUT PASS-BACK CODE                      
OFBD0050 EQU   *                                                                
         LTR   R0,R0               SET CC                                       
         XIT1                                                                   
OFBD0060 EQU   *                                                                
         SR    R3,R3               ERROR RETURN: NO PASS-BACK CODE              
OFBD0070 EQU   *                                                                
         LA    R0,1                SET CC NOT = ZERO: ERROR                     
         ST    R3,FULL             SET PASS-BACK CODE                           
         B     OFBD0050            RETURN                                       
         SPACE 4                                                                
         EJECT                                                                  
*                                                                               
*  LOADTABL:  RETRIEVE OFFICE RECD/PART II FOR THIS REP/OFFICE.  CHECK          
*        OPTION BITS.  IF FIRST BIT IS OFF, BYPASS.  IF ON, INSERT              
*        INTO TABLE, AND THEN CHECK SECOND BIT.  IF ON, SET SUBGROUP            
*        NEEDED FLAG TO YES.                                                    
*                                                                               
*  NOTE:   THIS TABLE ORIGINALLY CONTAINED ALL THE OFFICE RECORDS FOR           
*       THE REP.  AS THIS WASN'T NEEDED, AND WAS ONLY WASTED I/O, THE           
*       RECORD FOR THE SPECIFIC OFFICE IS THE ONLY ONE INSERTED INTO            
*       THE TABLE.  THIS ALLOWED THE LOGIC TO REMAIN THE SAME.                  
*                                                                               
LOADTABL NTR1                                                                   
         LA    R6,TEAMTABL         A(TABLE)                                     
         XC    TEAMTABL,TEAMTABL   CLEAR THE TABLE                              
         XC    KEY,KEY                                                          
         MVI   KEY,X'44'           TYPE:  OFFICE/PART II.                       
         MVC   KEY+23(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+25(2),BKEY+21   INSERT OFFICE CODE                           
         GOTO1 HIGH                READ FIRST KEY                               
         CLC   KEYSAVE(27),KEY       RECORD FOUND?                              
         BNE   LTAB0040            NO  - FINISHED                               
         BAS   RE,GETREC           YES - RETRIEVE RECORD                        
         LA    R2,REC                                                           
         USING ROFF2REC,R2                                                      
         LA    R3,ROFF2PRF         A(PROFILE BITS)                              
         TM    0(R3),X'80'         IS 'TEAM' REQUIRED FOR REPORT?               
         BNO   LTAB0040            NO  - FINISHED                               
         MVC   0(2,R6),REPALPHA    YES - MAKE TABLE ENTRY                       
         MVC   2(2,R6),ROFF2OFF    INSERT THE OFFICE CODE                       
         MVI   4(R6),C'N'          SET 'SUBGROUP NEEDED' FLAG TO 'NO'           
         TM    0(R3),X'40'         IS SUBGROUP NEEDED?                          
         BNO   LTAB0040            NO  - FINISHED                               
         MVI   4(R6),C'Y'          SET 'SUBGROUP NEEDED' FLAG TO 'YES'          
LTAB0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  CHEKTEAM:  DEPENDING ON OFFICE, THE TEAM MAY BE REQUIRED.  CHECK             
*     FOR PRESENCE/ABSENCE VS OFFICE.                                           
*        R6  =  A(TEAM TABLE ENTRIES)                                           
*        R4  =  A(SPACER: ',')                                                  
*                                                                               
CHEKTEAM NTR1                                                                   
         MVI   NEEDSGRP,C'N'       SET 'NEED SUBGROUP' FLAG TO 'NO'             
         LA    R6,TEAMTABL         A(TEAM TABLE)                                
CHEK0010 EQU   *                                                                
         CLI   0(R6),0             END OF TABLE?                                
         BE    CHEK0040            YES - REP/OFFICE NOT FOUND IN TABLE          
         CLC   0(2,R6),REPALPHA    REP FOUND?                                   
         BE    CHEK0030            YES - CHECK FOR OFFICE                       
CHEK0020 EQU   *                                                                
         LA    R6,TABENTRY(R6)     NO  - BUMP TO NEXT ENTRY                     
         B     CHEK0010            GO BACK FOR NEXT                             
CHEK0030 EQU   *                                                                
         CLC   2(2,R6),BKEY+21     OFFICE FOUND?                                
         BNE   CHEK0020            NO  - BUMP TO NEXT ENTRY                     
         CLI   0(R4),C' '          YES - IS THERE ANY TEAM?                     
         BE    CHEK0050            NO  - ERROR: SHOULD BE                       
         CLI   0(R4),X'00'                                                      
         BE    CHEK0050                                                         
*                                  TEAM PRESENT: IS SUBGROUP NEEDED?            
         CLI   4(R6),C'Y'          IS SUBGROUP NEEDED?                          
         BNE   CHEK0080            NO  - LEAVE FLAG 'OFF'                       
         MVI   NEEDSGRP,C'Y'       YES - SET 'NEED SUBGROUP' FLAG 'ON'          
         B     CHEK0080                                                         
CHEK0040 EQU   *                                                                
         CLI   0(R4),C' '          IS THERE ANY TEAM?                           
         BE    CHEK0080            NO  - SHOULDN'T BE - OKAY                    
         CLI   0(R4),X'00'                                                      
         BE    CHEK0080                                                         
         B     CHEK0060            YES - ERROR: SHOULDN'T BE                    
CHEK0050 EQU   *                                                                
         MVC   LFMMSG(L'TMMISS),TMMISS                                          
*                                  SEND 'TEAM MISSING' MESSAGE                  
         B     CHEK0070                                                         
CHEK0060 EQU   *                                                                
         MVC   LFMMSG(L'TMPRES),TMPRES                                          
*                                  SEND 'TEAM SHOULDN'T BE ENTERED'             
CHEK0070 EQU   *                                                                
         FOUT  LFMMSGH                                                          
         LA    R0,1                SEND CC NOT = ZERO: ERROR                    
         B     CHEK0090                                                         
CHEK0080 EQU   *                                                                
         SR    R0,R0               SEND CC = ZERO:  OKAY                        
CHEK0090 EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
*                                                                               
TMMISS   DC    CL44'OFFICE INDICATED REQUIRES ENTRY OF TEAM CODE'               
TMPRES   DC    CL44'OFFICE INDICATED PROHIBITS TEAM CODE        '               
SGMISS   DC    CL48'OFFICE INDICATED REQUIRES ENTRY OF SUBGROUP CODE'           
SGPRES   DC    CL48'OFFICE INDICATED PROHIBITS SUBGROUP CODE        '           
TOOMUCH  DC    CL38'KEY CONTAINS TOO MANY FIELDS          '                     
NOSUBGRP DC    CL38'INVALID SUBGROUP SPECIFIED            '                     
*                                                                               
* TEAMTABL:  FIVE POSITIONS PER ENTRY:                                          
*     POS  1 - 2 :     REP CODE                                                 
*     POS  3 - 4 :     OFFICE CODE                                              
*     POS      5 :     SUBGROUP NEEDED Y OR N                                   
*                                                                               
*     IF REP/OFFICE CODE FOUND IN TABLE, REQUIRES TEAM CODE.  IF                
*       NOT, TEAM CODE PROHIBITED.                                              
*                                                                               
*     TABLE HAS SPACE FOR ONLY 2 FIVE-BYTE ENTRIES                              
*                                                                               
TEAMTABL DS    CL10                                                             
TABENTRY EQU   5                                                                
NEEDSGRP DS    CL1                 'NEED SUBGROUP' FLAG                         
         EJECT                                                                  
*                                                                               
*  CHEKSGRP:  DEPENDING ON OFFICE, THE TEAM MAY BE REQUIRED.  THE               
*     SUBGROUP MAY ALSO BE REQUIRED, ENTERED AS 'GROUP/SUBGROUP'.               
*     CHECK FOR PRESENCE/ABSENCE VS OFFICE, USING 'NEEDSGRP' VALUE              
*     AS INDICATOR.                                                             
*           R4  =  A(SPACER: ',')                                               
*                                                                               
CHEKSGRP NTR1                                                                   
         CLI   NEEDSGRP,C'Y'       IS SUBGROUP REQUIRED?                        
         BNE   CGRP0010            NO                                           
         CLI   0(R4),C' '          YES - IS THERE ANY SUBGROUP?                 
         BE    CGRP0050            NO  - ERROR: SHOULD BE                       
         CLI   0(R4),X'00'                                                      
         BE    CGRP0050                                                         
         B     CGRP0080                                                         
CGRP0010 EQU   *                                                                
         CLI   0(R4),C' '          IS THERE ANY SUBGROUP?                       
         BE    CGRP0080            NO  - SHOULDN'T BE - OKAY                    
         CLI   0(R4),X'00'                                                      
         BE    CGRP0080                                                         
         B     CGRP0060            YES - ERROR: SHOULDN'T BE                    
CGRP0050 EQU   *                                                                
         MVC   LFMMSG(L'SGMISS),SGMISS                                          
*                                  SEND 'SUBGROUP MISSING' MESSAGE              
         B     CGRP0070                                                         
CGRP0060 EQU   *                                                                
         MVC   LFMMSG(L'SGPRES),SGPRES                                          
*                                  SEND 'SUBGROUP SHOULDN'T BE ENTERED'         
CGRP0070 EQU   *                                                                
         FOUT  LFMMSGH                                                          
         LA    R0,1                SEND CC NOT = ZERO: ERROR                    
         B     CGRP0090                                                         
CGRP0080 EQU   *                                                                
         SR    R0,R0               SEND CC = ZERO:  OKAY                        
CGRP0090 EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*- BEGIN WORK AREA DSECT                                                        
*                                                                               
       ++INCLUDE REGENSWI                                                       
         EJECT                                                                  
       ++INCLUDE RGENOLD                                                        
         EJECT                                                                  
       ++INCLUDE RELFMWRK                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENREPA         NEW REP REC                                  
         ORG                                                                    
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENOFF2         OFFICE RECORD/PART II                        
         ORG                                                                    
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENCTY          CONTRACT TYPE RECORD                         
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE RELFMTWA                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFME7D                                                       
*                                                                               
*   BUDGET SCREEN WORK AREA                                                     
*                                                                               
         ORG   BUDWORK+128                                                      
BCONTYP  DS    CL1                                                              
BCONDESC DS    CL20                                                             
FOUNDTYP DS    CL1                                                              
NOERRMSG DS    CL1                                                              
BUDACT   DS    CL3                                                              
BUDSCRN  DS    CL1                                                              
SAVER4   DS    F                                                                
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMF4D                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMF3D                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMF5D                                                       
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMF7D                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMF9D                                                       
*                                                                               
*  BUDGET ALLOCATION SCREEN WORK AREA                                           
*                                                                               
         ORG   BDGWORK+128                                                      
SAVER6   DS    F                   SAVE AREA:  R6 FOR ALLOC SCREEN              
SAVER4A  DS    F                   SAVE AREA:  R4 FOR ALLOC SCREEN              
ACTYPE   DS    F                   A(NEXT CONTRACT TYPE)                        
CTYPES   DS    CL8                 SAVE AREA FOR CONTRACT TYPES                 
OFFALLOC DS    CL1                 OFFICE ALLOCATION FLAG                       
       EJECT                                                                    
       ++INCLUDE FATWA                                                          
         SPACE 2                                                                
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
       ++INCLUDE DDGLOBEQUS                                                     
         SPACE 2                                                                
* DEMOD DSECT INCLUDING DEDBLOCK                                                
         PRINT OFF                                                              
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
SGPLIST  DS    A                                                                
SGPARMS  DS    2F                                                               
SGSVIO   DS    A                                                                
MGPLIST  DS    A                                                                
MGPARMS  DS    2F                                                               
         PRINT ON                                                               
         SPACE 2                                                                
* DEMO FILE DSECTS                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* KEY ROUTINE INDEX EQUATES                                                     
*                                                                               
NDXREP   EQU   1                                                                
NDXSTA   EQU   2                                                                
NDXRGN   EQU   3                                                                
NDXOFC   EQU   4                                                                
NDXTM    EQU   5                                                                
NDXSL    EQU   6                                                                
NDXGRP   EQU   7                                                                
NDXADV   EQU   8                                                                
NDXPRD   EQU   9                                                                
NDXAGY   EQU   10                                                               
NDXCLS   EQU   11                                                               
NDXCTG   EQU   13                                                               
NDXINV   EQU   14                                                               
NDXOBUD  EQU   15                                                               
NDXBUD   EQU   16                                                               
NDXFIX   EQU   17                                                               
NDXEOM   EQU   18                                                               
NDXDMEN  EQU   19                                                               
NDXOWN   EQU   20                                                               
NDXCMT   EQU   21                                                               
NDXPROF  EQU   22                                                               
NDXPTP   EQU   23                  POINT PERSON                                 
NDXCTY   EQU   24                  CONTRACT TYPE                                
NDXDCT   EQU   25                  CONTRACT TYPE                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117RELFMQ    05/01/02'                                      
         END                                                                    
