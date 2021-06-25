*          DATA SET CTMAD08    AT LEVEL 041 AS OF 05/01/02                      
*PHASE TA0C08A,*                                                                
         TITLE 'TA0C08 - $MAD REP CONTRACT SUPPORT DATA HANDLER'                
**********************************************************************          
*   HISTORY OF CHANGES                                                          
**********************************************************************          
*   03/20/91   (BU ) --- ORIGINAL ENTRY                                         
**********************************************************************          
TA0C08   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C08,RA                                                      
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
*        INITIALIZE OVERLAY WIDE REGISTERS                                      
*                                                                               
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
*                                                                               
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* 'INIT' INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                        
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
         L     R1,AIO1             SET A(IO AREA)                               
         ST    R1,AIO                                                           
         GOTO1 SETSYS,DMCB,(3,=C'REP'),=CL8'REPDIR',=CL8'REPFILE'               
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'PROCSTRT' PROCESSES THE START MODE.  IT PROCESSES THE REQUEST FOR            
* ASSISTANCE OBJECT PASSED BY 'COPILOT', AND RETURNS FRAME OF AGENCY            
* AND/OR ADVERTISER CODES/EXPANSIONS.                                           
*                                                                               
PROCSTRT NTR1                                                                   
         BAS   RE,PROCRQST         PROCESS REQUEST                              
         BNE   XIT                                                              
*                                                                               
         BAS   RE,FILLFRM          FILL FIRST FRAME                             
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* 'PROCMID' PROCESSES MIDDLE MODE.  IT RETURNS THE NEXT FRAME                   
* AND SETS THE LAST FRAME FLAG IF IT REACHES THE END OF THE REPORT              
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
         BAS   RE,FILLFRM          FILL FRAME                                   
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* 'PROCEND' PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.                 
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'PROCRQST' INTERROGATES THE REQUEST, AND ESTABLISHES THE FILTERS              
*    TO PROCESS THE DETAILS                                                     
*                                                                               
PROCRQST NTR1                                                                   
*                                                                               
         GOTO1 GETITEM             GET FIRST ITEM - ENTIRE REQUEST              
         BNE   XIT                                                              
*                                                                               
         L     R1,TYPENUM          IS IT 101 TYPE REQUEST?                      
         LA    R2,ITRSFLT                                                       
         CR    R1,R2                                                            
*        BNE   ERRFILT             NO  - ERROR ON INPUT                         
*                                                                               
*   NO ERROR ROUTINE FOR INCORRECT TYPE                                         
*                                                                               
*                                                                               
         XC    INFOSTOR(INFOLEN),INFOSTOR                                       
*                                                                               
         L     R1,ADATA            A(DATA)                                      
         L     R2,DATALEN          L(DATA)                                      
         AR    R2,R1               A(EOR+1)                                     
         BCTR  R2,0                A(EOR)                                       
*                                                                               
         L     R1,ADATA            SET A(INPUT DATA)                            
         USING CT08IN,R1                                                        
VR0002   EQU   *                                                                
         CR    R1,R2               E-O-R?                                       
         BNL   VR0099              YES                                          
         CLI   AIREQCDE,C'1'       ADVERTISER?                                  
         BNE   VR0004              NO                                           
         MVC   ADVFILON,AIREQCDE   YES - SET ADVERTISERS NEEDED                 
         LA    R3,1                INSERT L(FIELD LENGTH FIELD)                 
         BCTR  R3,0                DECREMENT BY 1                               
         EX    R3,VR0081           MOVE FILTER BY LENGTH                        
         CVB   R3,WORKAREA         CONVERT LENGTH TO BINARY                     
         LTR   R3,R3                                                            
         BZ    VR0008              NO FILTER INPUT                              
         STC   R3,LADVFILT         SET L(FILTER)                                
         LA    R5,ADVFILT          SET A(FILTER STRING)                         
         BCTR  R3,0                DECREMENT BY 1                               
         EX    R3,VR0080           MOVE FILTER TO STORAGE                       
         B     VR0008                                                           
VR0004   EQU   *                                                                
         CLI   AIREQCDE,C'2'       AGENCY?                                      
         BNE   VR0008              NO                                           
         MVC   AGYFILON,AIREQCDE   YES - SET AGENCIES NEEDED                    
         LA    R3,1                INSERT L(FIELD LENGTH FIELD)                 
         BCTR  R3,0                DECREMENT BY 1                               
         EX    R3,VR0081           MOVE FILTER BY LENGTH                        
         CVB   R3,WORKAREA         CONVERT LENGTH TO BINARY                     
         LTR   R3,R3                                                            
         BZ    VR0008              NO FILTER INPUT                              
         STC   R3,LAGYFILT         SET L(FILTER)                                
         LA    R5,AGYFILT          SET A(FILTER STRING)                         
         BCTR  R3,0                DECREMENT BY 1                               
         EX    R3,VR0080           MOVE FILTER TO STORAGE                       
         B     VR0008                                                           
VR0008   EQU   *                                                                
*                                                                               
*   BUMP TO NEXT POSSIBLE ENTRY.  ADD 1 BYTE FOR L(CODE), 1 BYTE                
*     FOR L(FILTER STRING INDICATOR FIELD), + 1 BYTE FOR 'EX'                   
*     STATEMENT DECREMENT.  THEN ADD STRING LENGTH.                             
*                                                                               
         LA    R1,3(R1)            L(CODE)+L(FILT STRG)+(EX DECR)               
         AR    R1,R3               ADD STRING LENGTH                            
         B     VR0002              RESET A(DSECT)                               
*                                                                               
VR0080   MVC   0(0,R5),AIFILSTR               LOAD FILTER STRING                
VR0081   PACK  WORKAREA(8),AISTRLEN(0)        PACK FILTER LENGTH                
*                                                                               
VR0099   EQU   *                                                                
         SR    R3,R3               SET CONDITION CODE = ZERO                    
         B     XIT                                                              
*                                                                               
         DROP  R1                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* 'FILLFRM' FILLS THE FRAME.  THE FRAME IS COMPOSED OF A SINGLE                 
* OBJECT, WHICH IS COMPOSED OF ONE OR MORE VARIABLE LENGTH ENTRIES.             
* WHILE EACH ENTRY COULD HAVE BEEN AN OBJECT BY ITSELF, IT WAS DECIDED,         
* FOR SPACE AND PROCESSING REASONS, TO MANUALLY BLOCK/UNBLOCK THE               
* FRAME.   BILL UHR (MAR/91)                                                    
*                                                                               
FILLFRM  NTR1                                                                   
         LA    R1,BLDAREA          SET A(OUTPUT AREA)                           
         ST    R1,ABLDAREA         SAVE IT OFF                                  
         CLI   AGYFILON,0          ANY AGENCY RECORDS NEEDED?                   
         BE    FF0030              NO  -                                        
         MVI   COMMREC,X'8A'       FILTER FLAG: WHICH RECD ID                   
         MVC   COMMTYP,AGYFILON                 WHICH RECORD TYPE               
         XC    LCOMMFIL(LCOMMEQU),LCOMMFIL                                      
         XC    KEY,KEY             ESTABLISH AGENCY PASSIVE KEY                 
         MVI   KEY,X'8A'           KEY TYPE                                     
         ZIC   R1,LAGYFILT         ANY AGENCY FILTER?                           
         LTR   R1,R1                                                            
         BZ    FF0010              NO                                           
         LA    R2,AGYFILT          AGENCY FILTER STRING                         
         LA    R3,KEY+1            SET FILTER IN KEY                            
         BCTR  R1,0                DECREMENT BY 1                               
         EX    R1,FF0800                                                        
         LA    R3,COMMFILT         SET FILTER IN COMMON AREA                    
         EX    R1,FF0800                                                        
         LA    R1,1(R1)            INCREMENT BY 1                               
         STC   R1,LCOMMFIL         SET L(FILTER) FOR LATER USE                  
FF0010   EQU   *                                                                
         BAS   RE,PROCRECS         PROCESS AGENCY RECORDS                       
*                                                                               
*    END OF AGENCY RECORD PROCESSING                                            
*                                                                               
FF0030   EQU   *                                                                
         CLI   ADVFILON,0          ANY ADVERTISER RECORDS NEEDED?               
         BE    FF0060              NO  -                                        
         MVI   COMMREC,X'88'       FILTER FLAG: WHICH RECD ID                   
         MVC   COMMTYP,ADVFILON                 WHICH RECORD TYPE               
         XC    LCOMMFIL(LCOMMEQU),LCOMMFIL                                      
         XC    KEY,KEY             ESTABLISH AGENCY PASSIVE KEY                 
         MVI   KEY,X'88'           KEY TYPE                                     
         ZIC   R1,LADVFILT         ANY ADVERTISER FILTER?                       
         LTR   R1,R1                                                            
         BZ    FF0040              NO                                           
         LA    R2,ADVFILT          ADVERTISER FILTER STRING                     
         LA    R3,KEY+1            SET FILTER IN KEY                            
         BCTR  R1,0                DECREMENT BY 1                               
         EX    R1,FF0800                                                        
         LA    R3,COMMFILT         SET FILTER IN COMMON AREA                    
         EX    R1,FF0800                                                        
         LA    R1,1(R1)            INCREMENT BY 1                               
         STC   R1,LCOMMFIL         SET L(FILTER) FOR LATER USE                  
FF0040   EQU   *                                                                
         BAS   RE,PROCRECS         PROCESS AGENCY RECORDS                       
*                                                                               
*    END OF ADVERTISER RECORD PROCESSING                                        
*                                                                               
FF0060   EQU   *                                                                
         LA    R2,ITRSDATA         SET ITEM TYPE                                
         L     R3,ABLDAREA         A(NEXT AVAILABLE ENTRY) -                    
         LA    R4,BLDAREA          A(BLDAREA) =                                 
         SR    R3,R4               L(ENTRIES IN BLDAREA)                        
         GOTO1 PUTITEM,DMCB,(R2),(R3),BLDAREA                                   
         BNE   XIT                                                              
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   XIT                                                              
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                  (ONLY A SINGLE FRAME)                        
         B     XIT                                                              
*                                                                               
FF0800   MVC   0(0,R3),0(R2)       INSERT STRING                                
*                                                                               
         EJECT                                                                  
*                                                                               
* THIS ROUTINE PROCESSES A SET OF RECORDS, APPLYING FILTERS, AND                
*  PLACING THOSE THAT ARE ACCEPTED INTO A BLOCK                                 
*                                                                               
PROCRECS NTR1                                                                   
         SR    R5,R5               INITIALIZE COUNTER                           
PR0004   EQU   *                                                                
         GOTO1 HIGH                RETRIEVE FIRST KEY                           
         B     PR0008                                                           
PR0006   EQU   *                                                                
         GOTO1 SEQ                 RETRIEVE NEXT KEY                            
PR0008   EQU   *                                                                
         CLC   KEY(1),COMMREC      SAME RECORD TYPE?                            
         BNE   PR0099              NO  - FINISHED                               
         CLC   SIGNON2C(2),KEY+25  SAME REP?                                    
         BNE   PR0006              NO  - RETRIEVE NEXT                          
         ZIC   R1,LCOMMFIL         ANY FILTER?                                  
         LTR   R1,R1                                                            
         BZ    PR0010              NO                                           
         LA    R2,KEY+1                                                         
         BCTR  R1,0                DECREMENT BY 1                               
         EX    R1,PR0800           TEST FILTER                                  
         BH    PR0099              DONE - FILTER EXCEEDED                       
*                                                                               
PR0010   EQU   *                                                                
         L     R1,ABLDAREA         A(OUTPUT AREA)                               
         USING CT08OUT,R1                                                       
*                                                                               
         MVC   RIREQCDE,COMMTYP    INSERT RECORD TYPE                           
         CLI   COMMTYP,C'1'        PROCESSING ADVERTISER?                       
         BNE   PR0020              NO  - AGENCY                                 
         MVC   RIPRICDE(4),KEY+21  INSERT ADVERTISER CODE                       
         LA    R2,RILEXADV         SET UP TO INSERT ADV NAME                    
         LA    R3,20               L(ADVERTISER NAME FIELD)                     
         B     PR0030              TO COMMON PROCESSOR                          
PR0020   EQU   *                                                                
         MVC   RIPRICDE(6),KEY+19  INSERT AGENCY CODE AND AGENCY OFF            
         LA    R2,RILEXAGY         SET UP TO INSERT AGY NAME                    
         LA    R3,18               L(AGENCY NAME FIELD)                         
PR0030   EQU   *                                                                
         LA    R4,KEY+1            START OF NAME FIELD +                        
         AR    R4,R3               L(NAME FIELD) -                              
         BCTR  R4,0                1  = A(LAST POSITION OF NAME)                
PR0032   EQU   *                                                                
         CLI   0(R4),C' '          SCAN BACKWARD FOR NON-SPACE                  
         BNE   PR0034              NOT SPACE - SCAN FINISHED                    
         BCTR  R4,0                BACK UP ONE POSITION                         
         BCT   R3,PR0032           LOOP BACK                                    
*                                                                               
*  WHEN LOOP IS FINISHED, R3 CONTAINS LENGTH OF FIELD MINUS TRAILING            
*    SPACES.                                                                    
*                                                                               
PR0034   EQU   *                                                                
         EDIT  (R3),(2,(R2)),FILL=0                                             
         LA    R2,2(R2)            SET TO A(EXPANSION FIELD)                    
*                                                                               
*  TEST TO SEE IF ANY MORE SPACE EXISTS IN THE BLOCK.  ADD L(NEW                
*    ENTRY) TO A(NEW ENTRY), AND COMPARE RESULT TO END OF BLOCK.                
*                                                                               
         LR    R4,R2               A(NEW ENTRY)                                 
         AR    R4,R3               L(NEW ENTRY)                                 
         LA    R6,ENDBLDAR         A(END OF BLOCK)                              
         CR    R4,R6               ANY ROOM LEFT?                               
         BNL   PR0099              NO  - LEAVE NOW!!                            
         BCTR  R3,0                DECREMENT BY 1                               
         EX    R3,PR0801           INSERT STRING W/O TRAILING SPACES            
         LA    R3,1(R3)            INCREMENT BY 1                               
         AR    R2,R3               A(NEXT SLOT IN BLDAREA)                      
         ST    R2,ABLDAREA         SAVE IT OFF                                  
*                                                                               
*   NOTE:  R2 IS STORED AS THE A(NEXT SLOT), AND THEN R1 IS RELOADED            
*     TO SET THE DSECT                                                          
*                                                                               
         LA    R5,1(R5)            INCREMENT COUNTER                            
         CH    R5,=H'30'           30 ENTRIES IN BLDAREA?                       
         BL    PR0006              NO  - CHECK FOR MORE                         
PR0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
PR0800   CLC   0(0,R2),COMMFILT                                                 
*                                                                               
PR0801   MVC   0(0,R2),KEY+1                                                    
*                                                                               
         DROP  R1                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
EXIT     EQU   *                                                                
         L     RD,SAVEDRD          RESTORE RD                                   
XIT      EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* CT08IDSECT                                                                    
       ++INCLUDE CT08IDSECT                                                     
         EJECT                                                                  
* CT08ODSECT                                                                    
       ++INCLUDE CT08ODSECT                                                     
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
INFOSTOR EQU   *                   FILTER FLAG STORAGE                          
ADVFILON DS    CL1                 ADVERTISER NEEDED                            
LADVFILT DS    X                   LENGTH OF ADVERTISER FILTER                  
ADVFILT  DS    CL8                 STORAGE FOR ADVERTISER FILTER                
AGYFILON DS    CL1                 AGENCY     NEEDED                            
LAGYFILT DS    X                   LENGTH OF AGENCY FILTER                      
AGYFILT  DS    CL8                 STORAGE FOR AGENCY FILTER                    
LCOMMFIL DS    X                   LENGTH OF COMMON FILTER                      
COMMFILT DS    CL8                 COMMON FILTER                                
LCOMMEQU EQU   *-COMMFILT                                                       
COMMREC  DS    CL1                 ID OF RECORD                                 
COMMTYP  DS    CL1                 TYPE OF RECORD                               
*                                  1 = ADVERTISER                               
*                                  2 = AGENCY                                   
         DS    0D                                                               
WORKAREA DS    CL8                                                              
INFOLEN  EQU   *-INFOSTOR                                                       
ABLDAREA DS    A                   A(CURRENT LOCATION IN BLDAREA)               
BLDAREA  DS    1500C               BUILD AREA FOR FRAME                         
ENDBLDAR EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041CTMAD08   05/01/02'                                      
         END                                                                    
