*          DATA SET CTMAD09    AT LEVEL 192 AS OF 05/01/02                      
*PHASE TA0C09A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE BOOKVAL                                                                
         TITLE 'TA0C09 - $MAD REP CONTRACT MAINFRAME PROCESSOR'                 
**********************************************************************          
*   HISTORY OF CHANGES                                                          
**********************************************************************          
*   03/20/91   (BU ) --- ORIGINAL ENTRY                                         
**********************************************************************          
TA0C09   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C09,RA,R8,RR=R2                                             
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
*                                                                               
         ST    R2,RELO             SAVE RELOCATION ADDRESS                      
*                                                                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
*        USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
*                                                                               
*  NOTE:  NO ROUTINES IN USUKD ARE USED BY THIS MODULE.  THE ADDRESS            
*     PASSED IN R8 IS THEREFORE FORFEIT TO GAIN AN ADDITIONAL BASE              
*     REGISTER.  BILL UHR.  JULY2/91                                            
*                                                                               
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
*        INITIALIZE OVERLAY WIDE REGISTERS                                      
*                                                                               
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
*                                                                               
         DROP  R7                  DROP 1ST APPLIC COMMON STORAGE               
*                                                                               
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
         XC    INFOSTOR(INFOLEN),INFOSTOR                                       
         GOTO1 SETSYS,DMCB,(3,=C'REP'),=CL8'REPDIR',=CL8'REPFILE'               
*                                                                               
*  SET TWO DATES:  TODAY, AND MONDAY'S DATE WITHIN THIS WEEK                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,TODAY),(3,TODAY)                                  
*                                                                               
*  CALCULATE MONDAY'S DATE FROM TODAY'S DATE                                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,TODAY),(0,WORKAREA)                               
         GOTO1 GETDAY,DMCB,WORKAREA,WORKAREA+6                                  
         ZIC   R3,DMCB             DAY OF WEEK IN FIRST BYTE ON RETURN          
         BCTR  R3,0                MAKE ZERO RELATIVE                           
         LTR   R3,R3               IS TODAY A MONDAY?                           
         BZ    IN0004              YES - USE AS IS                              
         LNR   R3,R3               NO  - MAKE VALUE NEGATIVE                    
IN0004   EQU   *                                                                
         ST    R3,DMCB+8           NUMBER OF DAYS (+/-) IN P3                   
         GOTO1 ADDAY,DMCB,WORKAREA,WORKAREA+6                                   
         GOTO1 DATCON,DMCB,WORKAREA+6,(2,WORKAREA+12)                           
         MVC   MONDATE,WORKAREA+12                                              
*                                                                               
         XC    WORKAREA(50),WORKAREA                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   DATVAL,CDATVAL      SET A(DATE VALIDATION ROUTINE)               
*                                                                               
         DROP  RF                                                               
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'PROCSTRT' PROCESSES THE START MODE.  IT PROCESSES THE CONTRACT               
* REQUEST OBJECT PASSED BY 'COPILOT' AND RETURNS:                               
*   WHEN SUCCESSFULLY VALIDATED, NEW HEADLINE NUMBER PLUS EXPANSIONS            
*       FOR CODES                                                               
*   WHEN VALIDATION FAILS:  INDICATORS FOR WHICH CODE(S) FAILED, AS             
*       WELL AS EXPANSIONS FOR THOSE CODES WHICH WERE OKAY                      
*                                                                               
PROCSTRT NTR1                                                                   
*                                                                               
         GOTO1 GETITEM             GET FIRST ITEM                               
         BNE   EXIT                                                             
*                                                                               
         L     R1,TYPENUM                                                       
         LA    R2,ITCONREQ                                                      
         CR    R1,R2               (CONTRACT ADDITION REQUEST)                  
         BNE   PS0010              NO                                           
         BAS   RE,PROCRQST         PROCESS REQUEST                              
         B     XIT                                                              
*                                                                               
PS0010   EQU   *                                                                
         LA    R2,ITCDTREQ                                                      
         CR    R1,R2               (EXISTING CONTRACT RETURN)                   
         BNE   PS0020              NO  - ERROR ON INPUT                         
         BAS   RE,PROCRET          PROCESS FOR RETURN                           
*                                                                               
*   NO ERROR ROUTINE FOR INCORRECT TYPE                                         
*                                                                               
PS0020   EQU   *                                                                
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* 'PROCMID' PROCESSES MIDDLE MODE.  IT CURRENTLY DOES NOTHING                   
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* 'PROCEND' PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.                 
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
* 'PROCRET':                                                                    
*    FINDS A CONTRACT BASED ON CONTRACT NUMBER SENT FROM PC                     
*    BUILDS A FRAME IN WORK AREA:                                               
*       INDICATES CONTRACT FOUND/NOT FOUND                                      
*       IF FOUND, RETURNS DETAILS                                               
*                                                                               
PROCRET  NTR1                                                                   
         L     R3,ADATA            SET A(INPUT DATA)                            
         L     R2,AIO1             SET A(IO AREA)                               
         ST    R2,AIO              SAVE IT BACK                                 
*                                                                               
         GOTO1 GETCONTR,DMCB,(R3),(R2)                                          
         BNZ   PR0099              NOT FOUND - EXIT                             
         GOTO1 LOADCONT,DMCB,(R2)                                               
*                                                                               
PR0099   EQU   *                   RETURN RESULTING FRAME                       
*                                                                               
         LA    R2,ITCDTRET         SET ITEM TYPE                                
         L     R3,LENCRWRK         LENGTH OF RETURN FRAME                       
         GOTO1 PUTITEM,DMCB,(R2),(R3),BLDAREA                                   
         BNE   EXIT                                                             
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
         B     EXIT                FINAL EXIT                                   
         EJECT                                                                  
* 'GETCONTR':                                                                   
*    TAKES CONTRACT NUMBER FROM FRAME ITEM TYPE 2902                            
*    CONVERTS TO 9'S COMPLEMENT, BUILDS X'8C' KEY                               
*    LOOKS FOR CONTRACT ON FILE - NOT FOUND RETURNS NON-ZERO                    
*                                                                               
GETCONTR NTR1                                                                   
         LA    R3,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT2,R3                                                      
         L     R2,AIO              LOAD A(CURRENT IO AREA)                      
         USING RCONRECD,R2                                                      
         L     R1,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN2,R1                                                       
*                                                                               
         MVC   CDTYPE(7),MYSPACES  INITIALIZE OUTPUT FRAME                      
         LA    RE,7                SET INITIAL FRAME LENGTH                     
         ST    RE,LENCRWRK         USED IF NO CONTRACT FOUND                    
         XC    TEMPAREA(50),TEMPAREA                                            
         MVC   TEMPAREA(8),=8C'9'  FILL WITH 9S                                 
         LA    R6,CRCONT#          POINT TO THE NUMBER                          
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R4,8                ITEM HAS LEADING ZEROS                       
         LA    R5,TEMPAREA                                                      
GC0010   EQU   *                                                                
         MVC   0(1,R5),0(R6)       COPY A CHARACTER                             
         NI    0(R5),X'FF'-X'F0'   TAKE OFF ZONE DIGIT                          
         ZIC   RE,0(R5)                                                         
         LA    RE,COMP9(RE)        COMPLEMENT OF 9 LINE                         
         MVC   0(1,R5),0(RE)                                                    
         OI    0(R5),X'F0'         PUT BACK THE ZONE                            
         LA    R6,1(R6)                                                         
         LA    R5,1(R5)                                                         
         BCT   R4,GC0010                                                        
*                                                                               
         XC    RCONKEY,RCONKEY     ESTABLISH X'8C' KEY                          
         MVI   RCONKEY,X'8C'                                                    
         MVC   RCONPREP,SIGNON2C   INSERT REP CODE                              
         GOTO1 HEXIN,DMCB,TEMPAREA,RCONPCON,8                                   
         MVC   KEY,RCONPTYP                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     CONTRACT FOUND?                              
         BE    GC0098              YES - SET FOUND CC                           
         LTR   RB,RB               NO - SET NOT FOUND CC                        
         B     XIT                 EXIT                                         
GC0098   EQU   *                                                                
         GOTO1 GETREC              RETRIEVE RECORD                              
         SR    R3,R3               SET CC = FOUND                               
GC0099   EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* 'LOADCONT':                                                                   
*    AFTER CONTRACT HAS BEEN FOUND, ROUTINE BUILDS RETURN ITEM                  
*    2903 FOR DISPLAY UPON THE PC.                                              
*                                                                               
LOADCONT NTR1                                                                   
*                                                                               
         LA    R2,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT2,R2                                                      
         L     R3,0(R1)            LOAD A(CONTRACT RECORD)                      
         USING RCONRECD,R3                                                      
*                                                                               
         MVI   BLDAREA,C' '        SPACE FILL OUTPUT BUILD AREA                 
         MVC   BLDAREA+1(250),BLDAREA                                           
         LA    RE,FIX2903          SET FIXED ITEM LENGTH                        
         ST    RE,LENCRWRK         SAVE IT OFF                                  
*                                                                               
*  FILL IN RETURN FRAME FROM CONTRACT RECORD  - EXPANSIONS FILLED               
*   IN LATER.                                                                   
*                                                                               
         CLI   RCONTYPE,X'00'      'REGULAR' CONTRACT TYPE?                     
         BE    LC0005              YES - DON'T PUT IN ANYTHING                  
         MVC   CDTYPE(1),RCONTYPE                                               
LC0005   EQU   *                                                                
         MVC   CDSTATN,RCONKSTA                                                 
         MVC   SAVDATES,RCONDATE   SAVE FLIGHT DATES FOR LATER                  
         MVC   CDBUYER,RCONBUYR                                                 
         MVC   CDAGENCY(6),RCONKAGY MOVE AGENCY/AGENCY OFFICE                   
         MVC   CDADVERT,RCONKADV                                                
         MVC   CDPROD,RCONPRD                                                   
         MVC   SAVPROD,MYSPACES    SET AREA TO SPACES                           
         L     R6,AIO1             A(CONTRACT RECORD)                           
         GOTO1 GETELEM,DMCB,5                                                   
         BNE   LC0008              NO X'05' - NO PRODUCT NAME ELT               
         ZIC   RE,1(R6)            LENGTH OF PRODUCT NAME                       
         LA    RF,SAVPROD                                                       
         EX    RE,LC0006                                                        
         B     LC0008                                                           
*                                                                               
LC0006   MVC   0(0,RF),2(R6)       SAVE PRODUCT NAME                            
*                                                                               
LC0008   EQU   *                                                                
         MVC   CDCTGRY,RCONCTGY                                                 
         MVC   CDSLPRSN,RCONSAL                                                 
         MVC   CDSERV,RCONRTGS                                                  
         L     R6,AIO1             A(CONTRACT RECORD)                           
         PRINT GEN                                                              
         GOTO1 GETELEM,DMCB,X'000000A2'                                         
         PRINT NOGEN                                                            
         BNE   LC0010              NO X'A2' - NOT ELEC INVOICING                
         USING RCONIEL,R6          INSERT EI CODES INTO FRAME                   
         MVC   CDEIADV,RCONIADV                                                 
         MVC   CDEIPROD,RCONIPRD                                                
         MVC   CDEIEST,RCONIEST                                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
LC0010   EQU   *                                                                
         LA    R5,CDCMTTYP         A(1ST COMMENT ENTRY)                         
         L     R6,AIO1             A(CONTRACT RECORD)                           
         PRINT GEN                                                              
         GOTO1 GETELEM,DMCB2,2                                                  
         PRINT NOGEN                                                            
         SR    R1,R1               INITIALIZE COUNTER                           
         XC    SAVR1,SAVR1         INITIALIZE SAVE AREA FOR COUNTER             
         B     LC0014                                                           
LC0012   EQU   *                                                                
         ST    R1,SAVR1            SAVE R1                                      
         GOTO1 NEXTELEM,DMCB2      LOOK FOR NEXT ELEMENT                        
LC0014   EQU   *                                                                
         BNE   LC0016              FINISHED WITH CONTRACT ELEMENTS              
         L     R1,SAVR1            RESTORE VALUE OF R1                          
         LA    R1,1(R1)            BUMP COMMENT COUNTER                         
         MVI   0(R5),C'H'          SET 'HEADLINE COMMENT' INDICATOR             
         ZIC   RF,1(R6)            L(COMMENT)                                   
         SH    RF,=H'2'            MINUS L(ELEMENT CONTROL)                     
         EDIT  (RF),(2,1(R5)),FILL=0                                            
         BCTR  RF,0                MINUS 1 FOR 'EX' STATEMENT                   
         EX    RF,LC0080           MOVE COMMENT BY LENGTH                       
         LA    RF,1(RF)            ADD BACK                                     
         L     RE,LENCRWRK         INCREMENT FRAME LENGTH                       
         AR    RE,RF               ADD L(COMMENT)                               
         LA    RE,3(RE)            ADD L(COMMENT DESCRIPTIVE)                   
         ST    RE,LENCRWRK         SAVE IT OFF AGAIN                            
         AR    R5,RF               SET FOR NEXT COMMENT (IF ANY)                
         LA    R5,3(R5)                                                         
         B     LC0012              LOOK FOR NEXT COMMENT                        
LC0016   EQU   *                   INSERT COMMENT COUNT                         
         L     R1,SAVR1            RESTORE VALUE OF R1                          
         EDIT  (R1),(2,CD#CMTS),FILL=0                                          
*                                                                               
*  RETRIEVE NECESSARY EXPANSIONS, INSERT INTO OUTPUT FRAME                      
*                                                                               
LC0020   EQU   *                                                                
         GOTO1 INSMKT,DMCB,(R2)      INSERT STATION'S MARKET NAME               
         GOTO1 INSFLT,DMCB,(R2)      INSERT FLIGHT DATES                        
         GOTO1 INSAGY,DMCB,(R2)      INSERT AGENCY EXPANSION                    
         GOTO1 INSADV,DMCB,(R2)      INSERT ADVERTISER EXPANSION                
         GOTO1 INSPRD,DMCB,(R2)      INSERT PRODUCT EXPANSION                   
         GOTO1 INSCTG,DMCB,(R2)      INSERT CATEGORY EXPANSION                  
         GOTO1 INSSLS,DMCB,(R2)      INSERT SALESPERSON EXPANSION               
         B     LC0099                                                           
*                                                                               
LC0080   MVC   3(0,R5),2(R6)         INSERT COMMENT                             
*                                                                               
LC0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
INSMKT   NTR1                                                                   
         L     R2,AIO              LOAD A(CURRENT IO AREA)                      
         USING RSTARECD,R2                                                      
         L     R3,0(R1)            LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT2,R3                                                      
*                                                                               
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKEY,2           ESTABLISH RECORD KEY                         
         MVC   RSTAKREP,SIGNON2C   SET REP CODE                                 
         MVC   RSTAKSTA,CDSTATN    SET STATION CALLS                            
         MVC   KEY,RSTAKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INMK0099            KEY NOT FOUND                                
         GOTO1 GETREC              RETRIEVE RECORD                              
         MVC   CDSTAMKT,RSTAMKT    INSERT NAME OF MARKET                        
INMK0099 EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
INSFLT   NTR1                                                                   
         L     R2,0(R1)            LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT2,R2                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,SAVDATES),(5,WORKAREA)                            
         MVC   CDSTDT,WORKAREA                                                  
         GOTO1 DATCON,DMCB,(3,SAVDATES+3),(5,WORKAREA)                          
         MVC   CDENDT,WORKAREA                                                  
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
INSAGY   NTR1                                                                   
         L     R2,AIO              LOAD A(CURRENT IO AREA)                      
         USING RAGYRECD,R2                                                      
         L     R3,0(R1)            LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT2,R3                                                      
*                                                                               
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKEY,10          ESTABLISH KEY                                
         MVI   RAGYKAGY,C' '       SPACE FILL SIX BYTES                         
         MVC   RAGYKAGY+1(5),RAGYKAGY                                           
         MVC   RAGYKAGY(6),CDAGENCY  INSERT AGENCY + AGY OFF CODE               
         MVC   RAGYKREP,SIGNON2C     INSERT REP CODE                            
         MVC   KEY,RAGYKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   INAG0099            NOT FOUND - SPACES SENT BACK                 
         CLC   KEY+25(2),SIGNON2C  CORRECT REP CODE?                            
         BE    INAG0008            YES - ACCEPT THIS ENTRY                      
         MVC   KEYSAVE+25(2),=C'ZZ'                                             
         CLC   KEY+25(2),=C'ZZ'    'GENERIC' REP CODE?                          
         BE    INAG0008            YES - ACCEPT THIS ENTRY                      
         MVC   KEY+25(2),=C'ZZ'    NO  - INSERT ZZ AS REP CODE                  
         GOTO1 HIGH                LOOK AGAIN                                   
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INAG0099            NOT FOUND - SEND SPACES BACK                 
INAG0008 EQU   *                                                                
         GOTO1 GETREC                                                           
         MVC   CDAGYEXP,RAGYNAM1   INSERT AGENCY EXPANSION                      
INAG0099 EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
INSADV   NTR1                                                                   
         L     R2,AIO              LOAD A(CURRENT IO AREA)                      
         USING RADVRECD,R2                                                      
         L     R3,0(R1)            LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT2,R3                                                      
*                                                                               
         XC    RADVKEY,RADVKEY                                                  
         MVI   RADVKEY,8           ESTABLISH KEY                                
         MVI   RADVKADV,C' '       SPACE FILL FOUR BYTES                        
         MVC   RADVKADV+1(3),RADVKADV                                           
         MVC   RADVKADV,CDADVERT   INSERT ADVERTISER CODE                       
         MVC   RADVKREP,SIGNON2C   INSERT REP CODE                              
         MVC   KEY,RADVKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   INAD0099            NOT FOUND - SEND SPACES BACK                 
         CLC   KEY+25(2),SIGNON2C  CORRECT REP CODE?                            
         BE    INAD0008            YES - ACCEPT THIS ENTRY                      
         MVC   KEYSAVE+25(2),=C'ZZ'                                             
         CLC   KEY+25(2),=C'ZZ'    'GENERIC' REP CODE?                          
         BE    INAD0008            YES - ACCEPT THIS ENTRY                      
         MVC   KEY+25(2),=C'ZZ'    NO  - INSERT ZZ AS REP CODE                  
         GOTO1 HIGH                LOOK AGAIN                                   
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INAD0099            NOT FOUND - SEND SPACES BACK                 
INAD0008 EQU   *                                                                
         GOTO1 GETREC                                                           
         MVC   CDADVEXP,RADVNAME   INSERT ADVERT EXPANSION                      
INAD0099 EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
INSPRD   NTR1                                                                   
         L     R2,AIO              LOAD A(CURRENT IO AREA)                      
         USING RPRDRECD,R2                                                      
         L     R3,0(R1)            LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT2,R3                                                      
*                                                                               
         MVC   CDPRODEX,SAVPROD    IF NO PRODUCT CODE                           
         CLC   CDPROD,MYSPACES     PRODUCT CODE = SPACE?                        
         BE    INPR0099            YES - GET X'05' ELEMENT                      
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKEY,9           ESTABLISH KEY                                
         MVC   RPRDKADV,CDADVERT   INSERT ADVERTISER CODE                       
         MVC   RPRDKPRD,CDPROD     INSERT PRODUCT CODE                          
         MVC   KEY,RPRDKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   INPR0099            NOT FOUND - SEND SPACES BACK                 
         CLC   KEY+25(2),SIGNON2C  SAME REP?                                    
         BE    INPR0008            YES                                          
         CLC   KEY+25(2),=C'ZZ'    DEFAULT REP?                                 
         BE    INPR0008            YES                                          
         MVC   KEY+25(2),=C'ZZ'    TRY DEFAULT REP                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INPR0099            NOT FOUND - SEND SPACES BACK                 
INPR0008 EQU   *                                                                
         GOTO1 GETREC                                                           
         MVC   CDPRODEX,RPRDNAME   INSERT PRODUCT EXPANSION                     
INPR0099 EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
INSCTG   NTR1                                                                   
         L     R2,AIO              LOAD A(CURRENT IO AREA)                      
         USING RCTGRECD,R2                                                      
         L     R3,0(R1)            LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT2,R3                                                      
*                                                                               
         XC    RCTGKEY,RCTGKEY                                                  
         MVI   RCTGKEY,X'0F'       ESTABLISH KEY                                
         MVC   RCTGKREP+23(2),SIGNON2C  INSERT REP CODE                         
         MVC   RCTGKCTG,CDCTGRY    INSERT CATEGORY CODE                         
         MVC   KEY,RCTGKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   INCT0099            NOT FOUND - SEND SPACES BACK                 
         GOTO1 GETREC                                                           
         MVC   CDCATEXP,RCTGNAME   INSERT EXPANSION INTO FRAME                  
INCT0099 EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
INSSLS   NTR1                                                                   
         L     R2,AIO              LOAD A(CURRENT IO AREA)                      
         USING RSALRECD,R2                                                      
         L     R3,0(R1)            LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT2,R3                                                      
*                                                                               
         XC    RSALKEY,RSALKEY                                                  
         MVI   RSALKEY,6           ESTABLISH KEY                                
         MVC   RSALKREP,SIGNON2C   INSERT REP CODE                              
         MVC   RSALKSAL,CDSLPRSN   INSERT SALES PERSON CODE                     
         MVC   KEY,RSALKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INSL0099            NOT FOUND - SEND BACK SPACES                 
         GOTO1 GETREC                                                           
         MVC   CDSLSEXP,RSALNAME   INSERT SALESPERSON EXPANSION                 
         MVC   CDSPOFF,RSALOFF     INSERT SALESPERSON OFFICE                    
INSL0099 EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
* 'PROCRQST':                                                                   
*    VALIDATES CODES PASSED FROM PC                                             
*    BUILDS A FRAME IN WORK AREA:                                               
*       INDICATES CODE SUCCESSFULLY/UNSUCCESSFULLY VALIDATED                    
*       ADDS EXPANSIONS FOR CODES, IF VALIDATION SUCCESSFUL                     
*    IF ALL CODES VALID, ADDS NEW HEADLINE NUMBER TO FRAME                      
*       OR UPDATES EXISTING CONTRACT                                            
*    RETURNS FRAME WHEN PROCESSING COMPLETE                                     
*                                                                               
PROCRQST NTR1                                                                   
*                                                                               
*                                                                               
         MVI   BLDAREA,C' '        SPACE FILL BLDAREA                           
         MVC   BLDAREA+1(250),BLDAREA                                           
*                                                                               
         L     R3,ADATA            SET A(INPUT DATA)                            
         L     R2,AIO1             SET A(IO AREA)                               
         ST    R2,AIO              SAVE IT BACK                                 
         LA    R2,CONTAREA         SET A(CONTRACT RECORD BUILD AREA)            
*                                                                               
VR0002   EQU   *                                                                
*                                                                               
         GOTO1 SETCON,DMCB,(R2)         INITIALIZE CONTRACT AREA                
         GOTO1 TYPEADD,DMCB,(R3),(R2)   ADD TYPE CODE TO OUTPUT                 
         GOTO1 VALSTATN,DMCB,(R3),(R2)  VALIDATE AND ADD STATION                
         GOTO1 FLGHTADD,DMCB,(R3),(R2)  ADD FLIGHT DATES TO OUTPUT              
         GOTO1 BUYERADD,DMCB,(R3),(R2)  ADD BUYER TO OUTPUT                     
         GOTO1 VALAGY,DMCB,(R3),(R2)    VALIDATE AND ADD AGENCY                 
         GOTO1 VALADV,DMCB,(R3),(R2)    VALIDATE AND ADD ADVERTISER             
         GOTO1 VALPROD,DMCB,(R3),(R2)   VALIDATE AND ADD PRODUCT                
         GOTO1 VALCTGY,DMCB,(R3),(R2)   VALIDATE AND ADD CATEGORY               
         GOTO1 VALSLSPN,DMCB,(R3),(R2)  VALIDATE AND ADD SALESPERSON            
         GOTO1 SERVADD,DMCB,(R3),(R2)   ADD SERVICE TO OUTPUT                   
         GOTO1 VALEASI,DMCB,(R3),(R2)   VALIDATE EASI CODES                     
         GOTO1 COMMADD,DMCB,(R3),(R2)   ADD COMMENTS TO OUTPUT                  
*                                                                               
         CLI   ERRFLAG,C'Y'        ANY ERRORS FOUND?                            
         BE    VR0008              YES - SKIP MISCELLANEOUS                     
*                                                                               
         GOTO1 MISCADD,DMCB,(R3),(R2)   ADD ADDITIONAL TO OUTPUT                
*                                                                               
VR0008   EQU   *                                                                
         LA    R2,ITCONRET         SET ITEM TYPE                                
         LA    R3,LENCPWRK         LENGTH OF RETURN FRAME                       
         GOTO1 PUTITEM,DMCB,(R2),(R3),BLDAREA                                   
         BNE   EXIT                                                             
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
VR0098   EQU   *                                                                
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                  (ONLY A SINGLE FRAME)                        
VR0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*  SETCON SETS UP CONTRACT BUILD AREA BY SKELETONIZING RECORD                   
*                                                                               
SETCON   NTR1                                                                   
         L     R2,0(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R2                                                      
*                                                                               
         XC    RCONREC(250),RCONREC                                             
         MVI   RCONLEN+1,94           REC LEN (34 + 60)                         
         MVC   RCONELEM(2),=X'013C'   DESC ELEM CODE + LENGTH                   
*                                                                               
         DROP  R2                                                               
*                                                                               
SC0099   EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  TYPEADD ADDS TYPE CODE FIELD FROM UPLOAD FRAME TO CONTRACT RECORD            
*                                                                               
TYPEADD  NTR1                                                                   
         L     R2,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R2                                                      
         L     R3,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R3                                                        
*                                                                               
         MVC   RCONTYPE,CTTYPE     INSERT CONTRACT TYPE                         
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
AT0099   EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  VALSTATN VALIDATES THE STATION CALL LETTERS.                                 
*                                                                               
VALSTATN NTR1                                                                   
         L     R5,AIO              LOAD A(CURRENT IO AREA)                      
         USING RSTARECD,R5                                                      
         LA    R4,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT,R4                                                       
         L     R3,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R3                                                      
         L     R2,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R2                                                        
*                                                                               
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKEY,2           ESTABLISH RECORD KEY                         
         MVC   RSTAKREP,SIGNON2C   SET REP CODE                                 
         MVC   RSTAKSTA,CTSTATN    SET STATION CALLS                            
         MVC   KEY,RSTAKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VS0090              KEY NOT FOUND                                
         GOTO1 GETREC              RETRIEVE RECORD                              
         MVC   SAVSTAST,RSTASTAT   SAVE STATION STATUS                          
         TM    RSTASTAT,X'40'      TEST FOR STATION LOCKOUT                     
         BNZ   VS0092              LOCKED OUT - ERROR                           
         L     R6,AIO              A(RECORD AREA)                               
         GOTO1 GETELEM,DMCB,5                                                   
         BNE   VS0008              NO X'05' - NOT ACE/GRAPHNET                  
         OC    10(2,R6),10(R6)     ANY RECEIVING ID?                            
         BZ    VS0008              NO                                           
         CLC   10(2,R6),=X'0406'   GRAPHNET?                                    
         BNE   VS0004              NO                                           
         OI    RCONMODR+1,X'40'    YES - MARK CONTRACT GRAPHNET                 
         B     VS0008                                                           
VS0004   EQU   *                                                                
         OI    RCONMODR+1,X'80'    ELSE, IT'S ACE                               
VS0008   EQU   *                                                                
*                                                                               
*   FIELD SUCCESSFULLY VALIDATED                                                
*                                                                               
         MVC   CPSTNMKT,RSTAMKT    NAME OF MARKET                               
         MVI   CPSTNERR,C'0'       SET 'STATION ACCEPTED' INDICATOR             
         MVC   RCONKGRP,RSTAGRUP   SET CONTRACT RECORD FIELD                    
         MVC   RCONKSTA,RSTAKSTA   SET CONTRACT RECORD FIELD                    
         MVC   SAVSTCDT,RSTACLDT   SAVE STATION CLOSE DATE                      
         B     VS0099                                                           
*                                                                               
VS0090   EQU   *                                                                
         MVC   CPSTNMKT,=C'STATION NOT FOUND   '                                
         B     VS0098                                                           
VS0092   EQU   *                                                                
         MVC   CPSTNMKT,=C'STATION LOCKED OUT  '                                
         B     VS0098                                                           
VS0098   EQU   *                                                                
         MVI   CPSTNERR,C'1'       SET 'STATION FAILED' INDICATOR               
         MVI   ERRFLAG,C'Y'        SET 'ERROR OCCURRED' FLAG                    
VS0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3,R4,R5                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
*  FLGHTADD ADDS FLIGHT DATES TO CONTRACT RECORD BEING CONSTRUCTED.             
*                                                                               
FLGHTADD NTR1                                                                   
         L     R2,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R2                                                      
         L     R3,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R3                                                        
*                                                                               
*   CONVERT DATE TO YYMMDD FORMAT, FOR DATCON AND ALSO FOR # OF                 
*      WEEKS CALCULATION                                                        
*                                                                               
         GOTO1 DATVAL,DMCB,CTSTDT,WORKAREA+30                                   
         GOTO1 DATVAL,DMCB,CTENDT,WORKAREA+36                                   
*                                                                               
         GOTO1 DATCON,DMCB,WORKAREA+30,(3,RCONDATE)                             
         GOTO1 DATCON,DMCB,WORKAREA+36,(3,RCONDATE+3)                           
*                                                                               
*  CALCULATE NUMBER OF WEEKS IN CONTRACT BY BUMPING START DATE                  
*    THRU END DATE                                                              
*                                                                               
         XC    WORKAREA(30),WORKAREA                                            
         SR    R4,R4                                                            
         MVC   WORKAREA(6),WORKAREA+30   LOAD START DATE                        
AF0010   EQU   *                                                                
         LA    R4,1(R4)                  INCREMENT WEEK COUNT                   
         GOTO1 ADDAY,DMCB,WORKAREA,WORKAREA+6,7                                 
         MVC   WORKAREA(6),WORKAREA+6                                           
         CLC   WORKAREA(6),WORKAREA+36                                          
         BNH   AF0010                                                           
         STC   R4,RCONWKS          LOAD WEEK COUNT TO RECORD                    
AF0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*  BUYERADD ADDS BUYER FIELD FROM UPLOAD FRAME TO CONTRACT RECORD               
*                                                                               
BUYERADD NTR1                                                                   
         L     R2,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R2                                                      
         L     R3,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R3                                                        
*                                                                               
         MVC   RCONBUYR,CTBUYER    INSERT BUYER                                 
*                                                                               
AB0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*  VALAGY VALIDATES THE AGENCY CODE.                                            
*                                                                               
VALAGY   NTR1                                                                   
         L     R5,AIO              LOAD A(CURRENT IO AREA)                      
         USING RAGYRECD,R5                                                      
         LA    R4,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT,R4                                                       
         L     R3,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R3                                                      
         L     R2,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R2                                                        
*                                                                               
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKEY,10          ESTABLISH KEY                                
         MVI   RAGYKAGY,C' '       SPACE FILL SIX BYTES                         
         MVC   RAGYKAGY+1(5),RAGYKAGY                                           
         MVC   RAGYKAGY(6),CTAGENCY  INSERT AGENCY + AGY OFF CODE               
         MVC   RAGYKREP,SIGNON2C     INSERT REP CODE                            
         MVC   KEY,RAGYKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VA0090              NOT FOUND - SET ERROR                        
         CLC   KEY+25(2),SIGNON2C  CORRECT REP CODE?                            
         BE    VA0008              YES - ACCEPT THIS ENTRY                      
         MVC   KEYSAVE+25(2),=C'ZZ'                                             
         CLC   KEY+25(2),=C'ZZ'    'GENERIC' REP CODE?                          
         BE    VA0008              YES - ACCEPT THIS ENTRY                      
         MVC   KEY+25(2),=C'ZZ'    NO  - INSERT ZZ AS REP CODE                  
         GOTO1 HIGH                LOOK AGAIN                                   
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VA0090              NOT FOUND - SET ERROR                        
VA0008   EQU   *                                                                
         GOTO1 GETREC                                                           
         MVC   CPAGYNAM,RAGYNAM1   INSERT EXPANSION INTO FRAME                  
         CLC   KEY+23(2),=C'  '    ANY OFFICE ENTERED?                          
         BNE   VA0012              YES - ACCEPT ENTRY                           
VA0010   EQU   *                                                                
         GOTO1 SEQ                 NO  - SHOULD ONE BE ENTERED?                 
         CLC   KEY(23),RAGYKEY     SAME AGENCY?                                 
         BNE   VA0012              NO  - ACCEPT ENTRY                           
         CLC   KEY+23(2),=C'  '    ANY OFFICE CODE?                             
         BE    VA0010              NO  - BYPASS                                 
         CLC   KEY+25(2),RAGYKREP  OFFICE CODE - SAME REP?                      
         BE    VA0092              YES - ERROR                                  
*                                                                               
VA0012   EQU   *                                                                
         MVI   CPAGYERR,C'0'          SET 'AGENCY ACCEPTED' INDICATOR           
         MVC   RCONKAGY(6),RAGYKAGY   LOAD CONTRACT RECORD (AGY + OFF)          
         B     VA0099                                                           
*                                                                               
VA0090   EQU   *                                                                
         MVC   CPAGYNAM,=C'AGENCY  NOT FOUND   '                                
         B     VA0098                                                           
VA0092   EQU   *                                                                
         MVC   CPAGYNAM,=C'AGENCY NEEDS OFFICE '                                
         B     VA0098                                                           
VA0098   EQU   *                                                                
         MVI   CPAGYERR,C'1'       SET 'AGENCY FAILED' INDICATOR                
         MVI   ERRFLAG,C'Y'        SET 'ERROR OCCURRED' FLAG                    
VA0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3,R4,R5                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
*  VALADV VALIDATES THE ADVERTISER CODE.                                        
*                                                                               
VALADV   NTR1                                                                   
         L     R5,AIO              LOAD A(CURRENT IO AREA)                      
         USING RADVRECD,R5                                                      
         LA    R4,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT,R4                                                       
         L     R3,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R3                                                      
         L     R2,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R2                                                        
*                                                                               
         XC    RADVKEY,RADVKEY                                                  
         MVI   RADVKEY,8           ESTABLISH KEY                                
         MVI   RADVKADV,C' '       SPACE FILL FOUR BYTES                        
         MVC   RADVKADV+1(3),RADVKADV                                           
         MVC   RADVKADV,CTADVERT   INSERT ADVERTISER CODE                       
         MVC   RADVKREP,SIGNON2C   INSERT REP CODE                              
         MVC   KEY,RADVKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VD0090              NOT FOUND - SET ERROR                        
         CLC   KEY+25(2),SIGNON2C  CORRECT REP CODE?                            
         BE    VD0008              YES - ACCEPT THIS ENTRY                      
         MVC   KEYSAVE+25(2),=C'ZZ'                                             
         CLC   KEY+25(2),=C'ZZ'    'GENERIC' REP CODE?                          
         BE    VD0008              YES - ACCEPT THIS ENTRY                      
         MVC   KEY+25(2),=C'ZZ'    NO  - INSERT ZZ AS REP CODE                  
         GOTO1 HIGH                LOOK AGAIN                                   
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VD0090              NOT FOUND - SET ERROR                        
VD0008   EQU   *                                                                
         GOTO1 GETREC                                                           
         MVC   CPADVNAM,RADVNAME   INSERT EXPANSION INTO FRAME                  
         MVI   CPADVERR,C'0'       SET 'ADVERTSER ACCEPTED' INDICATOR           
         MVC   RCONKADV,RADVKADV   LOAD CONTRACT RECORD                         
         B     VD0099                                                           
*                                                                               
VD0090   EQU   *                                                                
         MVC   CPADVNAM,=C'ADVERTISER MISSING  '                                
         B     VD0098                                                           
VD0098   EQU   *                                                                
         MVI   CPADVERR,C'1'       SET 'AGENCY FAILED' INDICATOR                
         MVI   ERRFLAG,C'Y'        SET 'ERROR OCCURRED' FLAG                    
VD0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3,R4,R5                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
*  VALPROD VALIDATES, IF ENTERED, THE PRODUCT CODE, AND RETURNS FOR             
*  IT THE APPROPRIATE CATEGORY CODE.                                            
*                                                                               
VALPROD  NTR1                                                                   
         L     R5,AIO              LOAD A(CURRENT IO AREA)                      
         USING RPRDRECD,R5                                                      
         LA    R4,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT,R4                                                       
         L     R3,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R3                                                      
         L     R2,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R2                                                        
*                                                                               
         CLC   CTPROD(2),=C'C='    PRODUCT CODE ENTERED?                        
         BE    VP0004              YES - CHECK IT OUT                           
         CLI   CTTYPE,C'N'         NO  - IS IT NEEDED?                          
         BE    VP0090              YES - ERROR OUT                              
         B     VP0020              NO  - EXPANSION ENTERED                      
*                                  LOAD IT TO CONTRACT RECORD                   
VP0004   EQU   *                                                                
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKEY,9           ESTABLISH KEY                                
         MVC   RPRDKADV,CTADVERT   INSERT ADVERTISER CODE                       
         MVC   RPRDKPRD,CTPROD     INSERT PRODUCT CODE                          
         MVC   KEY,RPRDKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VP0092              NOT FOUND                                    
         CLC   KEY+25(2),SIGNON2C  SAME REP?                                    
         BE    VP0008              YES                                          
         CLC   KEY+25(2),=C'ZZ'    DEFAULT REP?                                 
         BE    VP0008              YES                                          
         MVC   KEY+25(2),=C'ZZ'    TRY DEFAULT REP                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VP0092              NOT FOUND                                    
VP0008   EQU   *                                                                
         GOTO1 GETREC                                                           
         MVC   RCONPRD,CTPROD      LOAD CONTRACT RECORD                         
         MVC   CPPRDNAM,RPRDNAME   LOAD RETURN FRAME                            
         MVI   CPPRDERR,C'0'       SET 'PRODUCT ACCEPTED' INDICATOR             
         MVC   SAVECTG,RPRDCATG    STORE CATEGORY CODE                          
         B     VP0099                                                           
VP0020   EQU   *                                                                
         XC    ELAREA(100),ELAREA  INITIALIZE ELEMENT AREA                      
         MVC   ELAREA(2),=X'0516'  EXPANSION CODE + LEN                         
         MVC   ELAREA+2(20),CTPROD INSERT PRODUCT EXPANSION                     
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELAREA                     
         MVC   RCONPRD(3),=C'   '  SPACE OUT                                    
         MVI   CPPRDERR,C'0'       SET 'PRODUCT ACCEPTED' INDICATOR             
         MVC   CPPRDNAM,CTPROD     SEND EXPANSION BACK - IT'S FREE              
         B     VP0099                                                           
*                                                                               
VP0090   EQU   *                                                                
         MVC   CPPRDNAM,=C'PRODUCT CODE NEEDED '                                
         B     VP0098                                                           
*                                                                               
VP0092   EQU   *                                                                
         MVC   CPPRDNAM,=C'PRODUCT NOT FOUND   '                                
         B     VP0098                                                           
VP0098   EQU   *                                                                
         MVI   CPPRDERR,C'1'       SET 'PRODUCT FAILED' INDICATOR               
         MVI   ERRFLAG,C'Y'        SET 'ERROR OCCURRED' FLAG                    
VP0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3,R4,R5                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
*  VALCTGY VALIDATES THE CATEGORY CODE.                                         
*                                                                               
VALCTGY  NTR1                                                                   
         L     R5,AIO              LOAD A(CURRENT IO AREA)                      
         USING RCTGRECD,R5                                                      
         LA    R4,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT,R4                                                       
         L     R3,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R3                                                      
         L     R2,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R2                                                        
*                                                                               
         MVI   CPCTGERR,C'0'       SET 'CATEGORY PASSED' INDICATOR              
         CLC   CTCTGRY,=C'  '      ANY CATEGORY CODE ENTERED?                   
         BE    VC0099              NO  - NO FURTHER TESTS                       
         XC    RCTGKEY,RCTGKEY                                                  
         MVI   RCTGKEY,X'0F'       ESTABLISH KEY                                
         MVC   RCTGKREP(2),SIGNON2C  INSERT REP CODE                            
         CLC   CTCTGRY,=C'XX'      WHAT IS THIS TEST????                        
         BE    VC0090              ERROR FOR SOME REASON                        
         MVC   RCTGKCTG,CTCTGRY    INSERT CATEGORY CODE                         
         CLC   CTPROD(2),=C'C='    PRODUCT CODE ENTERED?                        
         BNE   VC0010              NO                                           
         CLC   SAVECTG,=C'  '      YES - ANY CATEGORY IN PROD RECORD?           
         BE    VC0010              NO                                           
         MVC   KEY+25(2),SAVECTG   YES - USE IT                                 
VC0010   EQU   *                                                                
         MVC   KEY,RCTGKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VC0092              NOT FOUND                                    
         GOTO1 GETREC                                                           
         MVC   RCONCTGY,RCTGKCTG   INSERT CATEGORY INTO CONTRACT                
         MVC   CPCTGNAM,RCTGNAME   INSERT EXPANSION INTO FRAME                  
         B     VC0099              EXIT                                         
*                                                                               
VC0090   EQU   *                                                                
         MVC   CPCTGNAM,=C'NOT VALID '                                          
         B     VC0098                                                           
*                                                                               
VC0092   EQU   *                                                                
         MVC   CPCTGNAM,=C'NOT FOUND '                                          
         B     VC0098                                                           
VC0098   EQU   *                                                                
         MVI   CPCTGERR,C'1'       SET 'CATEGORY FAILED' INDICATOR              
         MVI   ERRFLAG,C'Y'        SET 'ERROR OCCURRED' FLAG                    
VC0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3,R4,R5                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
*  VALSLSPN VALIDATES THE SALESPERSON CODE ENTERED, AND AFTER VALID-            
*   ATING, RETURNS THE OFFICE CODE                                              
*                                                                               
VALSLSPN NTR1                                                                   
         L     R5,AIO              LOAD A(CURRENT IO AREA)                      
         USING RSALRECD,R5                                                      
         LA    R4,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT,R4                                                       
         L     R3,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R3                                                      
         L     R2,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R2                                                        
*                                                                               
         XC    RSALKEY,RSALKEY                                                  
         MVI   RSALKEY,6           ESTABLISH KEY                                
         MVC   RSALKREP,SIGNON2C   INSERT REP CODE                              
         MVC   RSALKSAL,CTSLPRSN   INSERT SALES PERSON CODE                     
         MVC   KEY,RSALKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VL0090              SALESPERSON NOT FOUND                        
         GOTO1 GETREC                                                           
         MVC   CPSLSNAM,RSALNAME   INSERT SALESPERSON NAME IN FRAME             
         MVC   RCONTEM,RSALTEAM    INSERT TEAM INTO CONTRACT REC                
         MVC   RCONSAL,RSALKSAL    INSERT SP'S CODE INTO CONTRACT REC           
         MVC   RCONKOFF,RSALOFF    INSERT SP'S OFFICE INTO CONTRACT REC         
         MVC   FULL(2),RSALOFF     SAVE OFFICE TEMPORARILY                      
*                                                                               
*  RETRIEVE OFFICE NAME FOR SALESPERSON'S OFFICE                                
*                                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
         L     R5,AIO              LOAD A(CURRENT IO AREA)                      
         USING ROFFRECD,R5                                                      
         XC    ROFFKEY,ROFFKEY                                                  
         MVI   ROFFKEY,4           ESTABLISH KEY                                
         MVC   ROFFKREP,SIGNON2C   INSERT REP CODE                              
         MVC   ROFFKOFF,FULL       USE SALESPERSON'S OFFICE                     
         MVC   KEY,ROFFKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VL0092              OFFICE NOT FOUND! MAJOR ERROR                
*                                                                               
*  SECURITY CHECKING DONE AT THIS POINT SHOULD HAVE BEEN ACCOMPLISHED           
*  EARLY ON AT THE PC.  IF REQUIRED, REINSERT IT HERE.                          
*                                                                               
         GOTO1 GETREC                                                           
         MVC   CPSLSOFF,ROFFNAME   INSERT OFFICE NAME INTO FRAME                
         MVI   CPSLSERR,C'0'       SET 'SALESPERSON ACCEPTED' INDIC             
         B     VL0099              EXIT                                         
*                                                                               
VL0090   EQU   *                                                                
         MVC   CPSLSNAM,=C'SALESPERSON INVALID '                                
         B     VL0098                                                           
*                                                                               
VL0092   EQU   *                                                                
         MVC   CPSLSNAM,=C'OFFICE NOT ON FILE!!'                                
         B     VL0098                                                           
VL0098   EQU   *                                                                
         MVI   CPSLSERR,C'1'       SET 'SALESPERSON FAILED' INDICATOR           
         MVI   ERRFLAG,C'Y'        SET 'ERROR OCCURRED' FLAG                    
VL0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3,R4,R5                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
*  SERVADD ADDS SERVICE FIELD FROM UPLOAD FRAME TO CONTRACT RECORD              
*                                                                               
SERVADD  NTR1                                                                   
         L     R2,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R2                                                      
         L     R3,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R3                                                        
*                                                                               
         MVI   RCONRMON,0          SET RATING MONTH TO ZERO                     
         MVI   RCONRTGS,0          SET RATING SERVICE TO DEFAULT                
         CLI   CTSERV,C' '         SERVICE = 'NONE'? X'40'                      
         BE    AS0099              YES - LEAVE DEFAULT                          
         MVC   RCONRTGS,CTSERV     SET RATING SERVICE                           
AS0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*  VALEASI VALIDATES THE EASI CODES (ADVERTISER, PRODUCT, ESTIMATE)             
*                                                                               
VALEASI  NTR1                                                                   
         L     R5,AIO              LOAD A(CURRENT IO AREA)                      
         USING RSTARECD,R5                                                      
         LA    R4,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT,R4                                                       
         L     R3,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R3                                                      
         L     R2,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R2                                                        
*                                                                               
         MVI   TEMPAREA,0          INDICATE EASI                                
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,2          ESTABLISH KEY                                
         MVC   RSTAKREP,SIGNON2C   INSERT REP CODE                              
         MVC   RSTAKSTA,RCONKSTA                                                
         MVC   KEY,RSTAKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),RSTAKEY                                                  
         BNE   VE0090              NOT FOUND                                    
         GOTO1 GETREC              RETRIEVE RECORD                              
         LA    R6,RSTAREC                                                       
         GOTO1 GETELEM,DMCB,8                                                   
         BNE   VE0010              NO X'08' ELEMENT                             
*                                                                               
         USING RSTAXXEL,R6                                                      
         CLI   RSTAOPT4,C'Y'       IS THIS AN EASI STATION?                     
         BNE   VE0010              NO - DON'T BOTHER W/AGY                      
*                                                                               
         DROP  R5,R6                                                            
*                                                                               
         USING RAGYRECD,R5                                                      
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKTYP,10         ESTABLISH KEY                                
         MVC   RAGYKAGY,RCONKAGY   INSERT AGENCY CODE                           
         MVC   RAGYKAOF,=C'  '                                                  
         MVC   RAGYKREP,SIGNON2C   INSERT REP CODE                              
         MVC   KEY,RAGYKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),RAGYKEY                                                  
         BNE   VE0092              NOT FOUND                                    
         GOTO1 GETREC                                                           
         LA    R6,RAGYREC                                                       
         GOTO1 GETELEM,DMCB,1                                                   
         BNE   VE0094              AGENCY RECORD DAMAGED                        
*                                                                               
         USING RAGYELEM,R6                                                      
         CLI   RAGYPRO1,C'Y'       IS THIS AN EASI AGY?                         
         BNE   VE0010              NO                                           
*                                                                               
         DROP  R5,R6                                                            
*                                                                               
         MVI   TEMPAREA,1          EASI STA/AGY - SET FLAG                      
VE0010   EQU   *                                                                
         CLC   CTEIADV,MYSPACES    ANY INPUT?                                   
         BNE   VE0030              YES - GET NEXT FIELD                         
         CLI   TEMPAREA,1          NO  - EASI STATION?                          
         BE    VE0094              YES - CODES REQUIRED: ERROR                  
*                                  NO  - NO EASI FLDS ALLOWED                   
         CLC   CTEIPROD,MYSPACES                                                
         BNE   VE0094                                                           
         CLC   CTEIEST,MYSPACES                                                 
         BNE   VE0094                                                           
         B     VE0099                                                           
VE0030   EQU   *                   ALL FIELDS MUST BE INPUT                     
         CLC   CTEIPROD,MYSPACES                                                
         BE    VE0096                                                           
         CLC   CTEIEST,MYSPACES                                                 
         BE    VE0096                                                           
         XC    ELAREA(100),ELAREA  ADD ELEMENT                                  
         LA    R6,ELAREA                                                        
         USING RCONIEL,R6                                                       
         MVI   RCONICD,X'A2'                                                    
         MVI   RCONILN,32                                                       
         MVC   RCONIPRD,CTEIPROD                                                
         MVC   RCONIADV,CTEIADV                                                 
         MVC   RCONIEST,CTEIEST                                                 
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELAREA                     
         MVC   CPEIERR,C'0'        SET 'EI CODES ACCEPTED' INDICATOR            
         B     VE0099                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
VE0090   EQU   *                                                                
         MVC   CPEIMSG,=C'STATION NOT FOUND             '                       
         B     VE0098                                                           
*                                                                               
VE0092   EQU   *                                                                
         MVC   CPEIMSG,=C'AGENCY NOT ON FILE!!          '                       
         B     VE0098                                                           
*                                                                               
VE0094   EQU   *                                                                
         MVC   CPEIMSG,=C'NO CODES PERMITTED: NOT EASI  '                       
         B     VE0098                                                           
*                                                                               
VE0096   EQU   *                                                                
         MVC   CPEIMSG,=C'CODES REQUIRED: EASI STATION  '                       
         B     VE0098                                                           
VE0098   EQU   *                                                                
         MVI   CPEIERR,C'1'        SET 'EI CODES FAILED' INDICATOR              
         MVI   ERRFLAG,C'Y'        SET 'ERROR OCCURRED' FLAG                    
VE0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3,R4                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
*  COMMADD ADDS COMMENT RECORDS FROM UPLOAD FRAME TO CONTRACT RECORD            
*                                                                               
COMMADD  NTR1                                                                   
         L     R2,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R2                                                      
         L     R3,0(R1)            LOAD A(INPUT FRAME DSECT)                    
         USING CT09IN,R3                                                        
*                                                                               
         PACK  PACKWORK(8),CT#CMTS(2)                                           
         CVB   R6,PACKWORK         STORE # CMTS IN REG3                         
         LTR   R6,R6               ANY VALUE THEREIN?                           
         BZ    AC0099              NO  - EXIT                                   
         LA    R4,CTCMTTYP         SET A(1ST COMMENT TYPE)                      
AC0004   EQU   *                                                                
         MVC   COMMTYPE,0(R4)      SAVE COMMENT TYPE                            
         PACK  PACKWORK(8),1(2,R4) COMMENT LENGTH                               
         CVB   R5,PACKWORK         GET LENGTH OF COMMENT                        
         XC    ELAREA(100),ELAREA                                               
         MVI   ELAREA,2            SET ELEMENT TYPE TO HEADLINE CMMT            
         CLI   COMMTYPE,C'H'       HEADLINE TYPE COMMENT?                       
         BE    AC0006              YES                                          
         MVI   ELAREA,11           NO  - SET TYPE TO SAR CMMT                   
AC0006   EQU   *                                                                
         LA    R1,2(R5)            BUMP LENGTH FOR CONTROL                      
         STC   R1,ELAREA+1         SET ELEMENT LENGTH                           
         MVC   ELAREA+2(60),3(R4)  INSERT COMMENT                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELAREA                     
         LA    R4,3(R4)            BYPASS TYPE, LENGTH FIELDS                   
         AR    R4,R5               ADD LENGTH OF COMMENT                        
         BCT   R6,AC0004           PROCESS EACH COMMENT                         
AC0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*  MISCADD CONSTRUCTS ADDITIONAL FIELDS REQUIRED FOR CONTRACT RECORD            
*                                                                               
MISCADD  NTR1                                                                   
         L     R3,0(R1)            LOAD A(INPUT DATA)                           
         USING CT09IN,R3                                                        
         L     R2,4(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R2                                                      
*                                                                               
         ST    R2,AIO              SWITCH A(IO AREA) TO NEW                     
*                                  CONTRACT RECORD                              
         OC    SAVSTCDT,SAVSTCDT   ANY CLOSE DATE?                              
         BZ    AM0012              NO                                           
*                                                                               
*   IF CLOSE DATE, MONTHS IN FLIGHT UP TO AND INCLUDING CLOSE MONTH             
*   MUST HAVE ZERO INVOICE ELEMENTS GENERATED, TO ENSURE THAT NO                
*   BUSINESS IS ENTERED PRIOR TO CLOSE DATE                                     
*                                                                               
         CLC   RCONDATE(2),SAVSTCDT FLIGHT START VS CLOSE DATE                  
         BH    AM0012              FLIGHT START AFTER CLOSE                     
         XC    TEMPAREA(50),TEMPAREA                                            
         GOTO1 DATCON,DMCB,(3,RCONDATE),TEMPAREA                                
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),TEMPAREA+6                            
*                                                                               
         XC    ELAREA(50),ELAREA   INITIALIZE ELEMENT CONSTRUCT AREA            
         XC    HALF,HALF           SET 1ST-TIME SWITCH                          
*                                                                               
         MVC   ELAREA(2),=X'040A'  SKELETONIZE ELEMENT                          
         MVC   ELAREA+4(2),MONDATE INSERT MONDAY'S DATE                         
         XC    ELAREA+6(4),ELAREA+6                                             
AM0004   EQU   *                                                                
         GOTO1 =V(GETBROAD),DMCB,(1,TEMPAREA),TEMPAREA+12,GETDAY,ADDAY,X        
               RR=Y                                                             
         GOTO1 DATCON,DMCB,TEMPAREA+18,(3,FULL)                                 
         CLC   FULL(2),SAVSTCDT    VS CLOSE DATE                                
         BH    AM0012              FINISHED                                     
         OC    HALF,HALF           1ST TIME?                                    
         BZ    AM0006              YES                                          
*                                                                               
         CLC   HALF,FULL           SAME MONTH?                                  
         BE    AM0010              YES                                          
AM0006   EQU   *                                                                
         MVC   HALF,FULL           SAVE YEAR/MONTH                              
         MVC   ELAREA+2(2),FULL    INSERT YR/MO INTO BUCKET                     
AM0008   EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'G',=C'REPFILE'),(4,RCONREC),(10,ELAREA)            
         TM    DMCB+12,X'06'       FOUND?                                       
         BZ    AM0010              DON'T PUT DUPE ELEMENT                       
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,RCONREC),(0,ELAREA)             
*                                                                               
AM0010   EQU   *                   GET NEXT WEEK                                
         GOTO1 ADDAY,DMCB,TEMPAREA,TEMPAREA+24,7                                
         MVC   TEMPAREA(6),TEMPAREA+24                                          
         CLC   TEMPAREA(6),TEMPAREA+6                                           
         BNH   AM0004              GO BACK FOR NEXT                             
AM0012   EQU   *                                                                
         MVI   RCONKTYP,12         CONTRACT RECORD TYPE                         
         MVC   RCONKREP,SIGNON2C   INSERT REP CODE                              
         MVC   RCONCREA,TODAY      CREATION OR BUYLINE 1 ADDED DATE             
         MVC   RCONHDRD,TODAY      HEADER CREATION DATE (NEVER CHANGED)         
         TM    RCONMODR+1,X'C0'    ONLY ACE/GRAPHNET SET MOD TO -1              
         BZ    AM0013                                                           
         MVI   RCONMOD,X'FF'                                                    
AM0013   EQU   *                                                                
         MVI   CONUPDT,0           SET CONTRACT ADDITION FLAG                   
         CLC   CTHDLN#,MYSPACES    ANY HEADLINE # ENTERED?                      
         BE    AM0014              NO  - NOT A CONTRACT UPDATE                  
         MVI   CONUPDT,1           SET CONTRACT UPDATE FLAG                     
*                                                                               
         LA    R4,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT,R4                                                       
*                                                                               
         MVC   CPHDLN#,CTHDLN#     INSERT INTO OUTPUT FRAME                     
         PACK  PACKWORK(8),CTHDLN#(8)                                           
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
         MVO   TEMPAREA(5),PACKWORK+3(5)                                        
         MVC   RCONKCON,TEMPAREA                                                
         B     AM0018                                                           
*                                                                               
AM0014   EQU   *                   GET NEXT REP CONTRACT NUMBER                 
         ZAP   TEMPAREA(5),=P'99999999'                                         
         ZAP   TEMPAREA+5(5),=P'99999999'                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           PASSIVE POINTER KEY TYPE                     
         MVC   KEY+21(2),SIGNON2C  ALPHA REP CODE                               
         GOTO1 HIGH                                                             
         CLC   KEY(23),KEYSAVE     SAME REP?                                    
         BNE   AM0016                                                           
*                                                                               
*              GET NEXT CONTRACT NUMBER                                         
*                                                                               
         MVO   TEMPAREA+5(5),KEY+23(4)       K NUMBER                           
AM0016   EQU   *                                                                
         SP    TEMPAREA(5),TEMPAREA+5(5)     GET POSITIVE                       
         AP    TEMPAREA(5),=P'1'             NEXT K NUMBER                      
         MVO   TEMPAREA+10(5),TEMPAREA(5)                                       
         MVC   RCONKCON,TEMPAREA+10          TO K KEY                           
*                                                                               
         LA    R4,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT,R4                                                       
*                                                                               
         GOTO1 HEXOUT,DMCB,TEMPAREA+10,CPHDLN#,4,=C'TOG'                        
*                                                                               
         DROP  R4                                                               
*                                                                               
AM0018   EQU   *                                                                
*                                                                               
* FOR ACE/GRAPHNET CONTRACTS, MARK UNCONFIRMED, & BUILD X'20' SEND EL           
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    AM0028                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         GOTO1 GETELEM,DMCB,X'0000001F'                                         
         BNE   AM0024                                                           
*                                                                               
         USING RCONXEL,R6          EXTENDED DESCRIPTION ELEMENT                 
         OI    RCONCONF,X'80'      UNCONFIRMED                                  
         TM    SAVSTAST,X'02'      IS 'DON'T SEND' ALLOWED ON STATION           
         BZ    *+8                                                              
         OI    RCONSTAT,X'02'      YES                                          
         DROP  R6                                                               
         B     AM0026                                                           
         SPACE 1                                                                
AM0024   XC    ELAREA(100),ELAREA       BUILD NEW ELEMENT                       
         MVC   ELAREA(2),=X'1F18'                                               
         OI    ELAREA+6,X'80'      UNCONFIRMED                                  
         TM    SAVSTAST,X'02'      IS 'DON'T SEND' ALLOWED ON STATION           
         BZ    *+8                                                              
         OI    ELAREA+7,X'02'      YES                                          
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELAREA                     
*                                                                               
AM0026   EQU   *                      ADD SEND ELEMENT                          
         XC    ELAREA(100),ELAREA                                               
         MVC   ELAREA(2),=X'201E'                                               
         MVI   ELAREA+4,X'10'       START STA VERS. NOT ADVANCED                
         MVI   ELAREA+5,1           SET REP VERSION NUMBER TO 1                 
         MVI   ELAREA+14,0          SET STATION VER. NUMBER TO 0                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELAREA                     
*                                                                               
AM0028   EQU   *                                                                
         GOTO1 SARADD,DMCB,(R2)    ADD SAR INFO, IF PRESENT                     
         CLI   CONUPDT,0           CONTRACT UPDATE REQUESTED?                   
         BE    AM0032              NO  - ADDITION REQUESTED                     
*                                                                               
*   GET 9'S COMPLEMENT OF CONTRACT NUMBER, PRESENTLY STORED IN                  
*     'PACKWORK'                                                                
*                                                                               
         ZAP   TEMPAREA(5),=P'99999999'                                         
         SP    TEMPAREA(5),PACKWORK+3(5)                                        
         XC    KEY,KEY             BUILD KEY TO ENSURE                          
         MVI   KEY,X'8C'           THAT RECORD EXISTS FOR                       
         MVC   KEY+21(2),SIGNON2C  CONTRACT # ENTERED                           
         MVO   TEMPAREA+5(5),TEMPAREA(5)                                        
         MVC   KEY+23(4),TEMPAREA+5     INSERT UPDATE CONT #                    
         MVC   TEMPAREA+5(5),TEMPAREA   RESET FOR LATER                         
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     CONTRACT FOUND?                              
         BE    AM0030              YES - CONTINUE TO PROCESS                    
*                                                                               
         LA    R4,BLDAREA          LOAD A(OUTPUT FRAME DSECT)                   
         USING CT09OUT,R4                                                       
*                                                                               
         MVC   CPHDLN#,=C'NOTFOUND' FORCE 'NOT FOUND' INDICATOR                 
*                                                                               
         DROP R4                                                                
*                                                                               
         MVI   ERRFLAG,C'Y'        SET ERROR INDICATOR                          
         B     AM0099              EXIT: DON'T SET/RESET POINTERS               
*                                                                               
*  IN THIS PATH, A CONTRACT UPDATE HAS BEEN REQUESTED.  IT IS NECESSARY         
*    TO RETRIEVE THE ORIGINAL CONTRACT RECORD, BUILD A SET OF PASSIVE           
*    POINTERS FOR IT, THEN BUILD A SET FOR THE NEW RECORD.  THE NEW             
*    RECORD WILL OVER-WRITE THE ORIGINAL RECORD.  THE KEYS WILL THEN            
*    BE COMPARED ONE AT A TIME, AND FOR THOSE DIFFERING, THE NEW KEY            
*    WILL BE ADDED, THE ORIGINAL KEY MARKED AS DELETED.                         
*                                                                               
AM0030   EQU   *                                                                
*                                                                               
*  FIRST CALCULATE THE 9'S COMPLEMENT REVERSED CONTRACT NUMBER FOR              
*    CONSTRUCTION OF THE X'EC' SAR PASSIVE KEY                                  
*                                                                               
         MVO   TEMPAREA+15(5),TEMPAREA+5(5)                                     
         MVC   SAV9COMP,TEMPAREA+15                                             
         PACK  SAV9COMP(1),TEMPAREA+18(1)        REVERSE                        
         PACK  SAV9COMP+1(1),TEMPAREA+17(1)       THE                           
         PACK  SAV9COMP+2(1),TEMPAREA+16(1)        COMPLEMENT                   
         PACK  SAV9COMP+3(1),TEMPAREA+15(1)                                     
*                                                                               
         MVC   SAVAIO,AIO          SAVE A(NEW CONTRACT RECORD)                  
         L     R2,AIO1             RESET A(IO AREA)                             
         ST    R2,AIO              USING 'RCONRECD' STILL APPLIES               
         MVI   RDUPDATE,C'Y'       INDICATE 'READ FOR UPDATE'                   
         GOTO1 GETREC              RETRIEVE ORIGINAL CONTRACT RECORD            
*                                                                               
* PASSIVE PTRS FOR ORIGINAL CONTRACT RECORD: USE 1ST 500 BYTES                  
*    OF AIO2                                                                    
*                                                                               
         L     R3,AIO2             A(SECOND IO AREA)                            
         LR    RE,R3                                                            
         XCEF  (RE),500            INITIALIZE 1ST 500 BYTES OF AIO2             
         MVC   0(27,R3),RCONREC    SAVE ORIGINAL MASTER POINTER                 
*                                                                               
* BUILD PASSIVE PTRS FOR ORIGINAL CONTRACT RECORD                               
*                                                                               
         GOTO1 PTRS,DMCB,(R3),1,(R2)                                            
*                                                                               
         L     R2,SAVAIO           RESTORE A(NEW CONTRACT RECORD)               
         ST    R2,AIO                                                           
         MVC   KEY(27),RCONREC     INSERT PRIMARY KEY FOR REWRITE               
         CLC   0(27,R3),RCONREC    NEW KEY = ORIGINAL KEY?                      
         BNE   AM0031              NO  - ADD NEW RECORD                         
         GOTO1 PUTREC              YES - REWRITE ORIGINAL RECORD                
         MVC   DISKADDR,KEY+28     SAVE D/A OF REWRITTEN RECORD                 
         B     AM0036              DON'T SUBTRACT 1 FROM COMP KEY               
AM0031   EQU   *                                                                
         GOTO1 ADDORPUT            ADD OR PUT CHECK                             
         B     AM0036              DON'T SUBTRACT 1 FROM COMP KEY               
AM0032   EQU   *                                                                
         GOTO1 ADDREC                                                           
         MVC   DISKADDR,KEY        SAVE DISK ADDRESS OF RECORD                  
AM0034   EQU   *                                                                
         SP    TEMPAREA+5(5),=P'1'            CALCULATE 9'S COMP KEY            
AM0036   EQU   *                                                                
         MVO   TEMPAREA+15(5),TEMPAREA+5(5)                                     
         MVC   SAV9COMP,TEMPAREA+15                                             
         PACK  SAV9COMP(1),TEMPAREA+18(1)        REVERSE                        
         PACK  SAV9COMP+1(1),TEMPAREA+17(1)       THE                           
         PACK  SAV9COMP+2(1),TEMPAREA+16(1)        COMPLEMENT                   
         PACK  SAV9COMP+3(1),TEMPAREA+15(1)                                     
*                                                                               
* PASSIVE PTRS FOR NEW/UPDATED CONTRACT RECORD                                  
*                                                                               
         L     R3,AIO2             A(SECOND IO AREA)                            
*                                                                               
* BUILD PASSIVE PTRS IN SECOND 500 BYTES OF AIO2                                
*                                                                               
         LA    R4,500(R3)          SET A(2ND 500 BYTES OF AIO2)                 
         MVC   0(27,R4),RCONREC    SAVE NEW MASTER POINTER                      
         PRINT GEN                                                              
         GOTO1 PTRS,DMCB,(R4),0,(R2)                                            
* ADD PTRS:  ADDITION USES ADDPTRS  - UPDATE USES MODPTRS                       
         CLI   CONUPDT,0           ADDITION?                                    
         BNE   AM0038                                                           
         GOTO1 ADDPTRS,DMCB,(R3),(R4),DISKADDR                                  
         B     AM0099                                                           
AM0038   EQU   *                                                                
         GOTO1 MODPTRS,DMCB,(R3),(R4),DISKADDR                                  
         PRINT NOGEN                                                            
*                                                                               
AM0099   EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
* ROUTINE TO ADD SALES ACTIVITY REPORTING INFORMATION, IF SENT FROM             
*     MAINFRAME AS A TYPE 2904 OBJECT                                           
*                                                                               
SARADD   NTR1                                                                   
*                                                                               
         L     R2,0(R1)            LOAD A(CONTRACT RECORD AREA)                 
         USING RCONRECD,R2                                                      
*                                                                               
         GOTO1 GETITEM             RETRIEVE NEXT OBJECT                         
         BNE   SA0099              EXIT IF NONE FOUND                           
         L     RE,TYPENUM                                                       
         LA    RF,ITCONSAR         IS IT AN SAR ELEMENT?                        
         CR    RE,RF                                                            
         BNE   SA0099              NO  - FINISHED                               
*                                                                               
         L     R3,ADATA            A(SAR OBJECT)                                
         USING CT09IN3,R3                                                       
*                                                                               
         XC    ELAREA(160),ELAREA       SET UP AREA TO ADD X'12'                
         MVC   ELAREA(2),=X'1278'                                               
*                                                                               
         LA    R4,ELAREA                                                        
         USING RSAREL,R4                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(5,ELAREA),(3,RSAREDT)                               
*                                                                               
*  PROBABLY NEED TO SAVE ADD DATE ON AN UPDATE TO REPLACE                       
*                                                                               
         LA    R5,STDEMCDS         A(INPUT DEMO CODES)                          
         LA    R6,RSARDEM          A(OUTPUT 3-BYTE ENTRIES)                     
         LA    RE,WORKAREA                                                      
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
         LA    RF,8                                                             
SA0002   EQU   *                                                                
         CLI   0(R5),C'0'          GEOGRAPHICAL INDICATOR TRANSLATE?            
         BNE   SA0004              NOT NEEDED                                   
         MVI   0(R5),X'00'         INSERT BINARY ZEROS                          
SA0004   EQU   *                                                                
         MVC   0(1,R6),0(R5)       INSERT GEOGRAPHICAL INDICATOR                
         MVC   1(1,R6),1(R5)       INSERT DEMOGRAPHIC QUALIFIER                 
         PACK WORKAREA(8),2(3,R5)  CONVERT EBCDIC TO BINARY                     
         CVB   RE,WORKAREA                                                      
         STC   RE,2(R6)            INSERT DEMO CODE #                           
         LA    R5,5(R5)            BUMP A(INPUT)                                
         LA    R6,3(R6)            BUMP A(OUTPUT)                               
         CLC   =C'     ',0(R5)     ANY INPUT IN NEXT FIELD?                     
         BE    SA0006              NO  - FINISHED                               
         BCT   RF,SA0002           GO BACK FOR NEXT                             
SA0006   EQU   *                                                                
         MVC   RSARSRC,RCONRTGS    INSERT SERVICE                               
         LA    RE,WORKAREA                                                      
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
         L     R5,DATALEN          L(INPUT ITEM DATA)                           
         LA    R6,LSTDSCT3         L(DSECT WITHOUT BOOKS)                       
         CR    R5,R6               SAME SIZE?                                   
         BE    SA0010              YES - NO BOOKS ENTERED                       
         SR    R5,R6               CALC L(BOOKS FIELD)                          
         LR    R6,R5               SAVE L(BOOKS)                                
         LA    R6,2(R6)            ADD 2 FOR 'SERVICE+,'                        
         STC   R6,WORKAREA+5       L('FIELD' HEADER)                            
         BCTR  R5,0                DECREMENT FOR EXECUTE                        
         MVC   WORKAREA+8(1),RCONRTGS STRING SERVICE INTO FIELD                 
         MVI   WORKAREA+9,C','     STRING COMMA INTO FIELD                      
         LA    R6,WORKAREA+10      A(DATA IN FIELD)                             
*                                                                               
         EX    R5,SA0006EX                                                      
*                                                                               
         B     SA0008                                                           
*                                                                               
SA0006EX MVC   0(0,R6),STBOOKS                                                  
*                                                                               
SA0008   EQU   *                                                                
         GOTO1 =V(BOOKVAL),DMCB,WORKAREA,(6,RSARBKS),SCANNER,RR=RELO            
SA0010   EQU   *                                                                
         MVC   RSARWKS,RCONWKS     LOAD NUMBER OF WEEKS                         
*                                                                               
         LA    RE,WORKAREA                                                      
         LA    RF,L'WORKAREA                                                    
         XCEF  (RE),(RF)                                                        
*                                                                               
         LA    RF,30               CALCULATE L(LENGTH INPUT)                    
         LA    RE,STLENGTH+29      A(LAST CHARACTER)                            
SA0011   EQU   *                                                                
         CLI   0(RE),C' '          POSITION = SPACE?                            
         BNE   SA0011A             NO  - COUNT FINISHED                         
         BCTR  RE,0                DECREMENT A(CHARACTER)                       
         BCT   RF,SA0011           SCAN EACH POSITION BACKWARDS                 
SA0011A  EQU   *                                                                
         STC   RF,WORKAREA+5       INSERT LENGTH                                
         MVC   WORKAREA+8(30),STLENGTH                                          
*                                                                               
         L     R5,AADDIO           US SPARE IO AREA AS SCRATCH                  
         XCEF  (R5),200                                                         
*                                                                               
         PRINT GEN                                                              
         GOTO1 SCANNER,DMCB,WORKAREA,(6,(R5))                                   
         PRINT NOGEN                                                            
*                                                                               
         L     R5,AADDIO           A(IO AREA USED AS SCRATCH)                   
         LA    R5,4(R5)            DISPLACEMENT TO FIRST ENTRY                  
         LA    R6,RSARRFRM         A(OUTPUT IN SAR ELEMENT)                     
         LA    RF,6                LOOP CONTROL                                 
SA0012   EQU   *                                                                
         MVC   0(2,R6),2(R5)       LOW-ORDER 2 BYTES OF LENGTH                  
         LA    R5,32(R5)           NEXT 'LINE' OF SCANNED DATA                  
         LA    R6,2(R6)            A(NEXT OUTPUT VALUE)                         
         BCT   RF,SA0012           GO BACK FOR NEXT                             
*                                                                               
         LA    R5,STDAYPTS         A(DAYPART CODES)                             
         LA    R6,RSARDPT          A(OUTPUT ELEMENT)                            
         LA    RF,6                LOOP CONTROL                                 
SA0014   EQU   *                                                                
         CLI   0(R5),C' '          ANY VALUE?                                   
         BE    SA0016              NO  - FINISHED                               
         MVC   0(1,R6),0(R5)       MOVE TO ELEMENT                              
         LA    R6,3(R6)            BUMP A(OUTPUT)                               
         LA    R5,1(R5)            BUMP A(INPUT)                                
         BCT   RF,SA0014           GO BACK FOR NEXT                             
SA0016   EQU   *                                                                
         SR    R6,R6               ESTABLISH WEEK BIT PATTERN                   
         SR    R7,R7                                                            
         ZIC   RF,RSARWKS          # OF WEEKS OF FLIGHT                         
SA0018   EQU   *                                                                
         SLDL  R6,1                SHIFT REGISTERS LEFT 1 POSITION              
         O     R7,=F'1'            TURN ON LOW ORDER BIT                        
         BCT   RF,SA0018           DO EACH BIT                                  
*                                                                               
*   AT THIS POINT, THE LOW-ORDER 'RSARWKS' # OF BITS HAVE BEEN TURNED           
*       ON.  IT IS NECESSARY TO SHIFT THEM TO THE HIGH-ORDER BITS OF            
*       THE REGISTER PAIR.                                                      
*                                                                               
         ZIC   RF,RSARWKS          # OF WEEKS OF FLIGHT (AGAIN)                 
         LA    RE,64               MAX NUMBER OF WEEKS                          
         SR    RE,RF               COUNT TO SHIFT LEFT                          
SA0020   EQU   *                                                                
         SLDL  R6,1                KEEP SHIFTING LEFT                           
         BCT   RE,SA0020                                                        
*                                                                               
*   THERE IS PROBABLY A MORE ELEGANT WAY TO SHIFT A VARIABLE NUMBER OF          
*      BITS, BUT IT ISN'T WORTH THE EFFORT                                      
*                                                                               
         ST    R6,FULL             SAVE WEEKS                                   
         MVC   RSARBWKS(4),FULL                                                 
         ST    R7,FULL                                                          
         MVC   RSARBWKS+4(4),FULL                                               
*                                                                               
         LA    RF,100              DEFAULT VALUE OF STATION PERCENT             
         CLC   STSTAPCT,MYSPACES   ANY VALUE IN STATION PERCENT?                
         BE    SA0022              NO  - USE DEFAULT                            
         PACK  WORKAREA(8),STSTAPCT(3)                                          
         CVB   RF,WORKAREA                                                      
SA0022   EQU   *                                                                
         STC   RF,RSARSHR          SAVE SHARE                                   
*                                                                               
*   THIS IS PROBABLY NOT THE CORRECT SHARE TO SAVE.  WHAT IS SAVED              
*      HERE IS THE 'STATION PERCENT OF BUDGET' VALUE                            
*                                                                               
         PACK  WORKAREA(8),STBUDGET(8)                                          
         CVB   RF,WORKAREA                                                      
         ST    RF,FULL             INSERT BUDGET INTO ELEMENT                   
         MVC   RSARBGT,FULL                                                     
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELAREA                     
*                                                                               
SA0099   EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO CHECK IF REVISED RECORD IS ON FILE AS A PREVIOUSLY                 
*     DELETED KEY.  IF SO, RECORD IS OVERWRITTEN, AND KEY UNDELETED.            
*     IF NOT ON FILE, REVISED RECORD IS ADDED TO FILE.                          
*                                                                               
ADDORPUT NTR1                                                                   
         OI    DMINBTS,X'08'       PASS DELETES ALSO                            
         MVI   RDUPDATE,C'Y'       RETRIEVE KEY FOR UPDATE                      
         GOTO1 HIGH                NEW KEY ON FILE?                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AORP0004            NO  - NOT ON FILE                            
         TM    KEY+27,X'80'        ON FILE - DELETED?                           
         BO    AORP0002            YES                                          
         DC    H'0'                MUST BE DELETED                              
AORP0002 EQU   *                                                                
*                                                                               
*  KEY FOUND ON FILE, AS DELETED.  MUST BE REWRITTEN AS UNDELETED,              
*    RECORD FETCHED FOR UPDATE, THEN OVERWRITTEN WITH THE REVISED               
*    RECORD.                                                                    
*                                                                               
         MVI   KEY+27,0            TURN OFF DELETE BIT                          
         GOTO1 WRITE               REWRITE KEY AS UNDELETED                     
         MVC   SAVAIO,AIO          SAVE A(REVISED CONTRACT RECORD)              
         L     R2,AIO1             RESET A(IO AREA)                             
         ST    R2,AIO                                                           
         MVI   RDUPDATE,C'Y'       INDICATE 'READ FOR UPDATE'                   
         GOTO1 GETREC              RETRIEVE TO PERMIT REWRITE                   
         L     R2,SAVAIO           RESTORE A(REVISED CONTRACT RECORD)           
         ST    R2,AIO                                                           
         GOTO1 PUTREC              OVERWRITE THE ORIGINAL RECORD                
         MVC   DISKADDR,KEY+28                                                  
         B     AORP0099                                                         
AORP0004 EQU   *                                                                
*                                                                               
*  REVISED RECORD IS NOT ON FILE AS DELETED KEY.                                
*                                                                               
         USING RCONRECD,R2                                                      
*                                                                               
         MVC   KEY(27),RCONREC     RESET KEY AFTER NOT FOUND                    
         GOTO1 ADDREC              ADD REVISED RECORD                           
         MVC   DISKADDR,KEY        SAVE DISK ADDRESS OF RECORD                  
AORP0099 EQU   *                                                                
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
* ROUTINE TO BUILD POINTER LIST IN P1                                           
*  P2 - 0=NEW POINTER                                                           
*       1=OLD POINTER                                                           
*                                                                               
PTRS     NTR1                                                                   
         L     R2,0(R1)            A(POINTER BUILD AREA)                        
         LR    RE,R2                                                            
         L     R3,4(R1)            NEW/OLD POINTER FLAG                         
         XCEF  (RE),500                                                         
         L     R4,8(R1)            ESTABLISH ADDRESS/RCONREC                    
         USING RCONRECD,R4                                                      
*                                                                               
* BUILD ACTIVE PTR                                                              
         MVI   0(R2),X'0C'                                                      
         MVC   02(02,R2),SIGNON2C                                               
         MVC   04(02,R2),RCONKGRP                                               
         MVC   06(05,R2),RCONKSTA                                               
         MVC   11(02,R2),RCONKOFF                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 1                                                                  
         MVI   0(R2),X'8C'                                                      
         MVC   21(02,R2),SIGNON2C                                               
         ZAP   DUB(5),=P'99999999'                                              
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         MVC   23(4,R2),WORK                                                    
         LA    R2,32(R2)                                                        
* CREATE PTR 2                                                                  
         MVI   0(R2),X'9C'                                                      
         MVC   02(02,R2),SIGNON2C                                               
         MVC   04(02,R2),RCONKOFF                                               
         MVC   06(02,R2),RCONKGRP                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKADV                                               
         MVC   17(04,R2),RCONKAGY                                               
         MVC   21(02,R2),RCONKAOF                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 3                                                                  
         MVI   0(R2),X'AC'                                                      
         MVC   01(2,R2),SIGNON2C                                                
         MVC   03(2,R2),RCONKOFF                                                
         MVC   05(2,R2),RCONTEM                                                 
* INVERT SALESMAN CODE FOR LAST NAME HIGH                                       
         LA    RE,RCONSAL+2        LAST INITIAL                                 
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   07(1,R2),0(RE)                                                   
         MVC   08(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '      ONLY 2 INITIALS?                             
         BNE   *+8                                                              
         MVI   9(R2),C' '                                                       
         MVC   10(5,R2),RCONKSTA                                                
         MVC   15(4,R2),RCONKAGY                                                
         MVC   19(4,R2),RCONKADV                                                
         MVC   23(4,R2),RCONKCON                                                
         LA    R2,32(R2)                                                        
* CREATE PTR 4                                                                  
         MVI   0(R2),X'BC'                                                      
         MVC   02(02,R2),SIGNON2C                                               
         MVC   04(02,R2),RCONCTGY                                               
         MVC   06(02,R2),RCONKOFF                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 5                                                                  
         MVI   0(R2),X'CC'                                                      
         MVC   01(02,R2),SIGNON2C                                               
         MVC   03(05,R2),RCONKSTA                                               
         MVC   08(02,R2),RCONKOFF                                               
         MVC   10(02,R2),RCONTEM                                                
* INVERT SALESMAN                                                               
         LA    RE,RCONSAL+2                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   12(1,R2),0(RE)                                                   
         MVC   13(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '                                                   
         BNE   *+8                                                              
         MVI   14(R2),C' '                                                      
*                                                                               
         MVC   15(04,R2),RCONKADV                                               
         MVC   19(04,R2),RCONKAGY                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
*                                                                               
*   IN THIS VERSION, THERE IS NO PROVISION FOR BOP.  FOLLOWING CODE             
*   IS THEREFORE DEACTIVATED.                                                   
*                                                                               
*        LA    R6,RCONREC                                                       
*        GOTO1 GETELEM,DMCB,X'00000010'                                         
*        BNE   PTR00030            NO BOP                                       
*                                                                               
*        USING RCONBPEL,R6                                                      
*        MVI   0(R2),X'DC'         CREATE BOP POINTER FOR CHANGE                
*        MVC   5(2,R2),SIGNON2C                                                 
*        MVC   7(4,R2),RCONKADV                                                 
*        MVC   11(3,R2),TODAY      NEW POINTER GETS TODAYS DATE                 
*                                                                               
*        MVC   14(4,R2),RCONBPRF                                                
*        MVC   18(5,R2),RCONKSTA                                                
*        MVC   23(4,R2),RCONKCON                                                
*        LA    R2,32(R2)                                                        
*                                                                               
PTR00030 EQU   *                                                                
         LA    R6,RCONREC                                                       
         GOTO1 GETELEM,DMCB,X'00000012'                                         
         BNE   XIT                 NO SAR                                       
*                                                                               
         USING RSAREL,R6                                                        
         MVI   0(R2),X'EC'                                                      
         MVC   21(2,R2),SIGNON2C                                                
         MVC   23(4,R2),SAV9COMP                                                
         B     PTR00099                                                         
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
PTR00099 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE TO ADD POINTERS TO FILE                                               
*              P1=A(OLD PTR LIST)  - NOT NEEDED IN THIS ROUTINE                 
*              P2=A(NEW PTR LIST)                                               
*              P3=A(DISK ADDR)                                                  
ADDPTRS  NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         B     AP0004              SKIP MASTER KEY                              
AP0002   EQU   *                                                                
         MVC   KEY,0(R3)           ADD NEW KEY                                  
         MVI   KEY+27,0            SET CONTROL                                  
         MVC   KEY+28(4),0(R4)     INSERT DISK ADDR                             
         GOTO1 ADD                                                              
         BAS   RE,AP0006                                                        
*                                                                               
AP0004   LA    R3,32(R3)           BUMP TO NEXT KEY                             
         CLI   0(R3),0             LAST?                                        
         BNE   AP0002                                                           
         B     XIT                                                              
*                                                                               
AP0006   TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* ROUTINE TO MODIFY POINTERS ON FILE DURING AN UPDATE                           
*              P1=A(OLD PTR LIST)                                               
*              P2=A(NEW PTR LIST)                                               
*              P3=A(DISK ADDR)                                                  
MODPTRS  NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         CLC   0(27,R2),0(R3)      OLD MASTER = NEW MASTER?                     
         BE    MP0020                                                           
*                                                                               
*   OLD MASTER NOT SAME AS NEW MASTER:  NEW MASTER KEY PREVIOUSLY               
*     ADDED BY 'ADDREC' ROUTINE.  OLD MASTER MUST BE DELETED.                   
*                                                                               
         MVI   RDUPDATE,C'Y'       SET 'READ FOR UPDATE'                        
         MVC   KEY(27),0(R2)       RETRIEVE OLD MASTER KEY                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BE    MP0001A                                                          
         DC    H'0'                                                             
MP0001A  EQU   *                                                                
         OI    KEY+27,X'80'        SET DELETE                                   
         GOTO1 WRITE               REWRITE MASTER KEY AS DELETED                
         B     MP0020              BUMP TO NEXT SET OF KEYS                     
MP0002   EQU   *                                                                
         CLC   0(27,R2),0(R3)      OLD PASSIVE = NEW PASSIVE?                   
         BNE   MP0004              NO                                           
*                                                                               
*  PASSIVE KEYS EQUAL:  INSERT POSSIBLE NEW DISK ADDRESS, REWRITE               
*                                                                               
         MVC   KEY(27),0(R3)                                                    
         MVI   RDUPDATE,C'Y'       SET 'READ FOR UPDATE'                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BE    MP0002A             YES                                          
         DC    H'0'                NO                                           
MP0002A  EQU   *                                                                
         MVC   KEY+28(4),0(R4)     INSERT NEW DISK ADDRESS                      
         MVI   KEY+27,0            RESET CONTROL                                
         GOTO1 WRITE               REWRITE KEY                                  
         B     MP0020              BUMP TO NEXT SET OF KEYS                     
MP0004   EQU   *                                                                
*                                                                               
*  PASSIVE KEYS NOT EQUAL:  CHECK FOR OLD KEY FIRST.  IF PRESENT                
*     DELETE IT.                                                                
*                                                                               
         CLI   0(R2),0             ANY KEY?                                     
         BE    MP0006              NO                                           
         MVC   KEY(27),0(R2)                                                    
         MVI   RDUPDATE,C'Y'       SET 'READ FOR UPDATE'                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BE    MP0004A             YES                                          
         DC    H'0'                NO                                           
MP0004A  EQU   *                                                                
         OI    KEY+27,X'80'        SET DELETE BIT                               
         GOTO1 WRITE               REWRITE AS DELETED                           
MP0006   EQU   *                                                                
*                                                                               
*  CHECK NEW KEY.                                                               
*                                                                               
         CLI   0(R3),0                                                          
         BE    MP0020                                                           
         MVC   KEY(27),0(R3)       SET NEW KEY FOR READ                         
         OI    DMINBTS,X'08'       PASS DELETES ALSO                            
         MVI   RDUPDATE,C'Y'       SET 'READ FOR UPDATE'                        
         GOTO1 HIGH                CHECK FOR KEY                                
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BNE   MP0010              NO                                           
*                                                                               
*  KEY FOUND ON FILE MUST BE DELETED.  IF IT IS, IT IS UNDELETED,               
*    A POSSIBLY DIFFERENT DISK ADDRESS INSERTED, AND THEN THE                   
*    KEY IS REWRITTEN                                                           
*                                                                               
         TM    KEY+27,X'80'        DELETED?                                     
         BO    MP0008              YES                                          
         DC    H'0'                MUST BE DELETED                              
MP0008   EQU   *                                                                
         MVI   KEY+27,0            UNDELETE KEY                                 
         MVC   KEY+28(4),0(R4)     SET DISK ADDRESS                             
         GOTO1 WRITE               REWRITE KEY AS UNDELETED                     
         B     MP0020                                                           
MP0010   EQU   *                                                                
*                                                                               
*  KEY WAS NOT FOUND ON FILE.  THE KEY MUST BE RESET, AND THEN                  
*    ADDED TO THE DIRECTORY                                                     
*                                                                               
         MVC   KEY(27),0(R3)       RESET KEY AFTER NOT FOUND                    
         MVI   KEY+27,0            SET CONTROL                                  
         MVC   KEY+28(4),0(R4)     SET DISK ADDRESS                             
         GOTO1 ADD                                                              
         BAS   RE,MP0024           CHECK RESULT                                 
*                                                                               
MP0020   LA    R2,32(R2)                                                        
         LA    R3,32(R3)                                                        
         CLI   0(R2),0             ANY MORE OLD KEYS?                           
         BNE   MP0002              YES                                          
         CLI   0(R3),0             ANY MORE NEW KEYS?                           
         BNE   MP0002              YES                                          
         B     XIT                 NO  - KEY UPDATING FINISHED                  
*                                                                               
MP0024   TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  LOCAL WORK AREA/CONSTANTS, ETC                                               
COMP9    DC    C'9876543210'                                                    
         DS    0F                                                               
SAVR1    DS    F                                                                
SAVR2    DS    F                                                                
SAVR3    DS    F                                                                
SAVAIO   DS    F                                                                
RELO     DS    A                                                                
MYSPACES DC    CL20'                    '                                       
DMCB2    DS    6F                  ALTERNATE PARAMETER LIST                     
CONUPDT  DS    XL1                 CONTRACT UPDATE/ADDITION FLAG                
*                                  0   =   CONTRACT ADDITION                    
*                                  1   =   CONTRACT UPDATE                      
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
* CT09IDSECT:  REQUEST FOR ADDITION OF CONTRACT TO MAINFRAME                    
       ++INCLUDE CT09IDSECT                                                     
         EJECT                                                                  
* CT09IDSCT2:  REQUEST FOR CONTRACT FROM MAINFRAME                              
       ++INCLUDE CT09IDSCT2                                                     
         EJECT                                                                  
* CT09IDSCT3:  SAR INFORMATION FOR ADDITION ON MAINFRAME                        
       ++INCLUDE CT09IDSCT3                                                     
         EJECT                                                                  
* CT09ODSECT:  CONTRACT PROCESSED                                               
       ++INCLUDE CT09ODSECT                                                     
         EJECT                                                                  
* CT09ODSCT2:  RETURN OF CONTRACT FROM MAINFRAME                                
       ++INCLUDE CT09ODSCT2                                                     
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
         DS    0D                  DOUBLE-WORD ALIGNMENT                        
PACKWORK DS    XL8                 WORK-AREA FOR PACK                           
DISKADDR DS    F                   DISK ADDRESS OF NEW RECORD                   
INFOSTOR EQU   *                   STORAGE                                      
ERRFLAG  DS    CL1                 ERROR HAS OCCURRED:  NON-X'00'               
ELCODE   DS    CL1                 ELEMENT CODE ARGUMENT                        
COMMTYPE DS    CL1                 COMMENT TYPE STORAGE                         
MONDATE  DS    CL2                 MONDAY'S DATE                                
TODAY    DS    CL3                 TODAY'S DATE                                 
SAVECTG  DS    CL2                 SAVE PRODUCT CATEGORY CODE                   
SAVSTCDT DS    CL2                 SAVE STATION CLOSE DATE                      
SAVSTAST DS    CL1                 SAVE STATION STATUS                          
SAV9COMP DS    CL4                 CONTRACT # 9'S COMPLEMENT                    
SAVDATES DS    CL6                 FLIGHT DATE STORAGE                          
SAVPROD  DS    CL20                PRODUCT EXPANSION FOR X'05' ELT              
TEMPAREA DS    50C                 TEMPORARY WORK AREA                          
         DS    0D                  ALIGNMENT                                    
WORKAREA DS    50C                 TEMPORARY WORK AREA                          
LENCRWRK DS    F                   LENGTH OF VARIABLE LENGTH RECORD             
DATVAL   DS    A                   A(DATE VALIDATION ROUTINE)                   
INFOLEN  EQU   *-INFOSTOR                                                       
ABLDAREA DS    A                   A(CURRENT LOCATION IN BLDAREA)               
ELAREA   DS    160C                ELEMENT CONSTRUCT AREA                       
BLDAREA  DS    1600C               BUILD AREA FOR FRAME                         
CONTAREA DS    1024C               CONTRACT BUILD AREA                          
         EJECT                                                                  
*                                                                               
*  REGENCON:  CONTRACT RECORD LAYOUT:  DSECT                                    
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
*                                                                               
*  REGENSTA:  STATION RECORD LAYOUT:  DSECT                                     
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
*                                                                               
*  REGENOFF:  OFFICE RECORD LAYOUT:  DSECT                                      
ROFFRECD DSECT                                                                  
       ++INCLUDE REGENOFF                                                       
*                                                                               
*  REGENSAL:  SALESMAN RECORD LAYOUT:  DSECT                                    
RSALRECD DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
*                                                                               
*  REGENAGY:  AGENCY RECORD LAYOUT:  DSECT                                      
RAGYRECD DSECT                                                                  
       ++INCLUDE REGENAGY                                                       
*                                                                               
*  REGENADV:  ADVERTISER RECORD LAYOUT:  DSECT                                  
RADVRECD DSECT                                                                  
       ++INCLUDE REGENADV                                                       
*                                                                               
*  REGENPRD:  PRODUCT RECORD LAYOUT:  DSECT                                     
RPRDRECD DSECT                                                                  
       ++INCLUDE REGENPRD                                                       
*                                                                               
*  REGENCTG:  CATEGORY RECORD LAYOUT:  DSECT                                    
RCTGRECD DSECT                                                                  
       ++INCLUDE REGENCTG                                                       
*                                                                               
*  DDCOMFACS: ADDRESS ROUTINE DSECT                                             
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'192CTMAD09   05/01/02'                                      
         END                                                                    
