*          DATA SET REINV00    AT LEVEL 016 AS OF 10/04/12                      
*PHASE T80300A                                                                  
*INCLUDE GETBROAD                                                               
         TITLE 'REINV00 (T80300) --- REPPAK MULTIPLE INVOICE/BASE'              
*                                                                               
**********************************************************************          
*                                                                    *          
*- REINV00 (T80300) --- REPPAK MULTIPLE INVOICE CONTROLLER           *          
*                                                                    *          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*  08/24/89  PJS  CHANGE PHASE TO 'A' LEVEL                          *          
*                                                                    *          
*  10/20/89  PJS  INTEREP CHANGES                                    *          
*                 - NEW ACTION ('I'), MOVE ORDERED AMOUNTS TO INVOICE*          
*                                                                    *          
*  10/25/89  PJS  READ REP RECORD AND SAVE PROFILES                  *          
*                                                                    *          
*  10/30/89  PJS  ADDED TYPE FILTER FIELD TO SCREEN.                 *          
*                                                                    *          
*  11/02/89  PJS  ADDED LINE-ITEM ORDER TO INVOICE.                  *          
*                 (PUT 'I' IN INVOICE $ FIELD)                       *          
*                                                                    *          
*  04OCT90   EFJ  FIXED BUG CAUSING NEGATIVE NUMBER TO BE POSITIVE   *          
*                 WHEN ACTION 'I' USED  (WAS ONLY USING 10 SPACES,   *          
*                 IGNORING POSSIBILITY OF '-' AT END)                *          
*                                                                    *          
* MAY16/91 (MRR) --->CHANGE 'CLOSE DATE=' TO 'CLOSED THRU' TEXT ON   *          
*                     THE HEADER MESSAGE                             *          
*                   >TEST CLOSE MONTH TO BE > MONTH ALREADY ON FILE  *          
*                                                                    *          
* MAY28/91 (MRR) --->FIX 'I' (COPY ORDERED $) FUNCTION ON INDIVID    *          
*                    LINES WHEN USED WITH 'CHA' ACTION               *          
*                                                                    *          
* JUN01/92 (BU ) --->CHANGE EDIT PICTURE IF $ 1MEG OR >              *          
*                                                                    *          
* OCT07/93 (BU ) --->FORCE EDIT OF OFFICE/CONTRACT TYPE WHEN STATION *          
*                    CHANGES.                                        *          
*                                                                    *          
* AUG16/94 (SKU) --- REP PROFILE DOUBLE LINE DISPLAY SUPPORT         *          
*                                                                    *          
* FEB16/95 (BU ) --- PRODUCE 'TOTALS ONLY'                           *          
*                                                                    *          
* MAY24/96   SMP  NEW DIFFERENCE COLUMN                              *          
*                                                                    *          
* OCT03/96   SEP  ALLOW LOW POWER TV STATION ENTRY                   *          
*                                                                    *          
* JAN08/97 (BU )  REMOVE DDS-ONLY FLAG(S)                            *          
*                                                                    *          
* MAY14/97 (BU )  FIX 'I' ACTION WHEN $ > 99,999.99.                 *          
*                                                                    *          
* JUN05/97 (BU )  ADD ACTIVITY ELEMENT TO STATION RECORD             *          
*                                                                    *          
* JUN05/97 (RHV)  ADD PF2 SWAP TO CONTRACT                           *          
*                                                                    *          
* SEP24/97 (BU )  ALTERNATE INVOICE BUCKET CREATE                    *          
*                                                                    *          
* 25FEB/99 (RHV)  APPLICATION SECURITY                               *          
*                                                                    *          
* 03MAR/99 (RHV)  AGENCY FILTER                                      *          
*                                                                    *          
* 05MAR/99 (RHV)  SCROLLING                                          *          
*                                                                    *          
* 17MAR/00 (RHV)  RELEASE LOCKED RECS DURING PAGE JUMPING            *          
*                                                                    *          
* 19MAY/00 (BU )  ADD TRADE PROCESSING                               *          
*                                                                    *          
* 31MAY/00 (RHV)  REMOVE OLD PGM SECURITY                            *          
*                                                                    *          
* 28JUN/00 (BU )  REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG      *          
*                                                                    *          
* 23JAN/02 (HQ )  FIX A BUG WHERE INVOICE FIELD(LAST LINE) GOT       *          
*                 PROTECTED AFTER COME BACK FROM TOTAL SCREEN        *          
*                                                                    *          
*                    ***  END TOMBSTONE  ***                         *          
**********************************************************************          
*                                                                               
         PRINT NOGEN                                                            
T80300   CSECT                                                                  
         NMOD1 800,T80300,R9,RR=R5,CLEAR=YES                                    
         USING GENOLD,RC                                                        
         USING T803FFD,RA                                                       
         L     RE,=V(GETBROAD)                                                  
         AR    RE,R5               RELOCATE GETBROAD                            
         ST    RE,VGTBROAD                                                      
         L     RE,0(R1)                                                         
         ST    RE,ATIO             A(TIO)                                       
         L     RE,16(R1)                                                        
         ST    RE,ACOMFACS         A(COMFACS)                                   
         BAS   RE,INITL                                                         
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000AAC',0                                     
         MVC   VREPFACS,0(R1)      EXTERNAL ROUTINE, MISC. SUBROUTINES          
*                                                                               
*- READ IN REP RECORD AND SAVE PROFILES                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           KEY ID                                       
         MVC   KEY+25(2),REPALPHA  POWER CODE                                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                REP CODE NOT ON FILE?                        
         LA    RF,RREPREC                                                       
         ST    RF,AIOAREA                                                       
         GOTO1 GETREC                                                           
         MVC   SVREPROF(30),RREPPROF   SAVE ALL 30 PROFILES                     
         EJECT                                                                  
*                                                                               
         MVI   TWAPFKEY,0                                                       
         BAS   RE,CKGLOB           CHECK INCOMING FROM GLOBBER                  
         BE    *+8                 YES - SKIP PFKEY CHECK                       
         BAS   RE,HOTKEY           CHECK OUTGOING VIA PFKEY                     
*                                                                               
* EDIT STATION                                                                  
M05      DS    0H                                                               
         TM    INVTYPEH+4,X'20'    CONTRACT TYPE CHANGED?                       
         BNO   M050020             YES - CONSIDER AS STATION CHANGE             
*                                                                               
*   CONTRACT TYPE CHANGE MUST TRIGGER A RESTART.  A STATION CHANGE              
*        IS ALWAYS CONSIDERED A RESTART.                                        
*                                                                               
         TM    INVSTAH+4,X'20'     STATION CHANGED?                             
         BO    M100                NO                                           
M050020  DS    0H                                                               
         NI    INVOFFH+4,X'DF'     YES - FORCE EDIT OF OFFICE                   
         NI    INVTYPEH+4,X'DF'          FORCE EDIT OF CONTRACT TYPE            
         XC    TWAKEY,TWAKEY                                                    
         XC    TWACLIST,TWACLIST   LIST OF DISK ADDRESSES & INVOICE AMT         
         LA    R2,INVSTAH                                                       
         LA    R3,STAERR                                                        
         MVI   TWADISP,0           NO CONTRACTS DISPLAYED                       
         MVI   TWAPAGE,0                                                        
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         FOUT  INVMKTH,SPACES,20                                                
         FOUT  INVCLSH,SPACES,20                                                
         BAS   RE,MOVE                                                          
*                                                                               
         CLI   WORK+3,C' '                                                      
         BE    M10                                                              
         CLI   WORK+4,C' '                                                      
         BE    M10                                                              
*                                                                               
         CLI   WORK+3,C'-'                                                      
         BNE   *+12                                                             
         MVI   WORK+3,C' '                                                      
         B     M10                                                              
         CLI   WORK+4,C'-'                                                      
         BNE   ERROR                                                            
         MVC   WORK+4(1),WORK+5                                                 
*                                                                               
M10      CLI   WORK+4,C'T'                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C' '                                                      
         MVC   RSTAKSTA,WORK                                                    
         MVC   KEY,RSTAKEY                                                      
         BAS   RE,HIGH                                                          
*                                                                               
         CLC   KEY(27),KEYSAVE     VALID STATION?                               
         BNE   ERROR                                                            
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         MVI   TWAKEY,X'0C'                                                     
         MVC   TWAKEY+2(2),REPALPHA                                             
         MVC   TWAKEY+4(2),RSTAGRUP                                             
         MVC   TWAKEY+6(5),RSTAKSTA                                             
         OI    INVSTAH+4,X'20'                                                  
         MVC   INVMKT,RSTAMKT                                                   
         OC    RSTACLDT,RSTACLDT   CLOSE DATE FOR INVOICES?                     
         BZ    M30                                                              
         MVC   INVCLS(11),=C'CLOSED THRU'                                       
         GOTO1 VDATCON,DMCB,(3,RSTACLDT),(6,INVCLS+12)                          
M30      MVC   INVSTA+4(3),=C'-TV'                                              
         CLI   RSTAKSTA+4,C' '                                                  
         BE    M50                                                              
         MVC   INVSTA+4(3),=C'-L '                                              
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    M50                                                              
* NOT TV                                                                        
         MVI   INVSTA+6,C'M'                                                    
         MVC   INVSTA+5(1),RSTAKSTA+4                                           
M50      FOUT  INVSTAH                                                          
         CLI   RSTAKSTA+3,C' '                                                  
         BNE   M100                                                             
         MVC   INVSTA+3(3),INVSTA+4                                             
         MVI   INVSTA+6,C' '                                                    
         EJECT                                                                  
* EDIT MONTH                                                                    
M100     TM    INVMONH+4,X'20'                                                  
         BO    M125                PREVIOUSELY VALID ?                          
         XC    INVNETT,INVNETT     NO  - CLEAR THE 'NET' FIELD                  
         LA    RF,INVNETTH                                                      
         MVI   5(RF),0             CLEAR LENGTH OF INPUT                        
         FOUT  INVNETTH                                                         
         MVI   TWADISP,0           NO CONTRACTS DISPLAYED                       
         MVI   TWAPAGE,0                                                        
         XC    TWACLIST,TWACLIST                                                
         LA    R2,INVMONH                                                       
         LA    R3,MONERR                                                        
*                                                                               
         GOTO1 VDATVAL,DMCB,(2,INVMON),DUB                                      
*                                                                               
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*                                                                               
         GOTO1 VDATCON,(R1),DUB,(3,FULL)                                        
         MVC   TWAMON,FULL                                                      
         OI    INVMONH+4,X'20'                                                  
         EJECT                                                                  
*********************************************************                       
*                                                                               
*                                  ROUTINE TO BAL                               
*                                    TO TEST NET FIELD                          
*                                       THEN                                    
*                                    CHECK FOR ERROR                            
*                                       AND                                     
*                                    WHEN NOT ERROR RTN  150                    
*                                                                               
**                                                                              
M125     EQU   *                                                                
         BAS   RE,NETRTN           BAL  NET ROUTINE                             
         BZ    M150                RETURN NO ERROR                              
         LA    R2,INVNETTH         CC NE TO ZERO... AN ERROR                    
         LA    R3,NETVALER         ADDR ERROR MESSAGE                           
         B     ERROR               GO TO ERROR MESSAGE DISPLAY                  
***********************************************************JR*                  
         EJECT                                                                  
* EDIT OFFICE                                                                   
M150     TM    INVOFFH+4,X'20'     PREVIOUSLY VALIDATED                         
         BO    M195                                                             
*                                                                               
         MVI   TWADISP,0           START EVERYTHING OVER                        
         MVI   TWAPAGE,0                                                        
         XC    TWACLIST,TWACLIST                                                
*                                                                               
         LA    R2,INVOFFH                                                       
         FOUT  INVOFFNH,SPACES,10  SPACES TO OFFICE NAME                        
         CLI   5(R2),0             OFFICE IS OPTIONAL FILTER                    
         BE    M190                                                             
         CLC   INVACT(3),=C'CLO'      BUT CAN'T BE USED                         
         BNE   *+12                WITH ACTION CLOSE                            
         LA    R3,107              INVALID FILTER FOR THIS ACTION               
         B     ERROR                                                            
*                                                                               
         LA    R3,151              INVALID OFFICE                               
         XC    ROFFKEY,ROFFKEY                                                  
         MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,REPALPHA                                                
         BAS   RE,MOVE                                                          
         MVC   ROFFKOFF,WORK                                                    
         MVC   KEY,ROFFKEY                                                      
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE     VALID OFFICE                                 
         BNE   ERROR                                                            
*                                                                               
         BAS   RE,GETREC                                                        
         MVC   TWAKEY+11(2),ROFFKOFF                                            
         MVC   INVOFFN,ROFFNAME                                                 
         FOUT  INVOFFNH                                                         
M190     OI    INVOFFH+4,X'20'                                                  
         SPACE 2                                                                
*                                                                               
*- EDIT OPTIONAL TYPE FIELD                                                     
*                                                                               
*  'X'  = 'ONLY SHOW TYPE X'  (X NOT = '*')                                     
*  '*X' = 'EXCLUDE TYPE X'    (X NOT = '*')                                     
*                                                                               
M195     EQU   *                                                                
         CLC   =C'TON',INVACT      'TOTALS ONLY/ALTERNATE' REQUEST?             
         BNE   M195A               YES                                          
         MVC   INVACT,=C'DIN'      YES - SET TO 'DISPLAY ALTERNATE'             
         B     M195H                                                            
M195A    EQU   *                                                                
         CLC   =C'TOT',INVACT      'TOTALS ONLY' REQUEST?                       
         BNE   M195B               NO                                           
         MVC   INVACT,=C'DIS'      YES - SET TO 'DISPLAY'                       
         B     M195H                                                            
M195B    EQU   *                                                                
         CLC   =C'TTR',INVACT      'TOTALS ONLY/TRADE' REQUEST?                 
         BNE   M195C               YES                                          
         MVC   INVACT,=C'DIT'      YES - SET TO 'DISPLAY TRADE'                 
         B     M195H                                                            
M195C    EQU   *                                                                
         CLC   =C'TTA',INVACT      'TOTALS ONLY/TRADE/ALT' REQUEST?             
         BNE   M196                NO                                           
         MVC   INVACT,=C'DTA'      YES - SET TO 'DISPLAY TRADE/ALT'             
         B     M195H                                                            
M195H    EQU   *                                                                
         MVI   TOTREQ,C'Y'         SET 'TOTALS ONLY' REQUEST FLAG               
M196     EQU   *                                                                
         TM    INVTYPEH+4,X'20'    PREVIOUSLY VALID?                            
         BNZ   M200                                                             
*                                                                               
         CLC   =C'DIS',INVACT      ONLY CHECK ON DISPLAY                        
         BE    M196A                                                            
         CLC   =C'DIN',INVACT      ONLY CHECK ON DISPLAY/ALTERNATE              
         BE    M196A                                                            
         CLC   =C'DIT',INVACT      ONLY CHECK ON DISPLAY/TRADE                  
         BE    M196A                                                            
         CLC   =C'DTA',INVACT      ONLY CHECK ON DISPLAY/TRADE/ALT              
         BNE   M200                                                             
M196A    EQU   *                                                                
*                                                                               
         LA    R2,INVTYPEH                                                      
         LA    R3,2                INVALID INPUT                                
*                                                                               
         CLI   5(R2),0             OK IF NO INPUT                               
         BE    M199                                                             
*                                                                               
         CLI   5(R2),1                                                          
         BNE   M197                                                             
         CLI   INVTYPE,C'*'        TYPE CAN'T BE '*'                            
         BE    ERROR                                                            
         B     M199                                                             
*                                                                               
M197     CLI   INVTYPE,C'*'        EXCLUDE MUST START WITH '*'                  
         BNE   ERROR                                                            
         CLI   INVTYPE+1,C'*'      TYPE CAN'T BE '*'                            
         BE    ERROR                                                            
*                                                                               
M199     OI    INVTYPEH+4,X'20'    TYPE FIELD IS VALID                          
         EJECT                                                                  
*                                                                               
M200     DS    0H                  EDIT AGY FILTER                              
         TM    INVAGYH+4,X'20'     PREVIOUSLY VALIDATED                         
         BO    M300                                                             
*                                                                               
         MVI   TWADISP,0           START EVERYTHING OVER                        
         MVI   TWAPAGE,0                                                        
         XC    TWACLIST,TWACLIST                                                
         XC    TWAAGYF,TWAAGYF                                                  
*                                                                               
         LA    R2,INVAGYH                                                       
         CLI   5(R2),0             AGY IS OPTIONAL FILTER                       
         BE    M290                                                             
         CLC   INVACT(3),=C'CLO'      BUT CAN'T BE USED                         
         BNE   *+12                WITH ACTION CLOSE                            
         LA    R3,107              INVALID FILTER FOR THIS ACTION               
         B     ERROR                                                            
*                                                                               
         LA    R3,152              INVALID AGY                                  
         XC    DUB,DUB             BUILD RFBLOCK                                
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTOX (RFVALAGY,VREPFACS),DMCB,(R2),RAGYREC,0,DUB                      
         BNE   ERROR                                                            
         MVC   TWAKEY+13(6),RAGYKAGY                                            
         MVC   TWAAGYF,RAGYKAGY                                                 
M290     OI    INVAGYH+4,X'20'                                                  
         SPACE 2                                                                
*                                                                               
* ********************************************************************          
* PROGRAMMER NOTE: THIS PROGRAM USES APPLICATION SECURITY                       
*                  ALL VALID ACTIONS MUST BE DEFINED IN 'SECTAB' BELOW          
* ********************************************************************          
* EDIT ACTION                                                                   
M300     LA    R2,INVACTH                                                       
         LA    R3,CHGERR                                                        
*                                                                               
*- CHECK FOR SPECIAL ACTION TO COPY ORDERED TO INVOICED                         
*  (CALL COPY RTN AND TREAT AS CHANGE)                                          
         CLC   INVACT(1),=C'I'                                                  
         BNE   M300A                                                            
*                                                                               
         GOTO1 ORD2INV,DMCB,0      GLOBAL COPY                                  
         MVC   INVACT(3),=C'CHA'   TREAT AS CHANGE                              
         B     M301                                                             
*                                                                               
M300A    EQU   *                                                                
         CLC   INVACT(1),=C'N'     GLOBAL COPY (ALTERNATE)?                     
         BNE   M300B                                                            
*                                                                               
         GOTO1 ORD2INV,DMCB,0      GLOBAL COPY                                  
         MVC   INVACT(3),=C'CHN'   TREAT AS CHANGE/ALTERNATE                    
         B     M301                                                             
M300B    EQU   *                                                                
         CLC   INVACT(1),=C'T'     GLOBAL COPY (TRADE)?                         
         BNE   M300C                                                            
*                                                                               
         GOTO1 ORD2INV,DMCB,0      GLOBAL COPY                                  
         MVC   INVACT(3),=C'CTR'   TREAT AS CHANGE/TRADE                        
         B     M301                                                             
*                                                                               
M300C    EQU   *                                                                
         CLC   INVACT(1),=C'A'     GLOBAL COPY (TRADE/ALTERNATE)                
         BNE   M301                                                             
*                                                                               
         GOTO1 ORD2INV,DMCB,0      GLOBAL COPY                                  
         MVC   INVACT(3),=C'CTA'   TREAT AS CHANGE/TRADE/ALTERNATE              
         B     M301                                                             
*                                                                               
M301     EQU   *                                                                
         CLC   INVACT(3),=C'CHA'   CHANGE REGULAR                               
         BE    M301A                                                            
         CLC   INVACT(3),=C'CHN'   CHANGE ALTERNATE                             
         BE    M301A                                                            
         CLC   INVACT(3),=C'CTR'   CHANGE TRADE                                 
         BE    M301A                                                            
         CLC   INVACT(3),=C'CTA'   CHANGE TRADE/ALTERNATE                       
         BNE   M305                                                             
M301A    EQU   *                                                                
         TM    INVPAGEH+4,X'20'                                                 
         BZ    ERROR                                                            
         TM    INVTYPEH+4,X'20'    TYPE FIELD PREVALID?                         
         BZ    ERROR                                                            
         CLI   TWADISP,X'FF'       VALID CONTRACT DISPLAY?                      
         BNE   ERROR                                                            
         SPACE 2                                                                
* TEST INVOICES ALLOWED YET FOR THIS MONTH                                      
         BAS   RE,GETAMON          GET THIS ACCOUNTING MONTH                    
         BZ    M301B               NO ERROR RETURN                              
         L     R3,DUB              ERROR MESSAGE                                
         B     LOCALERR                                                         
M301B    EQU   *                                                                
         CLC   TWAMON(2),WORK      COMPARE YEAR/MONTH                           
         BL    M302                                                             
***>>>   CLI   1(RA),C'*'          DDS TERMINALS CAN DO ANYTHING!               
***>>>   BE    M302                                                             
         LA    R2,INVMONH                                                       
         LA    R3,231              INVALID INVOICE MONTH                        
         B     ERROR                                                            
*                                                                               
*- CHECK FOR LINE-ITEM ORDER TO INVOICE COPY                                    
M302     EQU   *                                                                
         GOTO1 ORD2INV,DMCB,1      SELECTIVE COPY                               
*                                                                               
         LA    R2,INVPAGEH         POINT TO PAGE FIELD                          
         BAS   RE,PACK             RESTORE PAGE NUMBER IN CASE IT WAS           
         STC   R0,TWAPAGE          WIPED OUT BY TOTALS                          
* CALL CHANGE OVERLAY                                                           
         GOTO1 VCALLOV,DMCB,(X'10',0),(RA)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             A(ROUTINE)                                   
         GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
         LA    R2,INVPAGEH         POINT TO PAGE FIELD                          
         OI    1(R2),X'01'    TURN ON MODIFIED BIT FOR AUTO PAGING              
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         LA    R2,INVACTH          POINT BACK TO ACTION FIELD                   
*                                                                               
         B     END                                                              
*                                                                               
M305     CLC   INVACT(3),=C'CLO'   CLOSE ACTION?                                
         BNE   M310                                                             
* TEST CLOSE FOR THIS MONTH                                                     
*                                                                               
         BAS   RE,GETAMON          GET THIS ACCOUNTING MONTH                    
         BZ    M306A               NO ERROR RETURN                              
         L     R3,DUB              ERROR MESSAGE                                
         B     LOCALERR                                                         
M306A    EQU   *                                                                
         LA    R2,INVMONH                                                       
         CLC   TWAMON(2),WORK                                                   
         BL    M307                                                             
**>>     CLI   1(RA),C'*'          DDS TERMINALS CAN DO ANYTHING!               
**>>     BE    M307                                                             
         LA    R3,231                                                           
         B     ERROR                                                            
* EDIT CLOSE AND UPDATE STATION RECORD WITH LATEST CLOSE DATE                   
M307     XC    RSTAKEY(27),RSTAKEY                                              
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,TWAKEY+6                                                
         MVC   KEY,RSTAKEY                                                      
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETREC                                                        
         CLI   1(RA),C'*'          DDS TERMINALS BY-PASS TEST                   
         BE    M308                                                             
         CLC   RSTACLDT,TWAMON     CAN'T CLOSE MONTH < ALREADY ON FILE          
         BL    M308                                                             
         LA    R3,220              MSG='MONTH ALREADY REPORTED'                 
         B     ERROR                                                            
M308     EQU   *                                                                
         L     R7,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,R7                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         LA    R0,WORK2            SAVE RETURNED FAFACTS AREA                   
         L     RE,0(R1)                                                         
         LHI   R1,L'WORK2                                                       
         LHI   RF,L'WORK2                                                       
         MVCL  R0,RE                                                            
         DROP  R7                                                               
*                                                                               
         LA    RF,WORK2                                                         
         USING FACTSD,RF                                                        
         MVC   WORK+40(8),FASYM    GET LUID                                     
         DROP  RF                                                               
*                                                                               
         MVC   RSTACLDT,TWAMON     UPDATE STATION CLOSE DATE                    
         MVI   ELCODE,X'23'                                                     
         LA    R6,RSTAREC                                                       
         BAS   RE,GETEL                                                         
         BNZ   M309                NOT FOUND:  ADD NEW ELEMENT                  
*                                  FOUND:  INSERT TODAY'S DATE                  
         GOTO1 VDATCON,DMCB,(5,WORK),(3,WORK)                                   
         MVC   2(3,R6),WORK        INSERT TODAY'S DATE INTO ELEMENT             
         MVI   5(R6),X'40'         TURN FLAG TO 'INVOICE'                       
         MVC   7(8,R6),WORK+40     INSERT USER LUID                             
         B     M309C                                                            
M309     EQU   *                                                                
         GOTO1 VDATCON,DMCB,(5,WORK),(3,NEW23ELT+2)                             
         MVC   NEW23ELT+7(8),WORK+40                                            
*                                  INSERT USER LUID                             
         LA    R3,RSTAELEM         SET A(1ST ELEMENT)                           
M309A    EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         CLI   0(R3),0             END OF RECORD REACHED?                       
         BE    M309B               YES - INSERT HERE                            
         CLI   0(R3),X'23'         FIND NEXT HIGHER ELEMENT                     
         BL    M309A               GO BACK FOR NEXT                             
*                                  HIGHER:  NO OTHER 23'S IN RECORD             
M309B    EQU   *                                                                
         PRINT GEN                                                              
         GOTO1 VRECUP,DMCB,(2,RSTAREC),NEW23ELT,(C'R',(R3))                     
         PRINT NOGEN                                                            
*                                                                               
         CLI   DMCB+8,C'R'         GOOD RETURN FROM RECUP?                      
         BE    M309C               YES.                                         
         DC    H'0'                                                             
*                                                                               
M309C    EQU   *                                                                
         BAS   RE,PUTREC           REWRITE THE RECORD                           
         MVC   INVMSG(L'INVMSG),SPACES                                          
         MVC   INVMSG(26),=C'STATION CLOSE DATE UPDATED'                        
         FOUT  INVMSGH                                                          
         MVC   INVACT(L'INVACT),SPACES                                          
         FOUT  INVACTH                                                          
         MVC   INVCLS(L'INVCLS),SPACES                                          
         MVC   INVCLS(11),=C'CLOSED THRU'                                       
         GOTO1 VDATCON,DMCB,(3,TWAMON),(6,INVCLS+12)                            
         FOUT  INVCLSH                                                          
         CLI   TOTREQ,C'Y'         'TOTALS ONLY' REQUEST?                       
         BNE   EXXMOD              NO                                           
         CLC   =C'DIS',INVACT      REGULAR DISPLAY?                             
         BNE   M309D               NO                                           
         MVC   INVACT(3),=C'TOT'   YES - RESTORE REQUEST                        
         B     EXXMOD                                                           
M309D    EQU   *                                                                
         CLC   =C'DIN',INVACT      ALTERNATE DISPLAY?                           
         BNE   M309E               NO                                           
         MVC   INVACT(3),=C'TON'   RESTORE ALTERNATE REQUEST                    
         B     EXXMOD                                                           
M309E    EQU   *                                                                
         CLC   =C'DTA',INVACT      TRADE/ALTERNATE DISPLAY?                     
         BNE   M309F               NO                                           
         MVC   INVACT(3),=C'TTA'   RESTORE TRADE/ALTERNATE REQUEST              
         B     EXXMOD                                                           
M309F    EQU   *                                                                
         CLC   =C'DIT',INVACT      TRADE DISPLAY?                               
         BNE   M309G               NO                                           
         MVC   INVACT(3),=C'TTR'   RESTORE TRADE REQUEST                        
         B     EXXMOD                                                           
M309G    EQU   *                                                                
         DC    H'0'                HOW'D WE GET HERE?                           
M310     LA    R3,ACTERR                                                        
         CLC   INVACT(3),=C'DIS'   REGULAR                                      
         BE    M310A                                                            
         CLC   INVACT(3),=C'DIN'   ALTERNATE                                    
         BE    M310A                                                            
         CLC   INVACT(3),=C'DIT'   TRADE                                        
         BE    M310A                                                            
         CLC   INVACT(3),=C'DTA'   TRADE/ALTERNATE                              
         BNE   ERROR                                                            
M310A    EQU   *                                                                
*                                                                               
* PAGE NUMBER RULES-                                                            
*    NO CHANGE, N, OR NEXT NUMBER - GO TO NEXT PAGE                             
*    1 - START AT PAGE 1                                                        
*                                                                               
*                                                                               
         LA    R2,INVLN18H         UNPROTECT LAST INVOICE FIELD                 
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         NI    1(R2),X'DF'                                                      
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
*                                                                               
         LA    R2,INVPAGEH         POINT TO PAGE NO. FIELD                      
         LA    R3,PAGERR                                                        
         MVI   TWADISP,0                                                        
         XC    TWACLIST,TWACLIST                                                
         MVI   TWAJUMP,0                                                        
         CLI   TWAPAGE,0                                                        
         BNE   M314                                                             
*                                                                               
         MVI   TWAPFKEY,0                                                       
         MVI   TWALSTPG,0                                                       
*                                                                               
*   CLEAR OUT 1000 BYTES PER TOTAL (250 X 4)                                    
*                                                                               
M310B    EQU   *                                                                
         ZAP   TWAORDGP(8),=P'0'                                                
         ZAP   TWAINVGP(8),=P'0'                                                
         MVC   ACCUMS,=C'**CTRS**'                                              
         XC    TWAORDT(250),TWAORDT                                             
         XC    TWAORDT+250(250),TWAORDT+250                                     
         XC    TWAORDT+500(250),TWAORDT+500                                     
         XC    TWAORDT+750(250),TWAORDT+750                                     
         XC    TWAINVT(250),TWAINVT                                             
         XC    TWAINVT+250(250),TWAINVT+250                                     
*                                                                               
*   NEXT SECITON OF TWAINVT NOT ADDRESSABLE DIRECTLY                            
*                                                                               
         LA    RF,TWAINVT+250      USE LAST ADDRESSABLE SECTION                 
         LA    RF,250(RF)          BUMP TO NEXT SECTION                         
         XC    0(250,RF),0(RF)     ZERO IT OUT                                  
         LA    RF,250(RF)          BUMP TO NEXT SECTION                         
         XC    0(250,RF),0(RF)     ZERO IT OUT                                  
*                                                                               
*        LA    R2,INVLN18H         THESE CODES ARE MOVED FURTHER UP             
*        ZIC   RE,0(R2)            TO FIX THE BUG WHERE INVOICE FIELD           
*        AR    R2,RE               OF THE LAST LINE GOT PROTECTED AFTER         
*        NI    1(R2),X'DF'         COME BACK FROM TOTAL SCREEN                  
*        OI    6(R2),X'80'                                                      
*        LA    R2,INVPAGEH                                                      
*                                                                               
         OC    INVOFF,INVOFF       IS THERE OFFICE FILTER                       
         BNZ   *+10                                                             
         XC    TWAKEY+11(2),TWAKEY+11       NO                                  
         XC    TWAKEY+13(14),TWAKEY+13                                          
*                                                                               
M311     DS    0H                                                               
         CLI   TWAPFKEY,PFPGUP                                                  
         BE    M311A                                                            
         CLI   TWAPAGE,0                                                        
         BE    M311A                                                            
         CLI   TWALSTPG,0                                                       
         BE    M311A                                                            
         CLC   TWAPAGE,TWALSTPG   JUST DISPLAYED LAST PAGE?                     
         BL    M311A              NO                                            
         MVI   TWAPAGE,0          YES - START OVER                              
         B     M310B                                                            
M311A    DS    0H                  PAGE UP/DOWN                                 
         SR    R4,R4                                                            
         IC    R4,TWAPAGE                                                       
         CLI   TWAPFKEY,PFPGUP     PAGE UP?                                     
         BNE   M312                NO                                           
         CLI   TWAPAGE,1                                                        
         BH    M311B                                                            
         LA    R2,INVACTH                                                       
         OI    6(R2),X'80'+X'40'                                                
         B     EXXMOD                                                           
M311B    BCTR  R4,0                PREVIOUS PAGE                                
         B     M313                                                             
M312     DS    0H                                                               
         LA    R4,1(R4)            NEXT PAGE                                    
M313     DS    0H                                                               
         STC   R4,TWAPAGE                                                       
         EDIT  (R4),(2,INVPAGE),ALIGN=LEFT                                      
         OI    4(R2),X'20'         INDICATE PAGE FIELD VALIDATED                
         OI    1(R2),X'01'         TURN ON MODIFIED BIT                         
         FOUT  INVPAGEH                                                         
         B     M315                                                             
* TEST FOR AUTOMATIC NEXT PAGE                                                  
M314     TM    4(R2),X'20'         IF THEY HAVEN'T CHANGED IT,                  
         BZ    M314L                  THEY WANT NEXT PAGE                       
         OI    1(R2),X'01'         TURN ON MODIFIED BIT                         
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         B     M311                                                             
         SPACE 1                                                                
M314L    CLI   INVPAGE,C'N'                                                     
         BE    M311                                                             
* SEE IF PAGE 1 OR NEXT PAGE NUMBER INPUT                                       
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         LR    R4,R0                                                            
         CH    R0,=H'1'                                                         
         BNE   *+12                                                             
         MVI   TWAPAGE,0                                                        
         B     M310B               START AT PAGE 1                              
*                                                                               
         LR    R5,R0                                                            
         BCTR  R5,R0                                                            
         STC   R5,DUB+1                                                         
         CLC   DUB+1(1),TWAPAGE                                                 
         BE    M311                THEY INPUT NEXT PAGE NO.                     
         CLI   DUB+1,254                                                        
         BH    ERROR                                                            
         OI    4(R2),X'20'         INDICATE PAGE FIELD VALIDATED                
         STC   R0,TWAJUMP                                                       
         CLC   TWAJUMP,TWAPAGE                                                  
         BH    M311                                                             
         MVI   TWAPAGE,0                                                        
         B     M310B                                                            
*                                                                               
* CALL DISPLAY OVERLAY                                                          
M315     GOTO1 VCALLOV,DMCB,(X'20',0),(RA)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
END      DS    0H                                                               
         CLI   SVREPROF+7,C'Y'     CHECK IF DOUBLE LINE DISPLAY                 
         BNE   END10                                                            
         OC    INVLN17(10),INVLN17                                              
         BZ    END20                                                            
         B     END15                                                            
                                                                                
END10    DS    0H                                                               
         OC    INVLN18(10),INVLN18   IF LAST LINE IS EMPTY                      
         BZ    END20                                                            
                                                                                
END15    DS    0H                                                               
         CLI   TWAJUMP,0                                                        
         BE    END18                                                            
         CLC   TWAPAGE,TWAJUMP                                                  
         BNL   END18                                                            
         GOTO1 VDATAMGR,DMCB,=C'DMUNLK'  RELEASE RECS BEFORE PROCEEDING         
         B     M311                                                             
END18    DS    0H                                                               
         CLC   INVLN18(4),=C'*TOT' OR ALREADY HAS TOTAL LINE                    
         BNE   END100              THEN SHOW TOTALS                             
*                                                                               
END20    DS    0H                                                               
         MVC   TWALSTPG,TWAPAGE    SAVE LAST PG NUMBER                          
         CLC   INVMSG(29),=C'CONTRACT INVOICE DATA CHANGED'                     
         BNE   END30                                                            
         MVC   INVMSG+30(20),=C'- NO MORE TO DISPLAY'                           
         B     END40                                                            
END30    MVC   INVMSG(L'INVMSG),SPACES                                          
         MVC   INVMSG(18),=C'NO MORE TO DISPLAY'                                
END40    FOUT  INVMSGH                                                          
*                                                                               
         CLI   TOTREQ,C'Y'         TOTALS ONLY?                                 
         BE    END66               YES - TOTALS ALREADY DEVELOPED               
         CLI   SVREPROF+7,C'Y'     CHECK IF DOUBLE LINE DISPLAY                 
         BNE   END45                                                            
         CLI   TWAPAGE,250         AND DO TOTALS, UNLESS                        
         BH    END90               MORE THEN 250 PAGES                          
         B     END48                                                            
                                                                                
END45    DS    0H                                                               
         CLI   TWAPAGE,125         AND DO TOTALS, UNLESS                        
         BH    END90               MORE THEN 125 PAGES                          
                                                                                
END48    DS    0H                                                               
         LA    R3,250              SCAN ALL PAGES                               
         LA    R4,TWAORDT          LIST OF PAGE ORDERED $ TOTALS                
         ZAP   TWAORDGP(8),=P'0'   ZERO OUT TOTAL AREA                          
END50    EQU   *                                                                
         L     R5,0(R4)            LOAD VALUE                                   
         CVD   R5,WORK2+12         CONVERT VALUE TO PACKED                      
         AP    TWAORDGP(8),WORK2+12(8)                                          
*                                  ADD VALUE PACKED                             
         LA    R4,4(R4)                                                         
         BCT   R3,END50                                                         
***      ST    R5,TWAORDGT         GRAND ORDERED $ TOTAL                        
         SPACE 1                                                                
         LA    R3,250              SCAN ALL PAGES                               
         LA    R4,TWAINVT          LIST OF PAGE INVOICE $ TOTALS                
         ZAP   TWAINVGP(8),=P'0'   ZERO OUT TOTAL AREA                          
END60    EQU   *                                                                
         L     R5,0(R4)            LOAD VALUE                                   
         CVD   R5,WORK2+12         CONVERT VALUE TO PACKED                      
         AP    TWAINVGP(8),WORK2+12(8)                                          
*                                  ADD VALUE PACKED                             
         LA    R4,4(R4)                                                         
         BCT   R3,END60                                                         
***      ST    R5,TWAINVGT         GRAND INVOICE $ TOTAL                        
END66    EQU   *                                                                
         SPACE 1                                                                
         LA    R2,INVLN18H                                                      
         LA    R3,8(R2)                                                         
         MVC   0(8,R3),=C'*TOTALS*'                                             
         EDIT  (P8,TWAORDGP),(14,41(R3)),2,FLOAT=$,MINUS=YES,COMMAS=YES         
***      EDIT  (P8,TWAORDGP),(13,42(R3)),2,FLOAT=$,MINUS=YES,COMMAS=YES         
         FOUT  INVLN18H                                                         
*                                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               POINT TO INVOICE FIELD                       
         CP    TWAINVGP(8),=P'100000000'                                        
*                                  ARE $ 1M(W/PENNIES) OR >?                    
*                                                                               
***      C     R0,=F'100000000'    ARE $ 1MEG(WITH PENNIES) OR >?               
*                                                                               
         BL    END70               NO  - USE REGULAR EDIT                       
         ZAP   WORK+20(10),TWAINVGP   YES - MOVE OUT ORIGINAL VALUE             
         DP    WORK+20(10),=P'100'    DROP PENNIES                              
         EDIT  (P8,WORK+20),(11,8(R2)),MINUS=YES,COMMAS=YES                     
***      EDIT  (P8,TWAINVGP),(11,8(R2)),2,MINUS=YES                             
         B     END80               THIS EDIT HAS NO COMMAS                      
END70    EQU   *                                                                
***      EDIT  (P8,TWAINVGP),(12,8(R2)),2,MINUS=YES,COMMAS=YES                  
         EDIT  (P8,TWAINVGP),(11,8(R2)),2,MINUS=YES,COMMAS=YES                  
END80    EQU   *                                                                
         OI    1(R2),X'20'         MAKE FIELD PROTECTED                         
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
*                                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               POINT TO DIFFERENCE FIELD                    
         MVC   WORK2(8),TWAORDGP   LOAD PACKED ORDER $                          
         MVC   WORK2+8(8),TWAINVGP LOAD PACKED INV   $                          
         SP    WORK2(8),WORK2+8(8) DIFFERENCE = ORDERED - INVOICE               
         CP    WORK2(8),=P'10000000'                                            
*                                  ARE $ 100K(W/PENNIES) OR >?                  
         BL    END83               YES - USE REGULAR EDIT                       
**                                                                              
**       L     R4,TWAORDGT         DIFFERENCE = ORDERED - INVOICE               
**       L     R5,TWAINVGT                                                      
**       SR    R4,R5                                                            
*                                                                               
**       LPR   R0,R4                                                            
**       C     R0,=F'10000000'     VALUE 100,000.00 OR >?                       
**       BL    END83               YES - USE REGULAR EDIT                       
**                                                                              
         ZAP   WORK+20(10),WORK2(8)   YES - MOVE OUT ORIGINAL VALUE             
         DP    WORK+20(10),=P'100'    DROP PENNIES                              
         EDIT  (P8,WORK+20),(10,8(R2)),MINUS=YES,COMMAS=YES                     
***      EDIT  (P8,WORK2),(10,8(R2)),2,MINUS=YES                                
         B     END85                                                            
END83    EQU   *                                                                
         EDIT  (P8,WORK2),(10,8(R2)),2,COMMAS=YES,MINUS=YES                     
*                                                                               
END85    EQU   *                                                                
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
*                                                                               
END90    DS    0H                                                               
***>     MVI   TWAPAGE,0           PREPARE TO START OVER                        
*                                                                               
END100   EQU   *                                                                
         CLI   TOTREQ,C'Y'         'TOTALS ONLY' REQUEST?                       
         BNE   EXXMOD              NO                                           
         CLC   =C'DIS',INVACT      REGULAR DISPLAY?                             
         BNE   END120              NO                                           
         MVC   INVACT(3),=C'TOT'   YES - RESTORE REQUEST                        
         B     EXXMOD                                                           
END120   EQU   *                                                                
         CLC   =C'DIN',INVACT      ALTERNATE DISPLAY?                           
         BNE   END140              NO                                           
         MVC   INVACT(3),=C'TON'   RESTORE ALTERNATE REQUEST                    
         B     EXXMOD                                                           
END140   EQU   *                                                                
         CLC   =C'DIT',INVACT      TRADE DISPLAY?                               
         BNE   END160              NO                                           
         MVC   INVACT(3),=C'TTR'   RESTORE TRADE REQUEST                        
         B     EXXMOD                                                           
END160   EQU   *                                                                
         CLC   =C'DTA',INVACT      TRADE/ALTERNATE DISPLAY?                     
         BNE   END180              NO                                           
         MVC   INVACT(3),=C'TTA'   RESTORE TRADE/ALTERNATE REQUEST              
         B     EXXMOD                                                           
END180   EQU   *                                                                
         DC    H'0'                HOW'D WE GET HERE?                           
         EJECT                                                                  
***>>>                                                                          
*          DATA SET REINV00    AT LEVEL 179 AS OF 01/15/98                      
*                                                                               
*        ROUTINE TO GET Y/M OF THIS ACCOUNTING MONTH INTO WORK                  
*                                                                               
         SPACE                                                                  
GETAMON  NTR1                                                                   
         L     R3,=F'765'          SET ERROR = NO EOM RECORD                    
         ST    R3,DUB                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,HALF)                                      
         GOTO1 (RF),(R1),(2,HALF),(3,TODAY)                                     
         XC    KEY,KEY             BUILD EOM KEY                                
         MVI   KEY,X'18'                                                        
         MVC   KEY+24(2),REPALPHA  REP                                          
         MVC   KEY+26(1),TODAY     YEAR                                         
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GAMO0120            BRANCH TO ERROR EXIT                         
         BAS   RE,GETREC           GET EOM RECORD                               
         SR    R7,R7                                                            
         LA    RF,REOMDATE                                                      
*                                                                               
GAMO0020 CLC   HALF,0(RF)          SEARCH FOR ACCOUNTING MONTH                  
         BNH   GAMO0040                                                         
         LA    R7,2(R7)                                                         
         CH    R7,=H'24'                                                        
         BH    GAMO0080                                                         
         LA    RF,REOMDATE(R7)                                                  
         B     GAMO0020                                                         
*                                                                               
GAMO0040 LTR   R7,R7                                                            
         BZ    GAMO0060                                                         
         SRA   R7,1                                                             
         STC   R7,WORK+1           MONTH                                        
         MVC   WORK(1),TODAY       YEAR                                         
         B     GAMO0100                                                         
*                                                                               
GAMO0060 MVI   WORK+1,12           DECEMBER                                     
         ZIC   R1,TODAY            LAST YEAR                                    
         BCTR  R1,0                                                             
         STC   R1,WORK                                                          
         B     GAMO0100                                                         
*                                                                               
GAMO0080 MVI   WORK+1,1            JANUARY                                      
         ZIC   R1,TODAY            NEXT YEAR                                    
         LA    R1,1(R1)                                                         
         STC   R1,WORK                                                          
*                                                                               
GAMO0100 SR    R0,R0               SET CC = ZERO                                
         B     GAMO0140                                                         
GAMO0120 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
GAMO0140 XIT1                                                                   
         EJECT                                                                  
***>>>                                                                          
**********************************************************************          
*                 TYPE OF CONTRACT ADDED TO SCREEN AND THEN                     
* AUG  95  (JR )  PROVIDE OPTION ON SCREEN TO INPUT DOLLARS IN       *          
*                 NET RATHER THAN GROSS.  THEY ARE STORED IN GROSS   *          
*                 IN THE CONTRACT FILE.                              *          
**********************************************************************          
*                                 BELOW IS ERROR MESSAGE                        
NETVALER EQU   137                                                              
*                                                                               
NETRTN   EQU   *                   N E T   R O U T I N E                        
         NTR1                                                                   
         XC    OPTNET$$,OPTNET$$                                                
*        TM    INVNETTH+4,X'20'                                                 
*        BO                        VALIDATED PREVIOUSLY                         
         LA    R2,INVNETTH         ADDR OF NET HEADER                           
         ZIC   RF,5(R2)            LENGTH                                       
         LTR   RF,RF                                                            
         BZ    MVCZ                DEFAULT , NOTHING ENTERED. INV10             
*                                  NOTHING ENTERED FOR NET SO GROSS             
         CH    RF,=H'1'            SINGLE CHARACTER ENTERED?                    
         BE    CHECKYN                                                          
         TM    4(R2),X'08'         NUMERIC ?                                    
         BNO   ERNTNUM             TYPE NOTHING, N, Y, OR 2 DIGIT NO.           
         B     MVPC                MOVE TYPE 'S PER CENT FOR CALC               
                                                                                
MVCZ     MVC   OPTNET$$,=C'00'     DEFAULT IS 85 FOR GROSS                      
         B     NETEXIT                                                          
                                                                                
CHECKYN  CLI   8(R2),C'Y'                                                       
         BE    DOCALC              DEFAULT CALCULATION OF 85                    
         CLI   8(R2),C'N'                                                       
         BNE   ERNTYN              ERROR NOT Y OR N                             
         MVC   OPTNET$$,=C'00'     ??JR                                         
         B     NETEXIT             NO CALCULATION REQUESTED   GROSS             
* N =USE THE GROSS AND NOT THE NET FIGURE CALCULATION                           
************************************************************                    
*                                                                               
*                                                                               
ERNTNUM  EQU   *                                                                
*                                  ERROR    NOT NUMERIC                         
         LTR   RB,RB               SET CC  NT  ZERO                             
         B     NETEX2                                                           
ERNTYN   EQU   *                                                                
*                                  ERROR    NOT Y  OR  N                        
         LTR   RB,RB               SET CC  NT  ZERO                             
         B     NETEX2                                                           
                                                                                
                                                                                
                                                                                
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*********************************************************************           
****                                                                            
****  MOVE THE TYPE PERCENT FROM SCREEN TO OPTNET$$ IN CHAR                     
****                                                                            
MVPC     XC    OPTNET$$,OPTNET$$                                                
         MVC   OPTNET$$(2),INVNETT  PER CENT ENTERED IN TYPE FOR CALC           
         SR    R0,R0         SET CC TO ZERO ::: SCREEN  OK                      
         B     NETEX2              USE THE ENTERED PER CENT NOT 85              
**                                                                              
*     WHEN NEEDED  FOR A BINARY CALCULATION DO BELOW                            
                                                                                
*MVPC     PACK  DUB(8),INVNETT(2)                                               
*        CVB   RF,DUB              PER CENT ENTERED IN TYPE FOR CALC            
*        STCM  RF,15,OPTNET$$                                                   
*        SR    R0,R0         SET CC TO ZERO ::: SCREEN  OK                      
*        B     NETEX2              USE THE ENTERED PER CENT NOT 85              
*                                  PROCESS CALCULATION IN REINV10               
*********************************************************************           
*                                                                               
*     CALCULATE THE NET WITH A DEFAULT PER CENT OF 85 OR A PER CENT             
*   THAT WAS ENTERED IN THE NET FIELD ON THE SCREEN.                            
*     DO THIS IN PROGRAM   REINV10...                                           
**                                                                              
DOCALC   EQU   *                                                                
         XC    OPTNET$$,OPTNET$$                                                
         MVC   OPTNET$$(2),=C'85'                                               
         SR    R0,R0         SET CC TO ZERO ::: SCREEN  OK                      
         B     NETEX2                                                           
                                                                                
*   WHEN NEEDED    FOR A BINARY CALCULATION DO BELOW                            
*DOCALC   EQU   *                                                               
*                                                                               
*        LA    RF,85                                                            
*        STCM  RF,15,OPTNET$$      15 MEANS ALL  1111  ARE STORED               
*        SR    R0,R0         SET CC TO ZERO ::: SCREEN  OK                      
*        B     NETEX2                                                           
***************************END MY ROUTINE                                       
*                                                                               
*                                                                               
*                                                                               
NETEXIT  EQU   *                                                                
*                        W H Y   O U T  ???? REG  IS SET ????                   
* FZERO  LA    RF,00         LOAD RF WITH ZERO WHEN NONE ENTERED                
*        STCM  RF,15,OPTNET$$                                                   
         SR    R0,R0         SET CC TO ZERO ::: SCREEN  OK                      
NETEX2   XIT1                                                                   
*                            USED WITH   BAS   RE,NETRTN                        
         EJECT                                                                  
*                                                                               
*- ORD2INV -- COPY ORDERED COLUMNS TO INVOICE COLUMNS                           
*                                                                               
*  INPUT:  P1 = GLOBAL/SELECTIVE SWITCH                                         
*               0 = COPY ALL ORDERED TO INVOICED                                
*               1 = ONLY COPY THOSE ITEMS WITH 'I' IN INVOICE FIELD.            
*                                                                               
ORD2INV  NTR1                                                                   
         L     R5,0(R1)            PICK UP CALLER'S P1                          
         LA    R3,18               18 LINES ON SCREEN                           
         LA    R2,INVLN01H                                                      
         USING LINED,R2                                                         
O2I100   EQU   *                                                                
         LR    RE,R2                                                            
         ZIC   R0,0(R2)            PROT FLD LEN                                 
         AR    RE,R0               RE=A(INVOICE FLD HEADER)                     
*                                                                               
         LTR   R5,R5               GLOBAL OR SELECTIVE COPY?                    
         BZ    O2I150              GLOBAL.                                      
*                                                                               
*- CHECK FOR SELECTIVE COPY INDICATOR IN THE INVOICE FIELD.                     
         CLI   8(RE),C'I'          COPY THIS FIELD?                             
         BE    O2I150              YES                                          
         TM    4(RE),X'C0'         INPUT IN THE INVOICED FIELD?                 
         BNZ   O2I195              YES                                          
         B     O2I200              NO.                                          
*                                                                               
*- COPY ORD TO INVOICED AND SET FLD HEADER TO LOOK LIKE INPUT.                  
O2I150   EQU   *                                                                
         MVI   4(RE),X'C0'         INPUT THIS TIME                              
         MVI   5(RE),10            SET LENGTH OF INPUT                          
         MVI   7(RE),10            SET LENGTH OF OUTPUT                         
         MVC   8(10,RE),LORDI      DATA TO INV FLD                              
         CLI   LORDI+10,C'-'       NEG AMT? IF SO, PRECEED NUM BY -             
         BNE   O2I190                                                           
         LA    R1,8(RE)                                                         
O2I160   DS    0H                                                               
         CLI   0(R1),C'$'          LARGE ENTRY?                                 
         BNE   *+12                                                             
         MVI   0(R1),C'-'                                                       
         B     O2I190                                                           
*                                                                               
         CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         LA    R1,1(R1)                                                         
         B     O2I160                                                           
*                                                                               
         BCTR  R1,0                BACK UP 1 POSITION                           
         MVI   0(R1),C'-'          INSERT - BEFORE AMOUNT                       
         MVI   5(RE),11            SET LENGTH OF INPUT                          
         MVI   7(RE),11            SET LENGTH OF OUTPUT                         
O2I190   EQU   *                                                                
         FOUT  (RE)                                                             
O2I195   EQU   *                                                                
         ST    RE,LASTFLD          MAKE SURE WE PROCESS THIS FLD                
*                                                                               
O2I200   EQU   *                                                                
         LA    RE,INVLN02H-INVLN01H                                             
         AR    R2,RE               NEXT PROT LINE                               
         BCT   R3,O2I100                                                        
         XIT1                                                                   
         DROP  R2                                                               
LOCALERR SR    R3,R3                                                            
         MVC   INVMSG(L'EOMDERR),EOMDERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
EOMDERR  DC    C'ER#BU NO EOM RECORD FOR CURRENT PERIOD ON FILE'                
***********************************************************************         
* CKGLOB - CHECK FOR INCOMING GLOBBER ELEMENTS                        *         
***********************************************************************         
CKGLOB   NTR1                                                                   
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
*                                                                               
* CHECK FOR GLOBBER CONTROL VARIABLE                                            
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',WORK,24,GLVXCTL                               
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   NO                                                               
         GOTO1 (RF),DMCB,=C'DELE',,,GLVXCTL                                     
*                                                                               
         LA    R3,WORK                                                          
         USING GLVXFRSY,R3                                                      
*                                                                               
         CLC   =C'CON',GLVXFRPR    RETURN CALL FROM CONTRACT?                   
         BNE   CKGL050             NO                                           
         LA    R2,INVMSGH                                                       
         XC    8(L'INVMSG,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            BUMP TO SERVICE REQUEST FLD                  
         AR    R2,R0                                                            
         MVC   8(4,R2),=C'=RE '      FORCE SCREEN REFRESH                       
         MVI   5(R2),4                                                          
         OI    6(R2),X'80'                                                      
         LA    R2,INVACTH          ACTION FIELD                                 
         OI    6(R2),X'40'+X'80'   CURSOR HERE                                  
         L     RD,4(RD)                GET ALL THE WAY OUT OF HERE              
         B     EXXMOD                                                           
*                                                                               
CKGL050  DS    0H                                                               
         B     NO                                                               
         DROP  R3                                                               
***********************************************************************         
* HOTKEY - CHECK FOR HOTKEY (PFKEY) PRESSED                           *         
***********************************************************************         
HOTKEY   NTR1                                                                   
         L     RF,ATIO                                                          
         USING TIOBD,RF                                                         
         MVC   HALF,TIOBCURS       CURSOR ABSOLUTE SCREEN POSITION              
         ZIC   R0,TIOBAID                                                       
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,TWAPFKEY         SAVE OFF KEY HIT                             
*                                                                               
* CHECK PFKEY NUMBER                                                            
         CLI   TWAPFKEY,2          SWAP TO CONTRACT?                            
         BE    HK100               YES                                          
         CLI   TWAPFKEY,PFPGUP     PG UP?                                       
         BE    HK200                                                            
         CLI   TWAPFKEY,PFPGDOWN   PG DOWN?                                     
         BE    HK200                                                            
*                                                                               
* PUT FURTHER PFKEY CHECKS HERE                                                 
         B     EXXMOD                                                           
*                                                                               
* SWAP TO CONTRACT WITH K NUMBER ON LINE WITH CURSOR                            
*                                                                               
HK100    DS    0H                  GET K NUM FROM CURSOR POS ON SCREEN          
         SR    R4,R4                                                            
         LH    R5,HALF             CURSOR POSITION                              
         D     R4,=F'80'           FIND ROW#                                    
         LA    R5,1(R5)            NOW R5=ROW# OF CURSOR                        
*                                                                               
         LA    R3,705              ERR MSG                                      
         CH    R5,=H'5'            CURSOR MUST BE BETWEEN ROWS 5&22             
         BL    HKERR                                                            
         CH    R5,=H'22'                                                        
         BH    HKERR                                                            
*                                                                               
         CLI   SVREPROF+7,C'Y'     CHECK IF DOUBLE LINE DISPLAY                 
         BNE   HK110               NO - DON'T WORRY                             
         SR    R6,R6                                                            
         LR    R7,R5                                                            
         D     R6,=F'2'            DIV LINE# BY 2                               
         LTR   R6,R6               EVEN LINE# ?                                 
         BNZ   HK110               NO - OK                                      
         BCTR  R5,0                YES - K NUM IS ON PREV LINE                  
*                                                                               
HK110    DS    0H                                                               
         SH    R5,=H'4'            SUBTRACT # OF HEADER LINES                   
         LA    R2,INVLN01          FIRST DISPLAY LINE                           
         B     *+8                                                              
         LA    R2,INVLN02-INVLN01(R2) NEXT DISPLAY LINE                         
         BCT   R5,*-4                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(8),35(R2)      K NUM FROM SCREEN                            
*                                                                               
         LA    R3,8                REMOVE PRECEDING SPACES                      
         LA    R4,WORK                                                          
HK115    CLI   0(R4),C' '                                                       
         BNE   HK120                                                            
         MVI   0(R4),0                                                          
         LA    R4,1(R4)                                                         
         BCT   R3,HK115                                                         
HK120    DS    0H                                                               
         OC    WORK,WORK                                                        
         BNZ   *+12                                                             
         LA    R3,705                                                           
         B     HKERR                                                            
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CHEXIN                                                        
         DROP  RE                                                               
         GOTO1 (RF),DMCB,WORK,FULL,8                                            
*                                                                               
         XC    WORK,WORK           BUILD GLOBBER CONTROL ELEM                   
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'INV'    SFM PROGRAM                                  
         MVC   GLVXTOSY,=C'REP'    TO THE CALLEE'S SYSTEM                       
         MVC   GLVXTOPR,=C'CON'    TO THE CALLEE'S PROGRAM                      
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'PUTD',WORK,24,GLVXCTL                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  BUILD DISPLAY K ELEM FOR CONTRACT            
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'PUTD',FULL,4,GLRDISPK                               
         CLI   DMCB+8,0                                                         
         L     RD,4(RD)            GET ALL THE WAY OUT                          
         BE    EXXMOD                                                           
         DC    H'0'                                                             
*                                                                               
HK200    DS    0H                  PG SCROLL HANDLER                            
         B     EXXMOD                                                           
*                                                                               
HKERR    DS    0H                                                               
         LA    R2,INVACTH                                                       
         L     R4,ERRAREA                                                       
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGETTXT                                                       
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(R3),0,(C'E',0),0,0,0                                  
         LA    R5,INVMSGH                                                       
         ZIC   R0,0(R5)            BUMP TO SERVICE REQUEST FLD                  
         AR    R5,R0                                                            
         OI    6(R5),X'80'+X'01'   MODIFIED                                     
         L     RD,4(RD)                                                         
         B     EXIT                                                             
*                                                                               
NO       LTR   RB,RB                                                            
         B     EXXMOD                                                           
YES      CR    RB,RB                                                            
         B     EXXMOD                                                           
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
* LOCAL VARIABLES                                                               
*                                                                               
ELCODE   DS    X                                                                
*                   .0.1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6.7.8.9                    
NEW23ELT DC    XL20'2314FFFFFF400040404040404040400000000000'                   
*                                                                               
         EJECT                                                                  
       ++INCLUDE REGENINT                                                       
         EJECT                                                                  
       ++INCLUDE REINVGEN                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE REGENPBLK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016REINV00   10/04/12'                                      
         END                                                                    
