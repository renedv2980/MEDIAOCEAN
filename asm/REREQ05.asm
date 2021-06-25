*          DATA SET REREQ05    AT LEVEL 149 AS OF 05/01/02                      
*PHASE T80705A,*                                                                
*INCLUDE SCANNER                                                                
*INCLUDE REQTWA                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE LOADER                                                                 
         SPACE                                                                  
REQ5     TITLE 'T80705 - REREQ05 - REP REQ SECURITY INTERFACE'                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  REREQ05 -- PHASE T80705 -- REP REQUEST SECURITY                    *         
*                                                                     *         
*  BUILDS 'SECURITY' PARAMETER BLOCK.                                 *         
*  RETRIEVES AND ANALYZES REQUEST VS  REPORT PARAMETERS               *         
*  INTERFACES WITH 'SECURITY BLACK BOX'                               *         
*  RETURNS 'SECURITY ERROR' IF DETERMINED.                            *         
*                                                                     *         
*                                                                     *         
       ++INCLUDE REREQPRF                                                       
***********************************************************************         
*                                                                     *         
*  MOD LOG                                                            *         
*  -------                                                            *         
*                                                                     *         
*  AUG05/98 (BU ) --- ORIGINAL ENTRY                                  *         
*                                                                     *         
*  OCT19/98 (BU ) --- TIGHTEN OWNER/MARKET PAR RECORD ACCESS          *         
*                                                                     *         
*  DEC08/98 (BU ) --- RESET SBBREAKS TO ZERO                          *         
*                     EXIT IF NO RRG SPEC                             *         
*                                                                     *         
*  JAN21/02 (RHV) --- FIX GROUP INFERENCE BUG                         *         
*                                                                     *         
*                     ***  END TOMBSTONE  ***                         *         
***********************************************************************         
*                                                                     *         
*   R8 AND R6 ARE USED AS ADDITIONAL BASE REGISTERS.  AFTER THEM,     *         
*          'RC' IS ALSO USED.                                         *         
*                                                                     *         
*                                                                     *         
*   NOTE:  'RC' IS USED AS A PROGRAM BASE REGISTER AND DOES NOT POINT *         
*           TO THE WORK AREA                                          *         
*                                                                     *         
*          FURTHER, RC CANNOT BE SET-UP IN THE NMOD MACRO BECAUSE     *         
*           THE MACRO TRIES TO SET RC TO THE WORKAREA                 *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         PRINT NOGEN                                                            
T80705   CSECT                                                                  
         NMOD1 0,T80705,R8,R6,RR=R2                                             
*                                                                               
         LA    RC,2048(R6)         ESTABLISH 'RC' AS BASE REGISTER              
*                                     AFTER LAST (R6) REGISTER                  
         LA    RC,2048(RC)                                                      
         USING T80705+12288,RC     DISPLACEMENT INTO MODULE                     
*                                                                               
         L     R9,0(R1)            A(WORK AREA FROM CALLER)                     
         USING REQWRK,R9                                                        
*                                                                               
         ST    R2,SUBRELO          SUB-OVERLAY RELOCATION FACTOR                
*                                                                               
         LR    RF,R9               SET A(REQWRK AREA)                           
         AH    RF,=Y(DIOWORK)      SET A(IOWORK W/IN DSECT)                     
*                                                                               
         MVC   0(4,RF),=C'*IO*'    INSERT FLAG                                  
         LA    RF,4(RF)            BUMP PAST FLAG                               
         ST    RF,AIOADDR2         SAVE ADDRESS OF IOWORK                       
*                                                                               
         L     RA,ASAVE            A(TWA)                                       
         USING TWAD,RA                                                          
*                                                                               
         LA    RE,REQDEF           USE REQTBL ENTRY IN TWA                      
         ST    RE,AREQNTRY                                                      
*                                                                               
         CLI   RTNFLAG,5           ADD/CHANGE/SOON/RFP?                         
         BL    ADNR0020            YES - CHECK ACCESS SECURITY                  
*                                  NO  - DONE IN REQ02 MODULE                   
         SR    R0,R0               SET CC = ZERO                                
         XIT1                      EXIT MODULE                                  
ADNR0020 EQU   *                                                                
*                                                                               
         CLI   RRRGFLAG,C'Y'       RRG REQUEST?                                 
         BNE   ADNR0040            NO  - DON'T DO ACCESS CHECK                  
         BAS   RE,ACCESRRG         CHECK ACCESS FOR RRG REPORTS                 
         B     ADNR0060                                                         
*                                                                               
ADNR0040 EQU   *                                                                
         BAS   RE,ACCESREG         CHECK ACCESS FOR NON-RRG RPTS                
ADNR0060 EQU   *                                                                
         BAS   RE,LDREQOPT         LOAD OPTIONS FROM REQUEST CARD               
*                                                                               
*                                                                               
         GOTOX (RFCKSEC,AREPFACS),DMCB,SBLOCK,RQSMSGH,0,RFBLOCK                 
         BE    ADNR0080            OKAY RETURN                                  
         XC    AERROR,AERROR       SET FOR MY MESSAGE                           
         B     ERROR               EXIT WITH CANNED MESSAGE                     
*                                                                               
ADNR0080 EQU   *                                                                
*                                                                               
*   THE 'BLACK BOX' WILL SEND BACK, IN BYTE 0 OF THE FIRST PARAMETER,           
*        X'60' IF SPECIAL OWNER/MARKET FILTERING IS TO BE DONE AT REPT          
*        TIME.  THIS IS TO SET BYTE 59 IN REQUEST CARD 3 (Q3OWNMKT).            
*                                                                               
*   TEST BRANCH                                                                 
*                                                                               
****>>>> B     TEST0020            ACTIVATE TO TEST IN ALL CASES                
*                                                                               
         TM    DMCB,X'40'          CHECK FOR OWNER/MARKET PASSBACK              
         BNO   ADNR0120            NOT OWNER/MARKET                             
         TM    DMCB,X'20'          SECOND CHECK FOR OWNER/MARKET                
         BNO   ADNR0120            NOT OWNER/MARKET                             
TEST0020 EQU   *                                                                
*                                  OWNER/MARKET: IS IT A                        
         TM    SBBREAKS,X'08'         MARKET-BREAKING REPORT?                   
         BNO   ADNR0120            NO  - DON'T SET FLAG                         
         TM    SBBREAKS,X'20'      YES - DOES IT ALSO SHOW STATION?             
         BO    ADNR0120            YES - DON'T SET FLAG                         
*                                     OWNER SERVES AS FILTER                    
         LA    RE,REQCARD1         SET A(REQUEST CARD 3)                        
         USING QREC3,RE                                                         
         CLC   QREC3,SPACES        ANYTHING IN REQ CARD 3?                      
         BNE   ADNR0100            YES - INSERT OWNER/MARKET FILTER             
         ZIC   RF,RQHFLAG          NO  - BUMP CARD COUNT BY 1                   
         A     RF,=F'16'           ADD 1 TO HIGH ORDER NYBBLE                   
         STC   RF,RQHFLAG          RESTORE BYTE                                 
ADNR0100 EQU   *                                                                
         MVI   Q3OWNMKT,C'Y'       SET 'OWNER/MARKET FILTER'                    
         DROP  RE                                                               
*                                                                               
ADNR0120 EQU   *                                                                
         CLI   RTNFLAG,1           REQUEST FOR CHANGE?                          
         BNE   ADNR0140            NO                                           
*                                                                               
         L     RF,=A(DOCHANGE)                                                  
         B     ADNR0180                                                         
*                                                                               
ADNR0140 EQU   *                                                                
         CLI   RTNFLAG,2           REQUEST FOR FILE ADD?                        
         BE    ADNR0150            YES                                          
         CLI   RTNFLAG,4           NO  - REQUEST FOR FILE ADD/RFP?              
         BNE   ADNR0160            NO                                           
         B     ADNR0200            NO FILE ADD FOR RLP                          
*                                     USED TO BE RFP                            
ADNR0150 EQU   *                                                                
         L     RF,=A(DOFILE)                                                    
         B     ADNR0180                                                         
*                                                                               
ADNR0160 EQU   *                                                                
         CLI   RTNFLAG,3           REQUEST FOR SOON?                            
         BE    *+6                 NO                                           
         DC    H'0'                UNRECOGNIZED ROUTINE NUMBER                  
         L     RF,=A(DOSOON)                                                    
         B     ADNR0180                                                         
*                                                                               
ADNR0180 GOTO1 (RF),P1,(R9),SUBRELO,RR=SUBRELO                                  
         BNZ   ERROR                                                            
*                                                                               
ADNR0200 EQU   *                                                                
         LA    RE,RQSACTH                                                       
         ST    RE,CURSPOS                                                       
*                                                                               
         SR    R0,R0               GOOD CC                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ACCESREG:  ACCESS NON-RRG REPORT TABLE.  INSERT REPORT                      
*        SPECIFICS INTO PARAMETER BLOCK.                                        
*                                                                               
ACCESREG NTR1                                                                   
         XC    SBBREAKS,SBBREAKS   CLEAR INITIAL VALUE                          
         LA    R3,REGREPS          SET A(REGULAR REPORT TABLE)                  
AREG0020 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE?                                
         BE    AREG0200            YES - FINISHED.                              
         CLC   REQCARD+0(2),0(R3)  REPORT REQUEST IN TABLE?                     
         BE    AREG0040            YES                                          
         LA    R3,LREGREP(R3)      NO  - BUMP TO NEXT ENTRY                     
         B     AREG0020            GO BACK FOR NEXT                             
AREG0040 EQU   *                                                                
         MVC   SBBREAKS(1),2(R3)   INSERT REPORT CHARACTERISTICS                
*                                     INTO PARAMETER BLOCK                      
AREG0200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
REGREPS  DC    C'15',X'00'         DARE STATION SWITCH                          
LREGREP  EQU   *-REGREPS                                                        
         DC    C'16',X'20'         STATION CLOSEOUT                             
         DC    C'17',X'20'         STATION ADV COMPARISON                       
         DC    C'18',X'20'         ACE SWITCH                                   
         DC    C'19',X'20'         INVOICE CONTROL LIST                         
         DC    C'1A',X'60'         BUDGET PREPARATION                           
         DC    C'1B',X'60'         BUDGET PROJECTION                            
         DC    C'1C',X'60'         BUDGET ALLOCATION SPREADER                   
         DC    C'1G',X'40'         OFFICE BUDGET REPORT                         
         DC    C'1K',X'20'         STRATEGY SEEDER                              
         DC    C'1L',X'60'         MONTHLY BUDGET PROJECTION                    
         DC    C'1S',X'00'         STATION JOIN/BILLING REPORT                  
         DC    C'20',X'20'         CONTRACT RECONCILIATION                      
         DC    C'21',X'20'         CONTRACT RECONCILIATION                      
         DC    C'2D',X'20'         DARE SPOT COUNT                              
         DC    C'30',X'20'         RADAR                                        
         DC    C'34',X'60'         STATION BILLING SUMMARY                      
         DC    C'36',X'60'         OFFICE SHARE SUMMARY                         
         DC    C'38',X'00'         WEEKLY SALES SUMMARY                         
         DC    C'40',X'60'         SALESPERSON REPORT                           
         DC    C'42',X'60'         SALESPERSON SUMMARY                          
         DC    C'47',X'60'         COMMISSION REPORT                            
         DC    C'85',X'00'         SALES ACTIVITY REPORT                        
         DC    C'8A',X'20'         SALES SUCCESS REPORT                         
         DC    C'8B',X'20'         SALESPERSON SUCCESS REPORT                   
         DC    C'8C',X'62'         SALESPERSON SUCCESS REPORT                   
         DC    C'90',X'20'         STATION/ADV DOWNLOAD                         
         DC    C'91',X'20'         STATION GAIN/LOSS REPORT                     
         DC    X'0000'                                                          
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*   ACCESRRG:  EXAMINE SPEC DELIVERED.  SET UP PARBLOCK ENTRY                   
*        FOR USE WITH CALL TO 'BLACK BOX' PERSONAL ACCESS                       
*        VALIDATION.                                                            
*                                                                               
ACCESRRG NTR1                                                                   
         XC    SBBREAKS,SBBREAKS   CLEAR INITIAL VALUE                          
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VSWITCH,CSWITCH     SET A(SWITCH)                                
         MVC   GETFACT,CGETFACT    SET A(GETFACT)                               
         DROP  RE                                                               
*                                                                               
*   RETRIEVE ORIGINAL SYSTEM NUMBER                                             
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   ORIGUTL,FASYS                                                    
*                                  SAVE ORIGINAL SYSTEM NUMBER                  
         DROP  RF                                                               
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                  SWITCH TO CONTROL FILE                       
         GOTO1 (RF),DMCB,(X'0A',X'FFFFFFFF'),0                                  
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    *+6                                                              
         DC    H'0'                NO  - ABORT                                  
*                                                                               
         L     R3,=A(SPECBLOC)     SET A(1ST RECORD ELEMENT)                    
         A     R3,SUBRELO          SET ADDRESSABILITY                           
*                                                                               
         L     R4,AIOWORK          SET A(WORKSPACE)                             
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(2),=X'0070'     INSERT RECORD TYPE                           
         MVC   KEY+2(4),=C'RERG'   INSERT RRG SPEC CODE                         
         MVC   KEY+6(2),REQCARD+77 INSERT REPORT NUMBER                         
         MVC   KEYALT(27),KEY                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEYALT,KEY,0              
         CLC   KEY(8),KEYALT       SAME KEY TYPE/REPORT CODE?                   
         BE    ARRG0040            YES - RETRIEVE RECORD                        
*                                  NO  - NO REPORT SPEC                         
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'REP ',0                                             
*                                  SWITCH BACK TO REP SYSTEM                    
*                                                                               
         B     ARRG0160                                                         
ARRG0020 EQU   *                                                                
         MVC   KEYALT(40),KEY      RESET KEY                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'GENDIR',KEYALT,KEY,0              
         CLC   KEY(8),KEYALT       SAME KEY TYPE/REPORT CODE?                   
         BNE   ARRG0060            NO  - ALL RECORDS RETURNED                   
*                                     PROCESS THE SPEC                          
         CLC   KEY+10(2),KEYALT+10 SAME SEQUENCE NUMBER?                        
         BE    ARRG0020            YES - SKIP DUPLICATES                        
ARRG0040 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,(R4),GFIL          
         CLI   DMCB+8,X'10'        RECORD NOT FOUND?                            
         BNE   *+6                                                              
         DC    H'0'                MUST BE THERE                                
         ZICM  R1,32(R4),2         INSERT LENGTH FOR MOVE                       
         S     R1,=F'42'           SUBTRACT KEY LENGTH                          
         ST    R1,SAVELEN          SAVE LENGTH OF MOVE                          
*                                                                               
*   ENSURE SPACE IS AVAILABLE TO LOAD PORTION OF SPEC                           
*                                                                               
         L     RE,=A(SPECEND)      SET A(END OF AVAILABLE SPEC AREA)            
         A     RE,SUBRELO          SET ADDRESSABILITY                           
         LR    RF,R3               SET A(NEXT PORTION TO LOAD)                  
         AR    RF,R1               ADD LENGTH OF PORTION                        
         CR    RF,RE               NEW END VS END OF AREA                       
         BNH   *+6                 SPACE AVAILABLE - PROCEED                    
         DC    H'0'                INCREASE SPACE FOR SPEC                      
*                                                                               
         LA    RE,42(R4)           INSERT A(RECORD DATA)                        
         LR    RF,R3               INSERT A(RECEIVING FIELD)                    
         PRINT GEN                                                              
         MOVE  ((RF),(R1)),(RE)    MOVE RECORD TO SPECBLOC                      
         PRINT NOGEN                                                            
         L     R1,SAVELEN          BUMP STORAGE BY LENGTH OF RECORD             
*                                                                               
         AR    R3,R1               SET A(NEXT RECORD)                           
         MVC   0(4,R3),=X'00000000'                                             
*                                  SET DELIMITER                                
         B     ARRG0020            GO BACK FOR NEXT RECORD                      
*                                                                               
SAVELEN  DS    F                                                                
*                                                                               
ARRG0060 EQU   *                                                                
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'REP ',0                                             
*                                  SWITCH BACK TO REP SYSTEM                    
*                                                                               
         L     R4,=A(SPECBLOC)     SET A(1ST RECORD ELEMENT)                    
         A     R4,SUBRELO          SET ADDRESSABILITY                           
*                                                                               
         MVI   SPEC,10             LOOK FOR REPORT SPEC                         
         BAS   RE,GETSPEC                                                       
         BNE   ARRG0160            END OF SPECS                                 
ARRG0080 EQU   *                                                                
         CLI   1(R4),3             TEST NEW STYLE REP SPEC                      
         BE    ARRG0120            NEW SPEC IS 4 LONG - ACCEPT SPEC             
         CLI   3(R4),0             TEST FOR IF                                  
         BE    ARRG0120            NO 'IF' IN ELEMENT - ACCEPT SPEC             
         LA    R5,3(R4)            A(1ST 'IF' CLAUSE OF REPORT)                 
         ZIC   R0,1(R4)            L(REPORT SPEC)                               
         LA    RF,3                CALCULATE # IFS IN ELEMENT                   
         SR    R0,RF                  L - 3 = # IFS                             
ARRG0100 EQU   *                                                                
         MVC   BYTE,0(R5)          MOVE 'IF' NUMBER                             
         BAS   RE,CHECKIF          CHECK FOR TRUE                               
         BNE   ARRG0140            NOT TRUE:  SKIP SPEC                         
         LA    R5,1(R5)            BUMP TO NEXT 'IF'                            
         BCT   R0,ARRG0100         GO BACK FOR NEXT 'IF'                        
*                                  ALL 'IFS' ARE TRUE: ACCEPT IT                
ARRG0120 EQU   *                                                                
         BAS   RE,SCANSPEC         CHECK FOR SPEC CHARACTERISTICS               
         B     ARRG0160            FINISHED                                     
ARRG0140 EQU   *                                                                
         MVI   SPEC,10             LOOK FOR NEXT REPORT SPEC                    
         BAS   RE,NEXTSPEC                                                      
         BNE   ARRG0160            END OF SPECS                                 
         B     ARRG0080            GO BACK AND PROCESS NEXT 10                  
ARRG0160 EQU   *                                                                
         XIT1                                                                   
GFIL     DS    12D                 WORK AREA FOR GETREC                         
KEYALT   DS    CL50                                                             
**>>>>                                                                          
*                                                                               
*   SCANSPEC:  REPORT SELECTED HAS BEEN IDENTIFIED.  SCAN IT, AND               
*        SET ITS CHARACTERISTICS INTO 'PARBLOCK'.                               
*                                                                               
SCANSPEC NTR1                                                                   
         LR    R2,R4               SET A(REPORT FOUND)                          
SSPE0020 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
*                                                                               
         CLI   0(R2),10            NEXT REPORT ENCOUNTERED?                     
         BE    SSPE0800            YES - FINISHED                               
         CLI   0(R2),0             END OF SPEC?                                 
         BE    SSPE0800            YES - FINISHED                               
         CLI   0(R2),30            IS IT A ROW SPEC?                            
         BNE   SSPE0020            NO  - SKIP IT                                
         LA    R3,SPECTAB          CHECK TABLE FOR ROW SPEC                     
SSPE0040 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE?                                
         BE    SSPE0020            YES - ROW NOT NEEDED                         
         CLC   3(1,R2),0(R3)       ROW TYPE IN SPEC VS TABLE: FOUND?            
         BE    SSPE0060            YES - CHECK FURTHER                          
         LA    R3,LSPECTAB(R3)     NO  - BUMP TO NEXT ENTRY                     
         B     SSPE0040            GO BACK FOR NEXT                             
SSPE0060 EQU   *                                                                
         CLI   6(R2),0             ANY 'IF' WITH ROW?                           
         BZ    SSPE0080            NO  - ACCEPT ROW                             
         MVC   BYTE,6(R2)          SET 'IF' NUMBER                              
         BAS   RE,CHECKIF          CHECK FOR TRUE                               
         BNZ   SSPE0020            CONDITION NOT TRUE - SKIP IT                 
SSPE0080 EQU   *                                                                
         OC    SBBREAKS(1),1(R3)   SET BREAK LEVEL IN PARBLOCK                  
         B     SSPE0020            GO BACK FOR NEXT ROW                         
SSPE0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SPECTAB:  ITEMS IN REPORT SPEC WHICH MUST BE IDENTIFIED, IF                 
*        PRESENT, AND SET INTO PARBLOCK                                         
*        BYTE   1   =   ROW IDENTIFIER IN SPEC                                  
*        BYTE   2   =   VALUE TO SET INTO SBBREAKS IN PARBLOCK                  
*                                                                               
SPECTAB  DC    AL1(07),X'80'       GROUP/SUBGROUP                               
LSPECTAB EQU   *-SPECTAB                                                        
         DC    AL1(26),X'80'       SUBGROUP ONLY                                
         DC    AL1(04),X'40'       OFFICE                                       
         DC    AL1(02),X'20'       STATION                                      
         DC    AL1(32),X'20'       STA/MARKET                                   
         DC    AL1(24),X'01'       OWNER                                        
         DC    AL1(39),X'08'       MARKET                                       
         DC    AL1(03),X'04'       REGION                                       
         DC    AL1(06),X'02'       SALESPERSON                                  
         DC    X'0000'             DELIMITER                                    
         SPACE                                                                  
         EJECT                                                                  
*                                                                               
*   LDREQOPT:  LOAD OF REMAINDER OF PARBLOCK: FILTERS                           
*                                                                               
LDREQOPT NTR1                                                                   
         MVC   SBGROUP(LSBFILTS),SPACES                                         
         LA    R2,REQCARD0         SET A(REQUEST CARD 2)                        
         USING QREC2,R2                                                         
*                                                                               
         LA    R3,REQCARD1         SET A(REQUEST CARD 3)                        
         USING QREC3,R3                                                         
*                                                                               
         CLC   RNUM,=C'10'                                                      
         BNE   LDRE0008            NOT 10 REQUEST                               
*                                  NOW DECIDE IF WE HAVE A K NUMBER             
         LA    RF,RCON+6                                                        
         LA    RE,7                                                             
LDRE0002 CLI   0(RF),C'0'                                                       
         BL    LDRE0008            NOT NUMERIC                                  
         CLI   0(RF),C'9'                                                       
         BH    LDRE0008            NOT NUMERIC                                  
         BCTR  RF,0                                                             
         BCT   RE,LDRE0002                                                      
*                                                                               
         PACK  DUB,RCON                                                         
         LM    RE,RF,DUB                                                        
         SRDL  RE,4                                                             
         L     RE,=X'99999999'                                                  
         SR    RE,RF               RE= 9'S K NUM                                
*                                                                               
         XC    KEY,KEY             BUILD 8C CONTRACT KEY                        
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),RREP                                                   
         STCM  RE,15,KEY+23                                                     
         GOTO1 (RFGETREC,AREPFACS),DMCB,KEY,AIOWORK,0,RFBLOCK                   
         BNE   LDRE0008            NO                                           
         USING RCONREC,R4                                                       
         L     R4,AIOWORK                                                       
         MVC   SBGROUP(2),RCONKGRP                                              
         MVC   SBOFFICE(2),RCONKOFF                                             
         MVC   SBSTATN(5),RCONKSTA                                              
         MVC   SBSALES(3),RCONSAL                                               
*                                                                               
         XC    KEY,KEY             BUILD 02 STATION KEY                         
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),RCONKREP                                               
         MVC   KEY+22(5),RCONKSTA                                               
         DROP  R4                                                               
         GOTO1 (RFGETREC,AREPFACS),DMCB,KEY,AIOWORK,0,RFBLOCK                   
         BNE   LDRE0100            NO                                           
         USING RSTAREC,R4                                                       
         L     R4,AIOWORK                                                       
         MVC   SBOWNER(3),RSTAOWN                                               
         LA    R4,RSTAELEM                                                      
         DROP  R4                                                               
LDRE0003 CLI   0(R4),0             FIND X'08' ELEM                              
         BE    LDRE0100                                                         
         CLI   0(R4),X'08'                                                      
         BE    LDRE0004                                                         
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     LDRE0003                                                         
LDRE0004 DS    0H                                                               
         USING RSTAXXEL,R4                                                      
         MVC   SBMARKET(4),RSTAMKTC                                             
         B     LDRE0100                                                         
         DROP  R4                                                               
*                                                                               
LDRE0008 EQU   *                                                                
         CLI   RGROUP,C'*'         GROUP SET ENTERED?                           
         BNE   LDRE0010            NO                                           
         MVI   SBGROUP,C'*'        YES                                          
         MVC   SBGROUP+1(4),QSETGSG INSERT GROUP SET NAME                       
         B     LDRE0020                                                         
LDRE0010 EQU   *                                                                
         MVC   SBGROUP(2),RGROUP   INSERT GROUP CODE                            
LDRE0020 EQU   *                                                                
         CLI   ROFF,C'*'           OFFICE SET ENTERED?                          
         BNE   LDRE0030            NO                                           
         MVI   SBOFFICE,C'*'       YES                                          
         MVC   SBOFFICE+1(4),Q3OFFSET                                           
*                                  INSERT OFFICE SET NAME                       
         B     LDRE0040                                                         
LDRE0030 EQU   *                                                                
         MVC   SBOFFICE(2),ROFF    INSERT OFFICE CODE                           
LDRE0040 EQU   *                                                                
         CLI   RSTA,C'*'           STATION SET ENTERED?                         
         BNE   LDRE0050            NO                                           
         MVI   SBSTATN,C'*'        YES                                          
         MVC   SBSTATN+1(4),Q3STASET                                            
*                                  INSERT STATION SET NAME                      
         B     LDRE0060                                                         
LDRE0050 EQU   *                                                                
         MVC   SBSTATN(5),RSTA     INSERT STATION CODE                          
LDRE0060 EQU   *                                                                
         CLI   RSMAN,C'*'          S/P     SET ENTERED?                         
         BNE   LDRE0070            NO                                           
         MVI   SBSALES,C'*'        YES                                          
         MVC   SBSALES+1(4),QSETSAL                                             
*                                  INSERT S/P SET NAME                          
         B     LDRE0080                                                         
LDRE0070 EQU   *                                                                
         MVC   SBSALES(3),RSMAN    INSERT S/P CODE                              
LDRE0080 EQU   *                                                                
         MVC   SBREGION(2),ROFFR   INSERT REGION                                
         MVC   SBMARKET(4),Q2MARKET                                             
*                                  INSERT MARKET                                
         MVC   SBOWNER(3),Q2OWNER  INSERT OWNER                                 
LDRE0100 MVC   SBREPCDE,RREP       INSERT REP CODE                              
         XIT1                                                                   
         EJECT                                                                  
CHECKIF  NTR1                      BYTE HAS IF NUMBER                           
         LR    R5,R4               R5 = A(ORIGINAL SPEC)                        
         MVI   SPEC,60                                                          
*                                                                               
CHIF0020 BAS   RE,NEXTSPEC         FIND IF SPEC                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,2(R4)          TEST IF NUMBER                               
         BNE   CHIF0020                                                         
         ZIC   R1,3(R4)            OPTION NUMBER                                
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,CHIFTABL(R1)                                                  
         LA    RE,REQCARD          SET A(1ST REQUEST CARD)                      
         CLI   0(R1),2             TEST OPTION ON 2ND REQUEST CARD              
         BNE   *+8                                                              
         LA    RE,REQCARD+80       YES                                          
         ZIC   RF,1(R1)                                                         
         AR    RF,RE               RF = A(OPTION VALUE)                         
         ZIC   RE,1(R4)                                                         
         SH    RE,=H'6'            RE = LENGTH FOR COMPARE                      
         BNM   *+6                                                              
         DC    H'0'                                                             
         NI    CHIF0040+7,X'0F'    RESET MASK IN BRANCH INSTRTN                 
         OC    CHIF0040+7(1),4(R4) SET MASK                                     
         EX    RE,CHIF0040                                                      
         B     *+10                                                             
*                                                                               
CHIF0040 CLC   5(0,R4),0(RF)                                                    
         BC    0,CHIF0060         CONDITION SATISFIED                           
         LTR   RB,RB              RETURN CC NE 0 FALSE                          
         B     CHIF0080                                                         
*                                                                               
CHIF0060 CR    RB,RB              RETURN CC = 0  TRUE                           
*                                                                               
CHIF0080 EQU   *                                                                
         XIT1                                                                   
         SPACE 2                                                                
CHIFTABL DC    AL1(1),AL1(ROPTN-REQCARD)                                        
         DC    AL1(1),AL1(ROPTN2-REQCARD)                                       
         DC    AL1(1),AL1(ROPTN3-REQCARD)                                       
         DC    AL1(2),AL1(Q2OPT4-QREC2)                                         
         DC    AL1(1),AL1(RSEQ-REQCARD)                                         
         DC    AL1(1),AL1(RSGROUP-REQCARD)                                      
         DC    AL1(1),AL1(RACCTOPT-REQCARD)                                     
         DC    AL1(1),AL1(RREP-REQCARD)                                         
         EJECT                                                                  
*                                                                               
**>>>>                                                                          
*                                                                               
         SPACE                                                                  
GETSPEC  DS    0H                                                               
         CLC   0(1,R4),SPEC        START SEARCH AT THIS SPEC                    
         BER   RE                                                               
*                                                                               
NEXTSPEC DS    0H                  START SEARCH AT NEXT SPEC                    
         ST    R4,REGSTOR                                                       
         SR    R0,R0                                                            
*                                                                               
GS005    ICM   R0,1,1(R4)            LOOK AT NEXT SPEC                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    GS020               END OF SPECS                                 
         CLC   0(1,R4),SPEC        COMPARE TO SOUGHT SPEC CODE                  
         BE    GS010               FOUND                                        
         CLI   0(R4),10            ONLY LOOK IN THIS REPORT                     
         BE    GS020               STOP AT NEXT REPORT                          
         B     GS005                                                            
*                                                                               
GS010    CR    RB,RB               CC = 0    SPEC FOUND                         
         BR    RE                                                               
*                                                                               
GS020    L     R4,REGSTOR                                                       
         LTR   RB,RB               CC = 2    SPEC NOT FOUND                     
*                                                                               
GS900    BR    RE                                                               
         EJECT                                                                  
***>>>                                                                          
*                                                                               
SUBRELO  DS    A                                                                
REGSTOR  DS    F                                                                
SPACES   DC    80C' '                                                           
SPEC     DS    CL1                                                              
*                                                                               
         DS    0H                                                               
ERROR    EQU   *                   UNTIL WE NEED SPECIAL ERROR PROC             
         LTR   RD,RD               ^0 CC                                        
EXIT     XIT1                      GENERIC EXIT                                 
*                                                                               
         TITLE 'REREQ02 -- ERROR MESSAGES AND EQUATES'                          
*                                                                               
*- ERROR MESSAGES                                                               
*                                                                               
*                                                                               
*- ERROR MESSAGE NUMBER EQUATES (STANDARD  SGS FROM FILE)                       
*                                                                               
MISSING  EQU   1                   REQUIRED FIELD NOT ENTERED                   
*                                                                               
INVIPT   EQU   2                   INVALID INPUT                                
*                                                                               
NOTFOUND EQU   53                  RECORD NOT FOUND                             
*                                                                               
SECLOCK  EQU   55                  SECURITY LOCKOUT                             
*                                                                               
ENDBFRST EQU   64                  END DATE BEFORE START                        
*                                                                               
MAX12MON EQU   170                 MAXIMUM DATE RANGE IS 12 MONTHS              
*                                                                               
GRPCONF  EQU   180                 STATION NOT IN GRP/SUBGRP                    
*                                                                               
SALCONF  EQU   181                 SALESMAN NOT IN DIV/TEAM                     
*                                                                               
CLSCONF  EQU   182                 CATEGORY NOT IN THIS CLASS                   
*                                                                               
OFFSTA   EQU   242                 OFFICE OR STATION REQUIRED                   
*                                                                               
NEED12M  EQU   252                 DATE SPAN MUST BE 12 MONTHS                  
         TITLE 'REREQ02 -- GOINDEX - CALL RTN VIA INDEX'                        
         TITLE 'DOFILE -- WRITE A REQUEST TO FILE'                              
*                                                                               
         TITLE 'DOCHANGE -- CHANGE EXISTING REQUEST'                            
*                                                                               
*- DOCHANGE -- CHANGE EXISTING REQUEST ON REQUEST FILE                          
*                                                                               
DOCHANGE NTR1                                                                   
         MVC   KEY(4),DAONSCRN     DA TO READ                                   
*                                                                               
         GOTO1 NUMCARDS                                                         
         MVC   P6,P2               LENGTH OF RECORD FOR DMGR                    
*                                                                               
         GOTO1 ASETREQ             POINT TO RQST FILE                           
*                                                                               
         LA    RE,REQREC           SET ADDRESSABILITY TO REC2                   
         LA    RE,REQREC2-REQREC(RE)                                            
****>>>  LA    RE,REQREC2          READ INTO REC2                               
         ST    RE,AIOWORK                                                       
         MVI   DMINBTS,X'20'       PROVIDING WHOLE RECORD                       
*                                                                               
*- READ IN REQUEST FROM FILE                                                    
         GOTO1 DIRCHAIN                                                         
         BNZ   COMMBAD             ERROR                                        
*                                                                               
*- MOVE NEW REQUEST DATA TO OLD RECORD (ALL 11 CARDS, JUST IN CASE)             
         LA    R1,LREQCARD                                                      
         LA    RE,REQREC           SET ADDRESSABILITY TO REC2                   
         LA    RF,REQREC2-REQREC+80(RE)                                         
         PRINT GEN                                                              
         MOVE  ((RF),(R1)),REQCARD                                              
         PRINT NOGEN                                                            
***>>>   MOVE  (REQREC2+80,(R1)),REQCARD                                        
*                                                                               
*- WRITE BACK RECORD TO FILE                                                    
         GOTO1 DIRWRIT                                                          
         BNZ   COMMBAD             ERROR                                        
*                                                                               
         GOTO1 ASETREP             POINT BACK TO REP FILE                       
*                                                                               
         L     RE,AIOADDR2                                                      
         ST    RE,AIOWORK                                                       
         MVI   DMINBTS,X'00'                                                    
*                                                                               
         LA    RE,REQCHA                                                        
         LA    RF,L'REQCHA                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         B     COMMGOOD            GOOD CC                                      
*                                                                               
REQCHA   DC    C'REQUEST HAS BEEN CHANGED'                                      
         EJECT                                                                  
         TITLE 'COMMON XIT LABELS'                                              
*                                                                               
*- COMMON XIT AND COND CODE SETTING BRANCH LABELS                               
*                                                                               
COMMGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     COMMTEST                                                         
*                                                                               
COMMBAD  EQU   *                                                                
         LA    R0,1                                                             
*                                                                               
COMMTEST EQU   *                                                                
         LTR   R0,R0                                                            
COMMXIT  EQU   *                                                                
         XIT1                                                                   
*                                                                               
*- DOFILE -- WRITE REQUEST TO FILE.                                             
*                                                                               
DOFILE   NTR1                                                                   
         GOTO1 =A(DOFILNMD),DMCB,(R9),RR=SUBRELO                                
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*   DOFILNMD:  DOFILE MOVED FOR ADDRESSABILITY                                  
*                                                                               
DOFILNMD NTR1                                                                   
         MVI   FLAG10,C'N'         SET '10 REPORT NO' FLAG                      
         LA    RF,RQSNUMH          A(NUMBER FIELD IN HEADER)                    
         CLI   5(RF),2             TWO CHARACTERS ENTERED?                      
         BNE   DOFI0040            NO - CHECK FOR 3 CHARACTER INPUT             
         CLC   RQSNUM(2),=C'10'    CONTRACT PRINT REQUEST?                      
         BNE   DOFI0080            NO  -                                        
         MVI   FLAG10,C'Y'         SET '10 REPORT YES' FLAG                     
         B     DOFI0080            NO                                           
*                                                                               
DOFI0040 EQU   *                                                                
*                                                                               
         CLI   5(RF),3             THREE CHARACTERS ENTERED?                    
         BNE   DOFI0080            NO  - SET SPACING                            
         CLC   RQSNUM(2),=C'R10'   YES - CONTRACT PRINT REQUEST?                
         BNE   DOFI0080            NO  -                                        
         MVI   FLAG10,C'Y'         SET '10 REPORT YES' FLAG                     
DOFI0080 EQU   *                                                                
*                                                                               
         CLI   FLAG10,C'Y'         10 REPORT REQUEST?                           
         BNE   DOFI0120            NO  - DON'T BOTHER WITH REP REC              
*                                                                               
         MVC   SVTWAAGY,TWAAGY     SAVE ORIGINAL REP CODE ALWAYS                
         LA    RF,SUBREPS                                                       
         ST    RF,ASUBREP          SAVE A(SUBREPS FIELD)                        
         XC    SUBREPS,SUBREPS                                                  
         GOTO1 GETREP                                                           
*                                  CHECK IF REP A SUBSIDIARY                    
*                                     IF MASTER, SEVERAL OUTPUTS                
*                                        WILL BE GEN'D                          
         OC    SUBREPS,SUBREPS     ANY SUBREPS?                                 
         BNZ   DOFI0120            YES -                                        
         MVI   FLAG10,C'N'         NO  - SET 'NOT MASTER'                       
DOFI0120 EQU   *                                                                
         MVC   KEY(4),=XL4'AAAAAAAA' ADD RQST TO CHAIN?                         
         XC    DAONSCRN,DAONSCRN                                                
*                                                                               
         GOTO1 NUMCARDS                                                         
         MVC   P6,P2               LENGTH OF RECORD FOR DMGR                    
*                                                                               
         CLI   FLAG10,C'Y'         MASTER?                                      
         BNE   DOFI0140            NO                                           
         L     RF,ASUBREP          YES - REPLACE REP CODES                      
         MVC   RQHAGY,0(RF)        INSERT FROM SUBREP TABLE                     
         MVC   RREP,0(RF)                                                       
         MVC   TWAAGY,0(RF)        SET REP IN TWA                               
***      BAS   RE,GETREP2          RETRIEVE SUBREP REP RECORD                   
DOFI0140 EQU   *                                                                
         GOTO1 ASETREQ             POINT TO RQST FILE                           
*                                                                               
         LA    RE,REQREC                                                        
         ST    RE,AIOWORK                                                       
         MVI   DMINBTS,X'20'       PROVIDING WHOLE RECORD                       
*                                                                               
         GOTO1 DIRADD                                                           
         BNZ   DOFIBAD             ERROR                                        
*                                                                               
         CLI   FLAG10,C'Y'         MASTER?                                      
         BNE   DOFI0150            NO                                           
         MVC   TWAAGY,SVTWAAGY     RESTORE TWAAGY                               
*                                                                               
DOFI0150 EQU   *                                                                
         CLI   RTNFLAG,4           REQUEST FOR ADD/RFP?                         
         BE    DOFI0180            YES                                          
         GOTO1 ASETREP             POINT BACK TO REP FILE                       
*                                                                               
         CLI   FLAG10,C'N'         10 REQUEST?                                  
         BE    DOFI0160            NO  - SKIP MASTER/SUBSIDIARY TESTS           
         OC    SUBREPS,SUBREPS     MASTER REP REQUEST?                          
         BZ    DOFI0160            NO  - FINISHED                               
         L     RF,ASUBREP          LOAD A(SUBREP IN PROGRESS)                   
         LA    RF,2(RF)            BUMP TO NEXT SUBREP                          
         ST    RF,ASUBREP          PUT IT BACK                                  
         OC    0(2,RF),0(RF)       ANY ENTRY?                                   
         BNZ   DOFI0120            YES - PUT OUT ANOTHER RECORD                 
DOFI0160 EQU   *                                                                
         MVC   DAONSCRN,KEYSAVE    SAVE DISK ADDRESS                            
         L     RE,AIOADDR2                                                      
         ST    RE,AIOWORK                                                       
         MVI   DMINBTS,X'00'       PROVIDING WHOLE RECORD                       
*                                                                               
         LA    RE,PROCOVRN                                                      
         LA    RF,L'PROCOVRN                                                    
         STM   RE,RF,AERROR                                                     
         B     DOFI0200                                                         
DOFI0180 EQU   *                                                                
         MVC   PROCRFP+23(8),RQSOUT INSERT GROUP NAME                           
         LA    RE,PROCRFP                                                       
         LA    RF,L'PROCRFP                                                     
         STM   RE,RF,AERROR                                                     
DOFI0200 EQU   *                                                                
*                                                                               
         SR    R0,R0               SET CC = ZERO                                
         B     DOFI0800            GOOD CC                                      
DOFIBAD  EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
DOFI0800 EQU   *                                                                
         XIT1                                                                   
PROCOVRN DC    C'REQUEST WILL PROCESS OVERNIGHT'                                
*                0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0                                
PROCRFP  DC    C'REQUEST ADDED TO GROUP         '                               
                                                                                
*                                                                               
* LOOK AT REP RECORD TO SEE IF REP IS A SUBSIDIARY                              
* IF REP IS SUBSIDIARY, ROUTINE RETURNS COMMBAD WITH REP RECORD                 
*   IN IOWORK (SO CALLER HAS ACCESS TO RREPMAST)                                
GETREP   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           RETRIEVE REP RECORD                          
         MVC   KEY+25(2),TWAAGY    INSERT REP CODE                              
         GOTO1 DIRREAD                                                          
         GOTO1 FILREAD                                                          
*                                                                               
         L     RE,AIOADDR2                                                      
         USING RREPREC,RE                                                       
*                                                                               
         CLC   =X'0000',RREPMAST                                                
         BE    GETGOOD                                                          
         CLC   =X'4040',RREPMAST                                                
         BE    GETGOOD                                                          
         CLC   =X'FFFF',RREPMAST                                                
         BNE   GETBAD                                                           
         LA    RF,RREPELEM         FIND X'02' ELEMENT                           
GETR0020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
         CLI   0(RF),2             SUBSIDIARY REP LIST                          
         BE    GETR0040                                                         
         ZIC   R1,1(RF)            BUMP TO NEXT ELEMENT                         
         AR    RF,R1                                                            
         B     GETR0020            GO BACK FOR NEXT                             
GETR0040 EQU   *                                                                
         ZIC   R1,2(RF)            GET SUB COUNT                                
         SLA   R1,1                DOUBLE THE COUNT                             
         BCTR  R1,0                SUBTRACT 1 FOR EX                            
         EX    R1,GETR0100         MOVE SUBS BY LENGTH                          
         B     GETGOOD                                                          
GETR0100 MVC   SUBREPS(0),10(RF)                                                
*                                                                               
GETGOOD EQU    *                                                                
         SR    R0,R0                                                            
         B     GETTEST                                                          
*                                                                               
GETBAD   EQU   *                                                                
         LA    R0,1                                                             
*                                                                               
GETTEST EQU   *                                                                 
         LTR   R0,R0                                                            
         XIT1                                                                   
*                                                                               
         DROP  RE                                                               
         EJECT                                                                  
*                                                                               
*- NUMCARDS -- COUNT NUMBER OF CONTROL CARDS                                    
*                                                                               
*  RETURN: P1 = # CARDS (INCLUDING PRIMARY CARD)                                
*          P2 = LENGTH OF ALL CARDS + 80 BYTE HEADER                            
*                                                                               
NUMCARDS NTR1                                                                   
         SR    R1,R1               # CARDS                                      
         LA    RE,REQCARD                                                       
*                                                                               
NUMC100  EQU   *                                                                
         CLC   0(80,RE),SPACES4    ALL BLANK?                                   
         BE    NUMC200                                                          
*                                                                               
         CLC   80(80,RE),SPACES4   FORCE CONTINUATION CHAR IF NEXT              
         BE    NUMC150              CARD HAS DATA                               
         CLI   79(RE),CTINCHAR     ALREADY SET?                                 
         BE    NUMC150              YES                                         
         ZIC   RF,79(RE)           SAVE OFF WHAT'S THERE                        
         STC   RF,80(RE)            AND PUT IT ON THE NEXT CARD                 
         MVI   79(RE),CTINCHAR     AND FORCE THE CONTINUATION                   
NUMC150  EQU   *                                                                
*                                                                               
         LA    R1,1(R1)            BUMP CARD COUNT/ADDRESS                      
         LA    RE,80(RE)                                                        
*                                                                               
         LA    RF,REQREC                                                        
         LA    RF,LREQREC(RF)      A(END OF REQ RECORD)                         
         CR    RE,RF                                                            
         BL    NUMC100             DON'T EXCEED MAX RECORD.                     
*                                                                               
NUMC200  EQU   *                                                                
         ST    R1,P1               NUMBER OF CARDS (INCLUDING PRIMARY)          
         LA    RF,REQREC                                                        
         SR    RE,RF                                                            
         ST    RE,P2               LENGTH, INCLUDING REQ HEADER                 
*                                                                               
         XIT1                                                                   
SPACES4  DC    CL80' '                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   PARAMETER BLOCK FOR PERSONAL ACCESS RECORD USE                              
*                                                                               
PARBLOCK DS    0H                                                               
       ++INCLUDE REGENSBLK                                                      
*                                                                               
***>>>                                                                          
         DC    C'SPECBLOC'                                                      
SPECBLOC DS    20000C                                                           
LSPECBLC EQU   *-SPECBLOC                                                       
SPECEND  DS    0H                                                               
         DS    CL16                FINAL DELIMITER SPACE                        
*                                                                               
*                                                                               
DOSOON   NMOD1 0,DOSOON,RR=R5                                                   
         L     R9,0(R1)                                                         
         MVC   LOCRELO,4(R1)       SAVE SUBRELO                                 
*                                                                               
         USING REQWRK,R9                                                        
         L     RA,ASAVE                                                         
         USING TWAD,RA                                                          
         USING SPOOK,R2                                                         
         XC    TEMP(SPOOKL),TEMP   BUILD SPOOK BLOCK                            
         LA    R2,TEMP                                                          
         MVC   SPOOKUID,TWAUSRID   CONNECT ID                                   
         MVC   SPOOKTID,TWATRM     TERMINAL ID                                  
*                                                                               
         MVC   SPOOKAGY,TWAAGY     TWO CHARACTER ID CODE                        
*                                                                               
*- GET 1ST 3 CHARS FROM REQUESTOR FOR INITIALS.                                 
         MVC   SPOOKDID,RQSNAME    USER INITIALS (ID)                           
         OC    SPOOKDID,=CL3' '                                                 
         CLI   SPOOKDID+2,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+2,C'*'                                                  
*                                                                               
         CLI   SPOOKDID+1,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+1,C'*'                                                  
*                                                                               
         MVC   SPOOKSYS,=C'RE'     REP SYSTEM                                   
         MVC   SPOOKEOD,RNUM                                                    
         MVC   SPOOKJCL,RNUM                                                    
         MVI   SPOOKWEN,2          SET SOON STATUS                              
*                                                                               
         L     R4,APARM                                                         
         L     R4,16(R4)           A(COMFACS)                                   
*                                                                               
*- WRITE OUT LINKED REQUEST                                                     
         GOTO1 =V(REQTWA),DMCB,(5,(RA)),REQREC+54,DATAMGR,(R4),(R2),   X        
               RR=R5                                                            
*                                                                               
         L     RE,8(R1)            GET A(PRTQUE) KEY                            
         SR    R0,R0                                                            
         ICM   R0,3,6(RE)          NON-0 IS GOOD.                               
         BNZ   SOON20                                                           
*                                                                               
         L     RE,=A(NOJCL)        JCL BOOK NOT FOUND                           
         A     RE,LOCRELO                                                       
         LA    RF,L'NOJCL                                                       
         STM   RE,RF,AERROR                                                     
         LA    RE,RQSNUMH                                                       
         ST    RE,CURSPOS                                                       
         B     DSONBAD             SET NON-ZERO C.C.                            
*                                                                               
SOON20   XC    RQSMSG,RQSMSG                                                    
         MVC   RQSMSG(39),=C'REPORT XXX,99999 WILL BE PROCESSED SOON'           
         MVC   RQSMSG+7(3),2(RE)                                                
         LH    RF,6(RE)                                                         
         LA    R4,RQSMSG+11                                                     
         EDIT  (RF),(5,(R4)),ALIGN=LEFT                                         
*                                                                               
         XC    AERROR,AERROR       SET FOR MY MESSAGE                           
*                                                                               
DSONGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     DSONEXIT                                                         
DSONBAD  EQU   *                                                                
         LA    R0,1                                                             
DSONEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
NOJCL    DC    C'JCL BOOK NOT ON FILE.  CONTACT DDS.'                           
LOCRELO  DS    A                                                                
*                                                                               
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'WORK AREA'                                                      
*                                                                               
*- REQTWA, REQWRK AND REGENREQ2                                                 
*        PRINT OFF                                                              
       ++INCLUDE REREQTWA                                                       
       ++INCLUDE REREQWRK                                                       
         PRINT ON                                                               
         SPACE                                                                  
*                                                                               
*- LOCAL WORK AREA                                                              
         ORG   USERWORK                                                         
LOCALWRK EQU   *        <----------INSERT VARIABLES BELOW THIS LABEL            
*                                                                               
AMAPNTRY DS    A                   A(CURRENT REQMAP ENTRY)                      
*                                                                               
SCANBLOK DS    0C                  SCANNER OUTPUT BLOCK                         
SCANBLK1 DS    CL32                (INLUDE 1 32-BYTE ENTRY FOR EACH             
SCANBLK2 DS    CL32                 POSSIBLE PARAMETER)                         
SCANBLK3 DS    CL32                                                             
SCANBLK4 DS    CL32                                                             
SCANBLK5 DS    CL32                                                             
SCANBLK6 DS    CL32                                                             
SCANBLK7 DS    CL32                                                             
SCANBLK8 DS    CL32                                                             
LSCANBLK EQU   *-SCANBLOK                                                       
*                                                                               
SCANCARD DS    CL80                SCANNER INPUT CARD                           
*                                                                               
TEMP     DS    CL30                SPOOK WORK AREA                              
*                                                                               
TODAY    DS    CL6                 SYSTEM DATE.  YYMMDD                         
*                                                                               
DEMOVAL  DS    A                   DEMO VALIDATION ROUTINE                      
DBDEMOB  DS    288C                DEMO BLOCK AREA                              
DEMWORK  DS    CL30                DEMO OUTPUT AREA                             
*                                                                               
GETFACT  DS    A                   A(GETFACT ROUTINE)                           
VSWITCH  DS    F                   A(SWITCH ROUTINE)                            
SRCEREP  DS    CL2                                                              
TRGTREP  DS    CL3                                                              
SRCEUTL  DS    CL1                                                              
ORIGUTL  DS    CL1                                                              
SAVSTDAT DS    CL3                                                              
SRCESTAT DS    CL8                                                              
BEFFDATE DS    CL3                 EFFECTIVE DATE: BINARY                       
EFDTCOMP DS    CL2                 EFFECTIVE DATE: COMPRESSED                   
WORKREP  DS    CL64                                                             
SUBREPS  DS    CL36                18 SUB REP CODES                             
FLAG10   DS    CL1                                                              
SVTWAAGY DS    CL2                                                              
ASUBREP  DS    F                                                                
RTNFLAG  DS    CL1                 ENTRY   FLAG FOR 05 MODULE                   
*                                                                               
LOCALWKX EQU   *        <----------INSERT VARIABLES ABOVE THIS LABEL            
AMTLOCAL EQU   USERWRKX-LOCALWKX  AMOUNT LOCAL WRK AREA                         
         DS    (AMTLOCAL)X                                                      
         SPACE 2                                                                
*                                                                               
*- RECORD DSECTS FOLLOW                                                         
*                                                                               
*  RECORD      ORG                                                              
*  --------    ------                                                           
*  REGENREG    IOWORK                                                           
*  REGENOFF    IOWORK                                                           
*  REGENSTA    IOWORK                                                           
*  REGENADV    IOWORK                                                           
*  REGENPRD    IOWORK                                                           
*  REGENAGY    IOWORK                                                           
*  REGENTEM    IOWORK                                                           
*  REGENSAL    IOWORK                                                           
*  REGENCTG    IOWORK                                                           
*  REGENCLS    IOWORK                                                           
*  REGENCON    IOWORK                                                           
*  REGENGRP    IOWORK                                                           
*  REGENOWN    IOWORK                                                           
*  REGENRDA    IOWORK                                                           
*  REGENREPA   IOWORK                                                           
*  REGENEOM    IOWORK                                                           
*                                                                               
         ORG   IOWORK                                                           
       ++INCLUDE REGENREG                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENOFF                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENSTA                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENADV                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENPRD                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENAGY                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENTEM                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENSAL                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENDSP                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENDCT                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENCTG                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENCLS                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENCON                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENGRP                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENOWN                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENRDA                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENREPA                                                      
         ORG   IOWORK                                                           
       ++INCLUDE REGENEOM                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENLAB                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENSET                                                       
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
REQ2D    DSECT                                                                  
       ++INCLUDE REGENREQ2                                                      
         SPACE 2                                                                
REQ3D    DSECT                                                                  
       ++INCLUDE REGENREQ3                                                      
         SPACE 2                                                                
*                                                                               
*  CONTROL BLOCK FOR DEMOGRAPHIC REQUIREMENTS                                   
*  DSECT TO COVER DEMO INTERFACE MODULE STORAGE                                 
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 5                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'149REREQ05   05/01/02'                                      
         END                                                                    
