*          DATA SET RECNT4A    AT LEVEL 053 AS OF 12/30/97                      
*PHASE T8024AA,+0                                                               
         TITLE 'T8024A - REP ORDER DISPLAY/EDIT - COMBO ORDER'                  
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT4A (T8024A) --- ORDER DISPLAY/EDIT - COMBO ORDER    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 30JUL92 (BU ) --- ORIGINAL ENTRY                                *             
*                   SCREEN CHANGED FROM T802F0 TO T802EO          *             
*                                                                 *             
* 28OCT94 (SKU) --- EXPAND X'20' ELEMENT TO SAVE LAST 3 STA/REP   *             
*                   VERSION DATES                                 *             
*                                                                 *             
* 18JAN95 (SKU) --- CALL REGENVER TO MARK CONTRACT UNCONFIRMED    *             
*                   AND BUMP VERSION NUMBERS                      *             
*                   SHORT X'9F' ELEMENT                           *             
*                                                                 *             
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 03MAR97 (RHV) --- FULL SIZE ORD SCREEN                          *             
*                                                                 *             
* 23JUL97 (SKU) --- 4K CONTRACT SUPPORT                           *             
*                                                                 *             
* 30DEC97 (RHV) --- ORD=NNNNNNNN                                  *             
*******************************************************************             
*                                                                               
T8024A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8024A,R9,RR=R5                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         L     R2,4(R1)            SET A(OPTION) BEFORE SETCOMBO                
*                                                                               
         BAL   RE,SETCOMBO         RETRIEVE AND SET COMBO ELEMENT               
*                                                                               
         CLC   =C'DISP',0(R2)                                                   
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)                                                   
         BE    EDIT0000                                                         
         DC    H'0'                                                             
         EJECT                                                                  
DISP     DS    0H                                                               
         GOTO1 VFOUTBLK,DMCB,OR2TRFH,OR2LAST,0                                  
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   DISP10                                                           
                                                                                
         LA    R4,4                                                             
         LA    R2,OR2TOT1H         TOTALS                                       
DISP05   OI    1(R2),X'20'         STATION CANNOT CHANGE THESE FLDS             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R4,DISP05                                                        
                                                                                
         LA    R2,OR2ASSTH         SALES ASSISTANT                              
         OI    1(R2),X'20'         OR THIS FIELD                                
         B     DISP20                                                           
*                                                                               
DISP10   LA    R2,OR2ADVH          ADVERTISER                                   
         OI    1(R2),X'20'         REP SHOULD NOT CHANGE THIS FIELD             
         LA    R2,OR2AGYH          AGENCY                                       
         OI    1(R2),X'20'         OR THIS FIELD                                
         LA    R2,OR2TRFH          TRAFFIC NUMBER                               
         OI    1(R2),X'20'         OR THIS ONE                                  
*                                                                               
DISP20   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAL   RE,GETEL                                                         
         BNE   DISP40                                                           
         USING RCONXEL,R6                                                       
         SPACE 1                                                                
         LA    R2,OR2TOT1H         TOTAL                                        
*                                                                               
         OC    RCONTOT,RCONTOT                                                  
         BZ    DISP30                                                           
         EDIT  (4,RCONTOT),(12,OR2TOT1),2,ALIGN=LEFT,FLOAT=-                    
         SPACE 1                                                                
DISP30   LA    R2,OR2TRFH          TRAFFIC NUMBER                               
*                                                                               
         OC    RCONTRF,RCONTRF                                                  
         BZ    *+10                                                             
         MVC   8(L'RCONTRF,R2),RCONTRF                                          
         DROP  R6                                                               
*                                                                               
DISP40   LA    R2,OR2ADVH          ADVERTISER                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAL   RE,GETEL                                                         
         BNE   DISP50                                                           
         USING RCONXXEL,R6                                                      
         OC    RCONXADV,RCONXADV   IS THERE ADVERTISER                          
         BZ    *+10                                                             
         MVC   8(L'RCONXADV,R2),RCONXADV                                        
*                                                                               
         LA    R2,OR2AGYH          AGENCY                                       
         OC    RCONXAGY,RCONXAGY                                                
         BZ    *+10                                                             
         MVC   8(L'RCONXAGY,R2),RCONXAGY                                        
*                                                                               
         LA    R2,OR2ASSTH         SALES ASSISTANT                              
         OC    RCONXAST,RCONXAST                                                
         BZ    *+10                                                             
         MVC   8(L'RCONXAST,R2),RCONXAST                                        
         DROP  R6                                                               
*                                                                               
DISP50   LA    R2,OR2RCMTH         REP ORDER COMMENT                            
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   DISP63                                                           
         LA    R5,10               FOR BCT LOOP                                 
DISP60   OI    1(R2),X'20'         STATION SHOULD NOT CHANGE THIS FIELD         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               POINT TO NEXT CMT LINE                       
         BCT   R5,DISP60                                                        
*                                                                               
DISP63   LA    R2,OR2RCMTH         CLEAR ORD CMNT FIELDS BEFORE REDISP          
         LA    R5,10               FOR BCT LOOP                                 
DISP65   XC    8(L'OR2RCMT,R2),8(R2)                                            
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT FIELD                                   
         BCT   R5,DISP65                                                        
*                                                                               
         LA    R2,OR2RCMTH         BACK TO 1ST REP ORDER CMT                    
*                                                                               
DISP70   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'82'                                                     
         BAL   RE,GETEL                                                         
         BNE   DISP90                                                           
         LA    R5,10               FOR BCT LOOP                                 
DISP80   MVC   WORK3(80),MYSPACES                                               
         ZIC   R3,1(R6)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK3(0),2(R6)                                                   
         SPACE 1                                                                
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK3                                                    
         SPACE 1                                                                
*                                                                               
         L     R7,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(1,(R2)),(R7),DATAMGR,RCONREC,GETTXT               
         BZ    *+10                                                             
         MVC   22(24,R2),=C'***CMNT REC NOT FOUND***'                           
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT FIELD                                   
         BAL   RE,NEXTEL                                                        
         BNE   DISP90                                                           
         BCT   R5,DISP80                                                        
         SPACE 1                                                                
DISP90   LA    R2,OR2SCMTH         STATION ORDER COMMENT                        
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    DISP103                                                          
         LA    R5,9                FOR BCT LOOP                                 
DISP100  OI    1(R2),X'20'         REP SHOULD NOT CHANGE THIS FIELD             
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               POINT TO NEXT STATION ORDER CMT              
         BCT   R5,DISP100                                                       
*                                                                               
DISP103  LA    R2,OR2SCMTH         CLEAR STA CMNT FIELDS BEFORE REDISP          
         LA    R5,9                FOR BCT LOOP                                 
DISP105  XC    8(L'OR2SCMT,R2),8(R2)                                            
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT FIELD                                   
         BCT   R5,DISP105                                                       
*                                                                               
         LA    R2,OR2SCMTH         BACK TO 1ST STATION ORDER CMT                
*                                                                               
DISP110  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'92'                                                     
         BAL   RE,GETEL                                                         
         BNE   DISP130                                                          
         LA    R5,9                                                             
DISP120  MVC   WORK3(80),MYSPACES                                               
         ZIC   R3,1(R6)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK3(0),2(R6)                                                   
         SPACE 1                                                                
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK3                                                    
         SPACE 1                                                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT FIELD                                   
         BAL   RE,NEXTEL                                                        
         BNE   DISP130                                                          
         BCT   R5,DISP120                                                       
         SPACE 1                                                                
DISP130  EQU   *                                                                
*                                                                               
         BAL   RE,OTHRTOTS         INSERT OTHER TOTALS INTO SCREEN              
*                                                                               
         GOTO1 VFOUTBLK,DMCB,OR2TOT1H,OR2LAST,1                                 
*                                                                               
         LA    R2,OR2TOT1H                                                      
         LA    R5,OR2TABH                                                       
         SR    R1,R1               MARK ALL FIELDS                              
         OI    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         IC    R1,0(R2)                                                         
         LA    R2,0(R1,R2)                                                      
         CR    R2,R5                                                            
         BL    *-14                                                             
         SPACE 1                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  OTHRTOTS:  RETRIEVES OTHER CONTRACTS IN COMBO ORDER AND INSERTS              
*    THE ORDER TOTALS INTO THE PROPER SLOTS                                     
*                                                                               
OTHRTOTS NTR1                                                                   
         MVI   STACTR,2            SET STATION COUNTER TO 1ST 'OTHER'           
OTHR0010 EQU   *                                                                
         GOTO1 NEXTCON#,DMCB,0     RETRIEVE NEXT CONTRACT NUMBER                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'        RETRIEVE ORDER ELEMENT                       
         BAL   RE,GETEL                                                         
         BNE   OTHR0030            NOT FOUND                                    
         USING RCONXEL,R6                                                       
*        SPACE 1                                                                
         LA    R2,OR2TOT2          TOTAL                                        
         CLI   STACTR,2            1ST OTHER STATION?                           
         BE    OTHR0020            YES                                          
         LA    R2,OR2TOT3          TOTAL                                        
         CLI   STACTR,3            2ND OTHER STATION?                           
         BE    OTHR0020            YES                                          
         LA    R2,OR2TOT4          TOTAL                                        
         CLI   STACTR,4            3RD OTHER STATION?                           
         BE    OTHR0020            YES                                          
         DC    H'0'                                                             
*                                                                               
OTHR0020 EQU   *                                                                
         OC    RCONTOT,RCONTOT                                                  
         BZ    OTHR0030                                                         
         PRINT GEN                                                              
         EDIT  (4,RCONTOT),(12,(R2)),2,ALIGN=LEFT,FLOAT=-                       
         PRINT NOGEN                                                            
*                                                                               
OTHR0030 EQU   *                                                                
         ZIC   RF,STACTR           BUMP STATION IN PROGRESS                     
         LA    RF,1(RF)                                                         
         STC   RF,STACTR                                                        
         CLC   STACTR,CONCTR       ALL CONTRACTS DONE?                          
         BNH   OTHR0010            NO  - GO BACK AND DO NEXT                    
         LR    RF,RA               YES - RELOAD ORIGINAL CONTRACT               
         AH    RF,=H'4096'         4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         B     EXXMOD                                                           
         EJECT                                                                  
EDIT0000 DS    0H                                                               
         MVI   UPVER,0             ACE/GRAPH FLAG-UP VERS & UNCONF              
         MVI   STACTR,1            SET TO 1ST COMBO CONT#                       
         CLC   CONACT,=C'ADDO'                                                  
         BE    EDIT0010                                                         
         LR    RF,RA                                                            
         AH    RF,=H'4096'         4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
***>     MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         SPACE 1                                                                
EDIT0010 DC    0H'0'                                                            
         CLI   TWAACCS,C'$'        STATION                                      
         BE    EDIT0130                                                         
         SPACE 1                                                                
* DEAL WITH REP INPUT FIELDS - TOTAL AND REP ORDER COMMENT                      
         SPACE 1                                                                
         BAL   RE,EDITTOTS                                                      
         L     R2,FULL             SET A(FIELD) FOR ERROR                       
         LA    R3,2                INVALID INPUT FIELD                          
         BNZ   ERROR               ERROR RETURN                                 
*                                                                               
         CLC   CONACT,=C'ADDO'     ADD/ORDER OPTION?                            
         BNE   EDIT0020            NO  - USE STACTR FOR LOOP                    
         LR    RF,RA               YES - RELOAD ORIGINAL CONTRACT               
         AH    RF,=H'4096'         4K                                           
         USING TWAWORK,RF                                                       
         MVC   STACTR,TWACMBPT     YES - USE TWA ORDER POINTER INSTEAD          
         DROP  RF                                                               
         B     EDIT0030                                                         
EDIT0020 EQU   *                                                                
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BE    EDIT0030            YES                                          
         GOTO1 NEXTCON#,DMCB,1                                                  
         CLI   TWAACCS,C'$'        STATION                                      
         BE    EDIT0130                                                         
EDIT0030 EQU   *                                                                
         XC    FULL,FULL                                                        
         LA    R2,OR2TOT1H         TOTAL                                        
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BE    EDIT0038            YES                                          
         LA    R2,OR2TOT2H         TOTAL                                        
         CLI   STACTR,2            2ND STATION IN PROGRESS?                     
         BE    EDIT0038            YES                                          
         LA    R2,OR2TOT3H         TOTAL                                        
         CLI   STACTR,3            3RD STATION IN PROGRESS?                     
         BE    EDIT0038            YES                                          
         LA    R2,OR2TOT4H         MUST BE 4TH STATION                          
EDIT0038 EQU   *                                                                
         CLC   CONACT,=C'ADDO'     ADD/ORDER OPTION?                            
         BE    EDIT0039            YES - USE STACTR FOR LOOP                    
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    EDIT0060                                                         
EDIT0039 EQU   *                                                                
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         CLI   5(R2),0             BLANK                                        
         BE    EDIT0040                                                         
         SPACE 1                                                                
         LA    R3,2                INVALID INPUT FIELD                          
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)     FIELD LENGTH                                 
         LR    R6,R2                                                            
         LA    R6,8(R6)            DATA FIELD                                   
         GOTO1 CASHVAL,DMCB,(R6)                                                
         SPACE 1                                                                
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVC   FULL,DMCB+4                                                      
         SPACE 1                                                                
EDIT0040 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAL   RE,GETEL                                                         
         BNE   EDIT0050                                                         
         USING RCONXEL,R6                                                       
         MVC   RCONTOT,FULL        ORDER TOTAL                                  
         B     EDIT0060                                                         
         DROP  R6                                                               
         SPACE 1                                                                
EDIT0050 CLI   5(R2),0             IF 0, DON'T BOTHER WITH ELEMENT              
         BE    EDIT0060                                                         
         XC    WORK2,WORK2         BUILD NEW ELEMENT IN WORK2                   
         MVC   WORK2(2),=X'1F18'                                                
         MVC   WORK2+18(4),FULL    ORDER TOTAL                                  
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         SPACE 1                                                                
EDIT0060 EQU   *                                                                
         CLC   CONACT,=C'ADDO'     ADD/ORDER OPTION?                            
         BNE   EDIT0065            NO  - PROCESS NORMALLY                       
         CLI   STACTR,4            YES - 1ST 'ADDO' ORDER?                      
         BNE   EXXMOD              NO  - RECORD COMPLETE - EXIT                 
EDIT0065 LA    R2,OR2ASSTH         SALES ASSISTANT                              
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    EDIT0080                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAL   RE,GETEL                                                         
         BNE   EDIT0070                                                         
         USING RCONXXEL,R6                                                      
         MVC   RCONXAST,8(R2)                                                   
         B     EDIT0080                                                         
         DROP  R6                                                               
*                                                                               
EDIT0070 XC    WORK2(40),WORK2                                                  
         MVC   WORK2(2),=X'9F28'                                                
         MVC   WORK2+22(9),8(R2)                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
EDIT0080 LA    R2,OR2RCMTH         REP ORDER CMT                                
         SR    R1,R1                                                            
         LA    R5,10                                                            
EDIT0090 TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BNO   EDIT0100            IF ANY LINE CHANGES, REVALIDATE ALL          
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R5,EDIT0090                                                      
         B     EDIT0240                                                         
         SPACE 1                                                                
*                 DELETE ALL REP COMMENT ELEMENTS                               
EDIT0100 GOTO1 VDELELEM,DMCB,(X'82',RCONREC)                                    
         SPACE 1                                                                
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         LA    R2,OR2RCMTH         REP ORDER COMMENT                            
*                                                                               
         GOTO1 =A(GETORD),RR=YES   COPY CMT FROM OTHER K?                       
         BNZ   EDIT0240                                                         
*                                                                               
         LA    R5,10                                                            
EDIT0110 CLI   5(R2),0                                                          
         BE    EDIT0120                                                         
*                                                                               
         L     R7,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,(R2)),(R7),DATAMGR,RCONREC,GETTXT               
         BZ    *+12                                                             
         L     RD,BASERD           ERROR RETURN CONTROL TO USER                 
         B     EXXMOD                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),8(R2)    DATA TO ELEMENT                              
         AH    R1,=H'3'                                                         
         STC   R1,WORK2+1                                                       
         MVI   WORK2,X'82'         REP ORDER COMMENT                            
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
EDIT0120 ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R5,EDIT0110                                                      
         SPACE 1                                                                
         B     EDIT0240                                                         
         EJECT                                                                  
*  DEAL WITH STATION FIELDS - TRAFFIC NO., ADV, AGY AND STA ORD CMT             
* CHANGE TO TRAFFIC NO, ADV OR AGY DOES NOT UP VERSION OR UNCONFIRM             
         SPACE 1                                                                
EDIT0130 LA    R2,OR2TRFH          TRAFFIC NUMBER                               
         TM    4(R2),X'20'         VALIDATED PREVOUSLY                          
         BO    EDIT0140                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAL   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6          UPDATE ELEMENT                               
         CLI   5(R2),0             IF NO DATA NOW                               
         BNE   *+14                                                             
         XC    RCONTRF,RCONTRF     ERASE WHAT WAS THERE                         
         B     EDIT0140                                                         
*                                                                               
         MVC   RCONTRF,8(R2)       TRAFFIC NUMBER                               
         DROP  R6                                                               
         SPACE 1                                                                
EDIT0140 LA    R2,OR2ADVH          ADVERTISER                                   
         XC    WORK(20),WORK                                                    
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    EDIT0150                                                         
*                                                                               
         CLI   5(R2),0             NOTHING THERE NOW                            
         BNE   *+14                                                             
         MVC   WORK(10),MYSPACES   INDICATE DATA CHANGED                        
         B     EDIT0150                                                         
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
*                                                                               
EDIT0150 LA    R2,OR2AGYH                                                       
         TM    4(R2),X'20'         VLAIDATED PREVIOUSLY                         
         BO    EDIT0160                                                         
*                                                                               
         CLI   5(R2),0             NOTHING THERE NOW                            
         BNE   *+14                                                             
         MVC   WORK+10(10),MYSPACES INDICATED DATA CHANGED                      
         B     EDIT0160                                                         
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+10(0),8(R2)                                                 
*                                                                               
EDIT0160 OC    WORK(20),WORK                                                    
         BZ    EDIT0180            NOTHING HAS CHANGED                          
*                                                                               
         DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAL   RE,GETEL                                                         
         BNE   EDIT0170                                                         
*                                                                               
         USING RCONXXEL,R6                                                      
         OC    WORK(10),WORK       DID ADV CHANGE                               
         BZ    *+10                                                             
         MVC   RCONXADV,WORK       YES                                          
*                                                                               
         OC    WORK+10(10),WORK+10 DID AGY CHANGE                               
         BZ    *+10                                                             
         MVC   RCONXAGY,WORK+10    YES                                          
         DROP  R6                                                               
         B     EDIT0180                                                         
*                                                                               
EDIT0170 XC    WORK2(40),WORK2                                                  
         MVC   WORK2(2),=X'9F28'                                                
         MVC   WORK2+2(20),WORK                                                 
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
EDIT0180 LA    R2,OR2SCMTH         STATION ORDER CMT                            
         SR    R1,R1                                                            
         LA    R5,9                                                             
EDIT0190 TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BNO   EDIT0200            IF ANY LINE CHANGES, REVALIDATE ALL          
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R5,EDIT0190                                                      
         B     EDIT0240                                                         
         SPACE 1                                                                
*              NO CHANGES ALLOWED IF STATION WAS LAST TO SEND                   
         SPACE 1                                                                
EDIT0200 DS    0H                                                               
*           DELETE ALL STATION ORDER COMMENTS                                   
         GOTO1 VDELELEM,DMCB,(X'92',RCONREC)                                    
         SPACE 1                                                                
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         LA    R2,OR2SCMTH         STATION ORDER COMMENT                        
         LA    R5,9                                                             
EDIT0220 CLI   5(R2),0                                                          
         BE    EDIT0230                                                         
         SPACE 1                                                                
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),8(R2)    DATA TO ELEMENT                              
         AH    R1,=H'3'                                                         
         STC   R1,WORK2+1                                                       
         MVI   WORK2,X'92'         STATION ORDER COMMENT                        
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
EDIT0230 ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R5,EDIT0220                                                      
         EJECT                                                                  
EDIT0240 CLC   CONACT,=C'ADDO'     IF ADDING CONTRACT & ORDER RECORD,           
         BE    EXXMOD              T80211 ADDS REC X'1F' X'20' ELTS             
         SPACE 2                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    EDIT0310                                                         
         SPACE 1                                                                
*                                                                               
*  FOR ACE/GRAPHNET CONTRACTS ONLY -                                            
*    IF JUST ADDING/CHANGING ORDER RECORD, UNCONFIRM CONTRACT                   
*    AND UPDATE VERSION NUMBER IF NECESSARY                                     
* OTHERWISE, JUST PUT CHANGED RECORD                                            
*                                                                               
         CLI   UPVER,1   DO WE NEED TO UPDATE VERS NUMBER & UNCONFIRM           
         BNE   EDIT0320                                                         
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAL   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THERE SHOULD ALREADY BE A '1F'               
                                                                                
         USING RCONXEL,R6                                                       
                                                                                
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    *+12                                                             
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
         OI    RCONCONF,X'80'      TURN ON NOT CONFIRMED                        
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAL   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THERE SHOULD BE A SEND ELEMENT               
                                                                                
         USING RCONSEND,R6                                                      
                                                                                
         CLI   TWAACCS,C'$'        STATION                                      
         BE    EDIT0260                                                         
*                                                                               
* CAN ONLY MAKE CHANGES IF STATION VERSION NOT ADVANCED                         
*                                                                               
         TM    RCONSENF,X'10'      STA VERSION NOT ADVANCED                     
         BO    EDIT0250                                                         
         LA    R3,167              LATEST STATION VERSION NOT YET SENT          
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
EDIT0250 TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BZ    EDIT0320                                                         
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC),(X'80',WORK)                        
         BNZ   ERROR                                                            
         B     EDIT0280                                                         
*                                                                               
* CAN ONLY MAKE CHANGES IF REP VERSION NOT ADVANCED                             
*                                                                               
EDIT0260 TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BO    EDIT0270                                                         
         LA    R3,168              LATEST REP VERSION NOT YET SENT              
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
EDIT0270 TM    RCONSENF,X'10'      STATION VERSION NOT ADVANCED                 
         BZ    EDIT0320                                                         
                                                                                
* ADVANCE STA VERSION AND UPDATE VERSION DATES                                  
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         GOTO1 VREGENVR,DMCB,(C'S',RCONREC),(X'80',WORK)                        
         BNZ   ERROR                                                            
*                                                                               
EDIT0280 DS    0H                                                               
         LA    R6,RCONREC          RESET ELEMENT POINTER                        
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
         CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION NUMBER                   
         BH    EDIT0290                                                         
         EDIT  (1,RCONSSV),(3,CONMOD+8),ALIGN=LEFT                              
         B     EDIT0300                                                         
EDIT0290 EDIT  (1,RCONSRV),(3,CONMOD+8),ALIGN=LEFT                              
EDIT0300 MVC   CONMOD(7),=C'WIP VER'                                            
         TM    RCONMODR+1,X'40'    GRAPHNET                                     
         BZ    *+10                                                             
         MVC   CONMOD(7),=C'ESL VER'                                            
         FOUT  CONMODH                                                          
         B     EDIT0320                                                         
         SPACE 1                                                                
* NON-ACE/GRAPHNET CONTRACTS NEED TO HAVE THEIR MOD NUMBER BUMPED               
         SPACE 1                                                                
EDIT0310 DS    0H                                                               
         BAL   RE,BUMPNUM                                                       
         SPACE 1                                                                
* SHOW MOD NUMBER ON SCREEN                                                     
         SPACE 1                                                                
         MVC   CONMOD,MYSPACES                                                  
         CLI   RCONMOD,0                                                        
         BE    EDIT0320                                                         
         MVI   HALF,0                                                           
         MVC   HALF+1(1),RCONMOD                                                
         CLI   HALF+1,250                                                       
         BL    *+8                                                              
         MVI   HALF,255                                                         
         EDIT  (2,HALF),(3,CONMOD+8),ALIGN=LEFT,FLOAT=-                         
         MVC   CONMOD(7),=C'MOD NUM'                                            
         FOUT  CONMODH                                                          
         SPACE 1                                                                
EDIT0320 DS     0H                 RE-READ K FOR PUTREC                         
         MVC   KEY(27),RCONKEY                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         ZIC   RF,STACTR           CONT# IN PROGRESS                            
         LA    RF,1(RF)                                                         
         STC   RF,STACTR                                                        
         CLC   STACTR,CONCTR       ALL CONTRACTS PROCESSED?                     
         BNH   EDIT0020            NO  - GO BACK FOR NEXT                       
         LR    RF,RA               YES - RELOAD ORIGINAL CONTRACT               
         AH    RF,=H'4096'         4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         B     EXXMOD                                                           
         EJECT                                                                  
* ROUTINE TO BUMP CONTRACT MODIFICATION MUMBER                                  
         SPACE 1                                                                
BUMPNUM  OI    RCONMODR,X'80'      CON HEADLINE CHANGE IND                      
         CLC   RCONCREA,TODAY      NEW K TODAY?                                 
         BCR   8,RE                                                             
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BNZ   BUMP5                                                            
*                                                                               
         CLC   RCONMODD,TODAY      MODIFIED TODAY?                              
         BCR   8,RE                                                             
* UPDATE MODIFICATION NUMBER                                                    
BUMP5    OI    TAREQ,1             T/A REQ IND                                  
         NI    RCONMODR,X'9F'      NO BUY HEADLINE CHANGE OR RESET              
         SPACE 1                                                                
         TM    RCONMODR+1,X'C0'  ACE/GRAPHNET- DON'T BUMP MOD NUMBER            
         BNZ   BUMP10                                                           
         SPACE 1                                                                
         SR    R1,R1                                                            
         IC    R1,RCONMOD          MOD NUMBER                                   
         LA    R1,1(R1)                                                         
         STC   R1,RCONMOD                                                       
BUMP10   MVC   RCONMODD,TODAY                                                   
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*   RETRIEVE AND SAVE COMBO CONTROL ELEMENT FROM CONTRACT RECORD.               
*      ABORT IF NOT FOUND, BECAUSE WE SHOULDN'T EVEN BE IN THIS                 
*      OVERLAY.                                                                 
*                                                                               
SETCOMBO NTR1                                                                   
         MVC   CONKSTA,RCONKSTA    SAVE STATION OF ORIG ORDER                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'        COMBO CONTROL ELEMENT                        
         BAL   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                MUST BE FOUND                                
         ZIC   R2,1(R6)            SAVE IT BY LENGTH                            
         BCTR  R2,0                DECREMENT FOR EX                             
         LA    R3,COMBOCTL         A(STORAGE AREA)                              
         EX    R2,SETC0010                                                      
         B     SETC0020                                                         
SETC0010 MVC   0(0,R3),0(R6)       SAVE IT                                      
*                                                                               
SETC0020 EQU   *                                                                
*                                                                               
*  CALCULATE # OF CONTRACTS IN COMBO BY DIVIDING THE LENGTH OF THE              
*    COMBO CONTROL ELEMENT (MINUS ELEMENT CODE AND LENGTH BYTES)                
*    BY LENGTH OF ONE COMBO CONTRACT ENTRY                                      
*                                                                               
         ZIC   R3,1(R6)            L(COMBO CONTROL ELEMENT)                     
         SR    R2,R2               EVEN REGISTER OF PAIR                        
         BCTR  R3,0                SUBTRACT L(ELEMENT CODE BYTE)                
         BCTR  R3,0                SUBTRACT L(ELEMENT LEN  BYTE)                
         LA    R1,9                L(CONTROL ELEMENT)                           
         DR    R2,R1               DIVIDED LEN BY SIZE TO CALCULATE             
         STC   R3,CONCTR              NUMBER OF ENTRIES                         
*                                                                               
         BAL   RE,DISPSTAS         INSERT STATION CALLS INTO SCREEN             
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
*  INSERT STATION CALL LETTERS FOR THIS CONTRACT INTO THE SCREEN                
*                                                                               
         DC    0H'0'                                                            
DISPSTAS NTR1                                                                   
         LA    R5,RCONELEM         A(DESCRIPTIVE ELEMENT)                       
DISP0010 EQU   *                                                                
         ZIC   R0,1(R5)            FIND X'17' ELEMENT                           
         AR    R5,R0                                                            
         CLI   0(R5),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                MUST HAVE AN X'17'                           
         CLI   0(R5),X'17'         X'17' ELEMENT?                               
         BNE   DISP0010            NO                                           
         ZIC   R3,CONCTR           YES - # OF CONTRACTS                         
*                                     IN COMBO ORDER                            
*                                                                               
         BAL   RE,REINSCRN         RESET SCREEN BEFORE HDG LOAD                 
*                                                                               
*  SET STATIONS IN COMBO CONTROL ELEMENT INTO HEADING                           
*                                                                               
         LA    R2,OR2STA1H         A(HDR OF 1ST STATION)                        
         LA    R4,RCONKSTA         A(STATION OF ORDER)                          
         PRINT GEN                                                              
         GOTO1 INSERTIT,DMCB,(R4),(R2),1                                        
         PRINT NOGEN                                                            
         LA    R5,2(R5)            1ST STATION IN COMBO CTL ELT                 
         LA    R4,2                SET STATION COUNTER                          
DISP0020 EQU   *                                                                
         CLC   RCONKSTA,0(R5)      STA OF ORDER VS COMBO CONTROL                
         BE    DISP0030            SAME - ALREADY INSERTED - SKIP IT            
         ZIC   R0,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,R0                                                            
*                                                                               
*  MOVE STATION TO HEADING                                                      
*                                                                               
         GOTO1 INSERTIT,DMCB,(R5),(R2),(R4)                                     
         LA    R4,1(R4)            INCREMENT STATION COUNTER                    
DISP0030 EQU   *                                                                
         LA    R5,9(R5)            NEXT STATION IN CONTROL ELEMENT              
         BCT   R3,DISP0020         GO BACK FOR NEXT                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   INSERTIT - INSERTS STATION CALL LETTERS INTO SCREEN, TURNS ON               
*     TRANSMIT BIT.  AFTER CALLS ARE INSERTED, THE TOTAL FIELD IN               
*     THAT COLUMN IS UNPROTECTED TO PERMIT ENTRY OF DATA.  THIS                 
*     ELIMINATES ANY UNNECESSARY TABBING.                                       
*          P1    =  A(STATION CALL LETTERS IN COMBO CONTROL ELEMENT)            
*          P2    =  SCREEN FIELD HEADER                                         
*          P3    =  STATION COUNTER FOR UNPROTECT                               
*                                                                               
         DC    0H'0'                                                            
FRSTTOTL EQU   OR2TOT1H-OR2STA1H   DISPLACEMENT: HDG TO 1ST TOTAL               
INSERTIT NTR1                                                                   
         L     R2,0(R1)            LOAD A(STA CALL LETTERS)                     
         MVC   FULL(1),11(R1)      LOAD STATION COUNTER FOR UNPROT              
         L     R1,4(R1)            LOAD A(SCREEN FIELD HEADER)                  
         MVC   8(4,R1),0(R2)       INSERT STATION LETTERS                       
         CLI   4(R2),C' '          NO MEDIA?                                    
         BE    INSE0010            NO MEDIA                                     
         CLI   4(R2),X'00'         NO MEDIA?                                    
         BE    INSE0010            NO MEDIA                                     
         CLI   4(R2),C'T'          MEDIA = TELEVISION?                          
         BE    INSE0010            YES                                          
         MVI   12(R1),C'-'         INSERT HYPHEN                                
         MVC   13(1,R1),4(R2)      INSERT MEDIA                                 
INSE0010 EQU   *                                                                
         OI    6(R1),X'80'         TURN ON TRANSMIT BIT                         
         CLI   FULL,1              1ST STATION?                                 
         BNE   INSE0011            NO                                           
         LA    R1,OR2TOT1H         YES - SET TOTAL ADDRESS                      
         B     INSE0020                                                         
INSE0011 EQU   *                                                                
         CLI   FULL,2              2ND STATION?                                 
         BNE   INSE0012            NO                                           
         LA    R1,OR2TOT2H         YES - SET TOTAL ADDRESS                      
         B     INSE0020                                                         
INSE0012 EQU   *                                                                
         CLI   FULL,3              3RD STATION?                                 
         BNE   INSE0013            NO                                           
         LA    R1,OR2TOT3H         YES - SET TOTAL ADDRESS                      
         B     INSE0020                                                         
INSE0013 EQU   *                                                                
         CLI   FULL,4              4TH STATION?                                 
         BNE   INSE0014            NO                                           
         LA    R1,OR2TOT4H         YES - SET TOTAL ADDRESS                      
         B     INSE0020                                                         
INSE0014 EQU   *                                                                
         DC    H'00'               ABORT:  INVALID VALUE                        
INSE0020 EQU   *                                                                
         NI    6(R1),X'FF'-X'20'   TURN OFF PROTECT BIT                         
         OI    6(R1),X'80'         TURN ON TRANSMIT BIT                         
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   REINITIALIZE ALL STATION HEADINGS, PROTECT ALL TOTAL FIELDS.                
*     THE TOTAL FIELD FOR A COLUMN IS UNPROTECTED WHEN STATION                  
*     CALL LETTERS ARE INSERTED, BASED ON THE NUMBER OF CONTRACTS               
*     IN THE COMBO CONTROL ELEMENT.  THIS PREVENTS UNNECESSARY                  
*     TABBING.                                                                  
*                                                                               
REINSCRN NTR1                                                                   
         LA    R4,4                LOOP CONTROL                                 
         LA    R1,OR2STA1H                                                      
REIN0010 EQU   *                                                                
         XC    8(7,R1),8(R1)       BLANK OUT HEADING                            
         OI    6(R1),X'80'         TRANSMIT SPACES                              
         ZIC   RF,0(R1)            BUMP TO NEXT FIELD                           
         AR    R1,RF                                                            
         BCT   R4,REIN0010         GO BACK FOR NEXT                             
REIN0020 EQU   *                                                                
         LA    R1,OR2TOT1H         1ST TOTAL FIELD ON SCREEN                    
         LA    R4,4                LOOP CONTROL WITHIN LINE                     
REIN0040 EQU   *                                                                
         OI    6(R1),X'80'+X'20'   XMIT/PROTECT                                 
         ZIC   R0,0(R1)            BUMP TO NEXT FIELD ON SCREEN                 
         AR    R1,R0                                                            
         BCT   R4,REIN0040         DO 4 FIELDS ON LINE                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  CALCULATE DISPLACEMENT INTO COMBO CONTROL ELEMENT, AND EXTRACT               
*    THE APPROPRIATE CONTRACT NUMBER.  RETRIEVE THE CONTRACT RECORD             
*    VIA THE 8C KEY.                                                            
*                                                                               
NEXTCON# NTR1                                                                   
*                                                                               
*  FIRST FIND POSITION IN COMBO CONTROL ELEMENT OF STATION OF                   
*    ORIGINAL ORDER, WHICH HAS BEEN PROCESSED FIRST                             
*       P1  =  RETRIEVE FOR UPDATE IF NOT ZERO                                  
*                                                                               
         L     R5,0(R1)            SET OPTION                                   
         LA    R2,COMBOCTL+2       A(1ST STA IN COMBO CTL ELT)                  
         LA    RF,4                LOOP CONTROL                                 
         LA    RE,1                SET COUNTER                                  
NEXC0010 EQU   *                                                                
         CLC   CONKSTA,0(R2)       STATION OF ORDER FOUND IN ELT?               
         BE    NEXC0020            YES                                          
         LA    R2,9(R2)            NO  - CHECK NEXT ONE                         
         LA    RE,1(RE)                  INCREMENT COUNT                        
         BCT   RF,NEXC0010         GO BACK FOR IT                               
NEXC0020 EQU   *                                                                
         SR    R2,R2                                                            
         ZIC   R3,STACTR           CONTRACT COUNTER IN PROGRESS                 
         CR    R3,RE               IN PROG VS ORIG STA POSITION                 
         BH    NEXC0030            AFTER ORIGINAL STA POSITION:                 
*                                    NO ADDITIONAL ADJUSTMENT NEEDED            
         BCTR  R3,0                BEFORE ORIGINAL STA POSITION:                
*                                    ADDITIONAL ADJUSTMENT NEEDED               
NEXC0030 EQU   *                                                                
         BCTR  R3,0                MAKE ZERO RELATIVE                           
         LA    R1,9                SIZE OF COMBO CONTROL CONTRACT ELT           
         MR    R2,R1               CALCULATE DISPLACEMENT                       
         LA    R2,COMBOCTL+2       A(COMBO CONTROL ELEMENT)                     
         AR    R2,R3               ADD DISPLACEMENT                             
         L     RE,=X'99999999'     CALC 9'S COMP OF CONTRACT #                  
         MVC   FULL,5(R2)          LOAD CONTRACT #                              
         L     RF,FULL                                                          
         SR    RE,RF               GET 9'S COMP                                 
         ST    RE,FULL             SAVE IT                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           INSERT KEY ID                                
         MVC   KEY+21(2),TWAAGY    INSERT REP ID                                
         MVC   KEY+23(4),FULL      INSERT CONTRACT # IN KEY                     
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     CONTRACT FOUND?                              
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE PRESENT!!                      
         LTR   R5,R5               RETRIEVE FOR UPDATE?                         
         BZ    NEXC0040            NO                                           
         MVI   UPDATE,YES          SET RETRIEVE FOR UPDATE                      
NEXC0040 EQU   *                                                                
         MVC   SAVEDADD,KEY+28     SAVE DISK ADDRESS                            
         GOTO1 VGETREC,DMCB,RCONREC                                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   EDITTOTS:  CHECK ALL TOTALS FOR THIS SCREEN.  IF ANY ARE IN                 
*     ERROR, SEND BACK NON-ZERO CONDITION CODE, AND ADDRESS OF                  
*     FIELD IN ERROR IN FULL.                                                   
*                                                                               
EDITTOTS NTR1                                                                   
         ZIC   R3,CONCTR           # OF CONTRACTS IN ORDER                      
         LA    R2,OR2TOT1H         1ST TOTAL $ HEADER                           
EDTO0010 EQU   *                                                                
         CLI   5(R2),0             ANY VALUE IN FIELD?                          
         BE    EDTO0030            NO  - ACCEPT IT                              
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)     INSERT FIELD LENGTH                          
         ST    R2,FULL             SAVE IN CASE OF ERROR                        
         LR    R6,R2                                                            
         LA    R6,8(R6)            A(DATA FIELD)                                
         ST    R6,DMCB             INSERT INTO P1                               
         GOTO1 CASHVAL,DMCB                                                     
         CLI   DMCB,X'FF'          ERROR IN FIELD?                              
         BE    EDTO0040            YES - EXIT WITH ERROR                        
EDTO0030 EQU   *                                                                
         ZIC   R0,0(R2)            LENGTH OF FIELD                              
         AR    R2,R0               BUMP TO NEXT FIELD                           
         BCT   R3,EDTO0010         GO BACK FOR NEXT                             
         LA    RF,0                SET ZERO CC                                  
         B     EDTO0050            EXIT WITH NO ERROR                           
EDTO0040 EQU   *                                                                
         LA    RF,1                SET NON-ZERO CC                              
EDTO0050 EQU   *                                                                
         LTR   RF,RF                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   LOCAL STORAGE                                                               
*                                                                               
COMBOCTL DS    CL60                STORAGE FOR COMBO CONTROL ELEMENT            
CONCTR   DS    XL1                 CONTRACT COUNTER                             
STACTR   DS    XL1                 STATION COUNT IN PROGRESS                    
CONKSTA  DS    CL5                 STATION OF ORIGINAL ORDER                    
SAVEDADD DS    CL4                 SAVE AREA FOR DISK ADDRESS                   
*                                                                               
         DS    CL1000              *** FORCE 2ND BASE REG USE HERE ***          
*                                                                               
*        EQUATES       *                                                        
YES      EQU   C'Y'                                                             
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONMOD+L'CONMOD                                                  
       ++INCLUDE RECNTE0D                                                       
*                                                                               
         CSECT                                                                  
         DROP  RB                                                               
GETORD   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'ORD=',8(R2)       HAVE 'ORD=NNNNNNNN' SYNTAX?                 
         BE    GORD010              YES                                         
         SR    R0,R0                NO - SET CC                                 
         B     EXXMOD               BYE                                         
*                                                                               
GORD010  DS    0H                                                               
         GOTO1 SCANNER,DMCB,(R2),AIO4                                           
         LA    R3,2                                                             
         CLI   4(R1),1                                                          
         BNE   ERROR                                                            
         L     R4,AIO4              SCANNER OUTPUT                              
         TM    3(R4),X'80'         NUMERIC?                                     
         BZ    ERROR                                                            
*                                                                               
         XC    KEY,KEY              LOOKUP CONTRACT                             
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),REPALPHA                                               
         GOTOX (RFCONNUM,VREPFACS),DMCB,(7,8(R4)),(2,KEY+23)                    
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
         L     R6,AIO4                                                          
         CLC   RCONKCON,23(R6)     SAME K?                                      
         BE    ERROR                                                            
*                                                                               
         MVI   ELCODE,X'82'                                                     
         BAS   RE,GETEL                                                         
         BE    GORD020                                                          
         LA    R3,757              NO CMT ON SOURCE K                           
         B     ERROR                                                            
GORD020  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,0(R6)                                      
         BAS   RE,NEXTEL                                                        
         BE    GORD020                                                          
*                                                                               
         LTR   RB,RB                                                            
         B     EXXMOD                                                           
*                                                                               
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053RECNT4A   12/30/97'                                      
         END                                                                    
