*          DATA SET GEKWX04    AT LEVEL 004 AS OF 05/01/02                      
*PHASE TF2004A,+0                                                               
         TITLE '$KWX MK3 - ADD MESSAGE, END && UNEND ACTIONS'                   
         PRINT NOGEN                                                            
KWX04    CSECT                                                                  
         NMOD1 0,**KWX04*,R9                                                    
         L     RC,0(R1)                                                         
         USING KWX04+4096,R9       R9 = 2ND BASE                                
         USING TWAD,RA             RA = TWA                                     
         USING GWS,RC              RC = GWS                                     
*                                                                               
T001     ST    RB,ABASE2                                                        
         ST    R9,A2NDBAS2                                                      
         ST    RD,AREGSAV2                                                      
         B     T010                                                             
         EJECT                                                                  
*              FURTHER PARAMETER CHECKS ON PRE-PROCESSED PARAMETERS             
*                                                                               
T010     CLI   ACTION,UND          NOT ENDED BOOK INVALID FOR UNEND             
         BNE   T011                                                             
         TM    MSGSTAT,ENDED                                                    
         BNZ   T038                                                             
         MVI   FERN,BKNENDED                                                    
         B     ERROR                                                            
*                                                                               
T011     TM    MSGSTAT,ENDED       ENDED BOOK INVALID FOR ADD & END             
         BZ    *+12                                                             
         MVI   FERN,BKENDED                                                     
         B     ERROR                                                            
         CLI   ACTION,END                                                       
         BNE   T012                                                             
*                                                                               
T011A    CLI   FXCHCKMK,0                                                       
         BE    T033                                                             
         CLI   CHCKMK,C'N'                                                      
         BE    T035                                                             
         CLI   CHCKMK,C'Y'                                                      
         BE    T033                                                             
         MVI   FERN,INVALID                                                     
         MVC   FNDX,FXCHCKMK                                                    
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
*                                                                               
T012     OC    FRMRECHI,FRMRECHI   NO FORMAT DEFINED                            
         BNZ   T013                                                             
         MVC   KWXHEAD(30),=C'PLEASE SPECIFY SCREEN REQUIRED'                   
         B     MOREXIT                                                          
T013     CLC   FRMRECHI,FRMRPEAT   CANT ADD AFTER LAST                          
         BNH   T014                                                             
         CLC   FRECMHI,FRMRECHI                                                 
         BNE   T014                                                             
         MVI   FERN,AFTRLAST                                                    
         B     ERROR                                                            
*                                                                               
T014     CLI   FXENDMK,0           CANT HAVE END + NEXTID OR NUM                
         BE    T016                                                             
         CLI   FXID,0                                                           
         BNE   *+12                                                             
         CLI   FXNUM,0                                                          
         BE    T016                                                             
         MVI   FERN,INCPARMS                                                    
         MVC   FNDX,FXID                                                        
         CLC   FNDX,FXNUM                                                       
         BH    ERROR                                                            
         MVC   FNDX,FXNUM                                                       
         B     ERROR                                                            
*                                                                               
T016     CLC   NUM,=H'18'          NUMBER OF CHUNKS MUST BE LEQ 18              
         BNH   T017                                                             
         MVC   FNDX,FXNUM                                                       
         MVI   FERN,XCEEDMAX                                                    
         B     ERROR                                                            
*                                                                               
T017     CLI   FXLAST,0            LAST                                         
         BE    T018                                                             
         CLI   FXID,0                                                           
         BE    T017A                                                            
         MVI   FERN,INCPARMS       INCOMPATIBLE WITH NEXT                       
         MVC   FNDX,FXID                                                        
         B     ERROR                                                            
T017A    CLC   FRMRECHI,FRMRPEAT                                                
         BH    T017B                                                            
         MVI   FERN,NOFORMAT       MUST BE A LAST FORMAT                        
         MVC   FNDX,FXLAST                                                      
         B     ERROR                                                            
T017B    CLI   FXNUM,0             IMPLIES 1 CHUNK UNLESS OVERRIDDEN            
         BNE   T018                                                             
         MVI   NUM+1,1                                                          
*                                                                               
T018     LA    R0,KWXHEAD          FULL IS USED AS A MKER                       
         ST    R0,FULL             BO   0=NOADDS,1=ADDS,E=ADDS TO EOTWA         
         TM    FRMSTAT,MESSAGE     B1-3 A(NEXT POSN IN KWXHEAD)                 
         BNO   T070                GO TO SET UP SCREEN IF NOT READY             
         LH    R1,MSGRECHI                                                      
         LA    R1,1(R1)                                                         
         CH    R1,DISPLOM                                                       
         BNE   T070                                                             
         CLI   FXLAST,0                                                         
         BE    T020                                                             
         CLC   DISPLOF,FRMRECHI                                                 
         BNE   T070                                                             
         B     T020                                                             
         EJECT                                                                  
*              VALIDATE INPUT                                                   
*                                                                               
T020     OC    FRMBOOK,FRMBOOK     IF DEFAULT SCREEN, CALL OVERLAY TO           
         BNZ   T022                HANDLE KEYWORDS                              
         MVI   TYPE,CHECK                                                       
         GOTO1 ACALLOV,PARAS,(32,0),0                                           
         CLI   PARAS+4,X'FF'                                                    
         BE    T022                                                             
         L     RF,PARAS                                                         
         GOTO1 (RF),PARAS,(RC)                                                  
         BP    T022                OK                                           
         B     EXIT                ERROR OR MODIFIED                            
*                                                                               
T022     LH    R1,FRECMHI          PREPARE VALIDATION LOOP                      
         LA    R1,1(R1)                                                         
         ST    R1,PARAS            GETFORM PARAS(4)                             
         LH    R1,DISPLOM          VALMESS PARAS+4(20)                          
         ST    R1,PARAS+4                                                       
         ST    R1,PARAS+20                                                      
         GOTO1 ,PARAS+8,ADDBUFF,AIOB,KWXDATAH                                   
         SR    R0,R0               R0=A(END OF INPUT) IF PRE END OF TWA         
*                                                                               
T024     L     RF,PARAS+16         LOOP FOR A CHUNK                             
         CLI   0(RF),0             END OF TWA NOW OR AFTER A TAB FLD            
         BE    T026                                                             
         CLI   0(RF),9                                                          
         BNE   *+12                                                             
         CLI   9(RF),0                                                          
         BE    T026                                                             
         GOTO1 AGETFORM,PARAS      GET DEFINING CHUNK                           
         BZ    ERROR                                                            
         GOTO1 AVALMESS,PARAS+4    VALIDATE CHUNK IN TWA AND ADD REC TO         
         BZ    ERROR               BUFFER                                       
         BNM   T025B               (OK INPUT)                                   
         LTR   R0,R0               NO INPUT                                     
         BNZ   T025A               IF PREVIOUS CHUNK INPUT                      
         L     R0,12(R1)           SAVE NEW END OF INPUT                        
         OC    ATWADIFF,ATWADIFF   SET FIRST FORMAT CHANGE ADDRESS              
         BNZ   T025A               IF NOT SET                                   
         ST    R0,ATWADIFF                                                      
T025A    L     R1,PARAS+12         BUMP TWA POINTER TO NEXT CHUNK               
         LH    RE,0(R1)            FORMAT CHUNK RECORD LENGTH                   
         SH    RE,=H'3'            - LENGTH OF RECLEN + TERMINATOR              
         L     R1,PARAS+16         IS MESSAGE CHUNK SIZE IN TWA                 
         AR    R1,RE                                                            
         ST    R1,PARAS+16                                                      
         B     T025C                                                            
T025B    SR    R0,R0               SOME INPUT - CLEAR END OF INPUT ADDR         
         L     R1,PARAS+4          BUMP MESSAGE CHUNK                           
         LA    R1,1(R1)                                                         
         ST    R1,PARAS+4                                                       
T025C    L     R1,PARAS            AND DEFINING CHUNK                           
         LA    R1,1(R1)                                                         
         ST    R1,PARAS                                                         
         B     T024                                                             
*                                                                               
T026     CLI   ADDBUFF,X'FF'       END OF TWA                                   
         BNE   *+12                CHECK FOR ANY INPUT AT ALL                   
         MVI   FERN,NOINPUT                                                     
         B     ERROR               NONE = ERROR                                 
         LTR   RF,R0               CHECK FOR LAST CHUNK USED                    
         BZ    T027                IT WAS                                       
         OI    FULL,1              SOME INPUT BUT NOT TO END OF TWA             
         XC    0(3,RF),0(RF)       R0 IS A(NEW END OF TWA)                      
         B     T028                                                             
*                                                                               
T027     MVI   FULL,C'E'           END OF TWA                                   
         B     T028                                                             
         EJECT                                                                  
*              ADD RECORDS TO BOOK                                              
*                                                                               
T028     CLC   DISPHIF,FRMRPEAT    HANDLE REPEAT FLDS                           
         BL    T031                                                             
         SR    R1,R1               FIND 2ND REPEAT CHUNK IN ADDBUFF             
         ICM   R1,3,FRMRPEAT                                                    
         SH    R1,DISPLOF                                                       
         BM    T031                                                             
         LA    R1,1(R1)                                                         
         LA    R5,ADDBUFF                                                       
         CLI   0(R5),X'FF'                                                      
         BE    T031                                                             
         LH    R0,0(R5)                                                         
         AR    R5,R0                                                            
         BCT   R1,*-14                                                          
         SR    R1,R1                                                            
*                                                                               
T029     CLI   0(R5),X'FF'         NOW LOOK FOR REPEAT FIELDS                   
         BE    T031                                                             
         LA    R4,2(R5)                                                         
T029A    ICM   R1,1,0(R4)          LOOP FOR A FIELD                             
         BZ    T030                                                             
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),SPACES      IF SPACES                                    
         BNE   T029B                                                            
         TM    4(R4),REPEAT        AND REPEAT                                   
         BZ    T029B                                                            
         LR    RF,R4               FIND CORRESPONDING FLD IN PREVIOUS           
         SH    RF,0(R5)            REC                                          
         EX    R1,*+8              AND MOVE VALUE IN                            
         B     *+10                                                             
         MVC   8(0,R4),8(RF)                                                    
T029B    LA    R4,9(R1,R4)         BUMP TO NEXT FIELD                           
         B     T029A                                                            
T030     LA    R5,1(R4)            BUMP TO NEXT REC                             
         B     T029                                                             
*                                                                               
T031     LH    R1,DISPLOM          ADD RECS                                     
         ST    R1,PARAS                                                         
         MVI   PARAS,MESSAGE                                                    
         GOTO1 AADDCHNK,PARAS,,ADDBUFF                                          
         BZ    ERROR                                                            
         MVC   PARAS(4),DISPLOM    SET UP START OF HDR MESSAGE (REF N-N         
         MVC   PARAS+10(2),MSGRECHI OF N ADDED - )                              
         GOTO1 AEDITREF,PARAS,,KWXHEAD                                          
         L     RF,PARAS+4                                                       
         MVC   0(7,RF),=C'ADDED -'                                              
         LA    RF,8(RF)                                                         
         STCM  RF,7,FULL+1         SAVE PNTR TO HDR MESSAGE IN FULL             
*                                                                               
T032     CLI   FXENDMK,0           HANDLE BOOK ENDING                           
         BE    T042                                                             
T033     TM    MSGSTAT,CHECKSUM                                                 
         BZ    T035                                                             
         BAS   RE,CHKSUM                                                        
         BZ    TEND                                                             
T035     OI    MSGSTAT,ENDED                                                    
         B     T040                                                             
*                                                                               
T038     NI    MSGSTAT,ALL-ENDED   HANDLE BOOK UNENDING                         
*                                                                               
T040     CLI   ACTION,ADM          SIGN OFF IF NO MORE INPUT EXPECTED           
         BE    *+14                                                             
         MVC   KWXHEAD(29),=C'ACTION COMPLETED - ENTER NEXT'                    
         B     TEND                                                             
T042     CLI   FXENDMK,0           WE NEED TO RETURN A NEW SCREEN               
         BNE   T045                UNLESS END                                   
         CLI   FXLAST,0                                                         
         BNE   T045                OR LAST                                      
         TM    FRMSTAT,PARTSCRN    OR RESTRICTED NUMBER WAS REQUESTED           
         BO    T045                                                             
         CLI   FXID,0                                                           
         BNE   T050                IF ID GIVEN                                  
         CLI   FULL,C'E'                                                        
         BE    T050                OR THIS SCREEN WAS FILLED                    
T045     L     RF,FULL                                                          
         MVC   0(16,RF),=C'ACTION COMPLETED'                                    
         LH    R1,DISPLOM          REDISPLAY (REPEATS)                          
         LH    R0,DISPHIM                                                       
         SR    R0,R1                                                            
         AH    R0,=H'1'                                                         
         ST    R1,PARAS                                                         
         GOTO1 ADISSCRN,PARAS,,(R0)                                             
         B     TEND                                                             
         EJECT                                                                  
*              RETURN NEW SCREEN FOR INPUT                                      
*                                                                               
T050     CLI   FXID,0              NEXT SCREEN IS DIFFERENT FORMAT              
         BE    T070                                                             
         XC    FRMBOOK,FRMBOOK                                                  
         XC    FRMRECHI,FRMRECHI                                                
         XC    FRMRPEAT,FRMRPEAT                                                
         XC    FRECMHI,FRECMHI                                                  
         MVI   FRMSTAT,0                                                        
         OC    ID,ID                                                            
         BNZ   T052                                                             
         MVI   FRMRECHI+1,1        DEFAULT FORMAT                               
         MVI   FRMRPEAT+1,1                                                     
         B     T070                                                             
*                                                                               
T052     GOTO1 ,PARAS,ID           DOES THE BOOK EXIST                          
         MVI   PARAS,FORMAT                                                     
         L     RF,AFINDBK                                                       
         BASR  RE,RF                                                            
         L     RF,FULL                                                          
         TM    DMCB+8,X'6F'                                                     
         BZ    *+14                                                             
T053     MVC   0(24,RF),=C'NEXT SCREEN UNOBTAINABLE'                            
         B     T054                                                             
         TM    DMCB+8,X'10'                                                     
         BZ    T060                                                             
         MVC   0(19,RF),=C'NEXT BOOK NOT FOUND'                                 
T054     MVC   FNDX,FXID                                                        
         B     TEND                                                             
*                                                                               
T060     XC    1(3,R1),1(R1)       IF SO GET HDR REC (CHUNK ZERO)               
         GOTO1 AGETCHNK                                                         
         BZ    ERROR                                                            
         LM    R3,R4,DMCB+8                                                     
         USING UKRECD,R3                                                        
         USING HDRD,R4                                                          
         CLI   HDTYPE,FORMAT                                                    
         BNE   T064                                                             
         CLC   UKUSRID,TWAUSRID                                                 
         BE    T065                                                             
*&&UK                                                                           
         CLC   UKUSRID,SAVTKWID                                                 
*&&                                                                             
*&&US                                                                           
         CLC   UKUSRID,=H'43'      TCH1                                         
*&&                                                                             
         BE    T065                                                             
         LA    RF,HDACCS           OTHERWISE CHECK HDR REC                      
T062     CLI   0(RF),X'FF'                                                      
         BE    T064                                                             
         CLC   TWAUSRID,0(RF)                                                   
         BE    *+12                                                             
         LA    RF,L'HDACCS(RF)                                                  
         B     T062                                                             
         TM    2(RF),READACC       READ ACCESS IS SUFFICIENT                    
         BO    T065                                                             
T064     L     RF,FULL                                                          
         MVC   0(31,RF),=C'ACCESS TO NEXT BOOK NOT ALLOWED'                     
         B     T054                                                             
*                                                                               
T065     OC    HDFRMHI,HDFRMHI                                                  
         L     RF,FULL                                                          
         BZ    T053                                                             
         MVC   FRMBOOK,UKKEY       SAVE FORMAT DATA IN TWA                      
         MVC   FRMRECHI,HDFRMHI                                                 
         MVC   FRMRPEAT,HDFRMRPT                                                
         GOTO1 ACLOSE              AND INDEX ENTRY/DISKADDR                     
*                                                                               
T070     XC    PARAS(12),PARAS     NOW SET UP SCREEN                            
         LH    R1,FRECMHI                                                       
         LA    R1,1(R1)                                                         
         LH    R2,MSGRECHI                                                      
         LA    R2,1(R2)                                                         
         LH    R3,NUM                                                           
         CLI   FXLAST,0                                                         
         BE    *+10                                                             
         SR    R1,R1                                                            
         ICM   R1,3,FRMRECHI                                                    
         STM   R1,R3,PARAS                                                      
         GOTO1 ASETSCRN,PARAS                                                   
         NI    FRMSTAT,ALL-PARTSCRN                                             
         LTR   R3,R3                                                            
         BZ    T075                                                             
         OI    FRMSTAT,PARTSCRN    IF MAX=N GIVEN, SET INDICATOR                
*                                                                               
T075     LA    R2,KWXDATAH         NO ADDS - EXIT FOR THIS INPUT                
         ST    R2,ACURSOR                                                       
         CLI   FULL,0                                                           
         BNE   T080                                                             
         MVC   KWXHEAD(26),=C'SCREEN READY - ENTER INPUT'                       
         B     MOREXIT                                                          
*                                                                               
T080     L     RF,FULL             ADDS - EXIT FOR NEXT INPUT                   
         MVC   0(16,RF),=C'ENTER NEXT INPUT'                                    
         B     TEND                                                             
         EJECT                                                                  
*              UPDATE HEADER REC                                                
*                                                                               
TEND     GOTO1 AGETCHNK,PARAS,(SAVMODE,0)                                       
         BZ    ERROR                                                            
         L     R4,DMCB+12                                                       
         MVC   HDFRMBK,FRMBOOK                                                  
         MVC   HDFRMHI,FRMRECHI                                                 
         MVC   HDFRMRPT,FRMRPEAT                                                
         MVC   HDMSGHI,MSGRECHI                                                 
         MVC   HDFRECMH,FRECMHI                                                 
         MVC   HDMSTAT,MSGSTAT                                                  
         GOTO1 APUTCHNK,(R1),,IO                                                
         BZ    ERROR                                                            
         B     OKXIT                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE TO PERFORM CHECKSUM ON A BOOK                            
*              ON EXIT CC = EQU IF ERROR AND KWXHEAD CONTAINS MESSAGE           
*                                                                               
CHKSUM   NTR1                                                                   
         SR    R2,R2               R2 = SUM TOTAL                               
         SR    R3,R3               R3 = CHECKSUM TOTAL                          
         SR    R4,R4                                                            
         LA    R5,1                R5 = CHUNK NUMBER                            
         USING FLDHDRD,R6          R6 = CHUNK FIELD POINTER                     
         XC    PARAS(4),PARAS      PARAS(4)   - USED FOR GETCHNK                
         MVI   PARAS,MESSAGE       PARAS+4(8) -          CASHVAL                
*                                                                               
CHS01    STH   R5,PARAS+2          LOOP FOR A CHUNK                             
         GOTO1 AGETCHNK,PARAS                                                   
         TM    DMCB+8,X'80'                                                     
         BO    CHSEND              EOF                                          
         CLI   DMCB+8,0                                                         
         BNE   ERROR                                                            
         LA    R6,IO+2                                                          
         XC    PARAS+12(4),PARAS+12 USE PARAS+12 FOR SUM VALUE                  
         LA    R7,WORK             & WORK FOR MULTIPLIER STRING                 
         MVI   WORK,X'FF'                                                       
*                                                                               
CHS10    CLI   FLDLEN,0            LOOP FOR A FIELD                             
         BE    CHS30                                                            
         TM    FLDIIND,SUM         SUM                                          
         BZ    CHS15                                                            
         BAS   R8,CHSTOBIN                                                      
         MVC   PARAS+12(4),PARAS+8                                              
         B     CHS20                                                            
CHS15    TM    FLDIIND,CHECKSUM    CHECKSUM                                     
         BZ    CHS17                                                            
         BAS   R8,CHSTOBIN                                                      
         A     R3,PARAS+8                                                       
         B     CHS20                                                            
CHS17    TM    FLDILEN,QUANTITY                                                 
         BZ    CHS20                                                            
         BAS   R8,CHSTOBIN                                                      
         MVC   0(4,R7),PARAS+8                                                  
         MVI   4(R7),X'FF'                                                      
         LA    R7,4(R7)                                                         
CHS20    IC    R4,FLDLEN           BUMP FIELD                                   
         AR    R6,R4                                                            
         B     CHS10                                                            
*                                                                               
CHS30    L     RF,PARAS+12         ADD SUM X MULTIPLIERS (IF ANY) TO R2         
         LA    R1,WORK                                                          
CHS31    CLI   0(R1),X'FF'                                                      
         BE    CHS32                                                            
         L     R0,0(R1)                                                         
         MR    RE,R0                                                            
         LA    R1,4(R1)                                                         
         B     CHS31                                                            
CHS32    AR    R2,RF                                                            
         LA    R5,1(R5)            BUMP CHUNK                                   
         B     CHS01                                                            
*                                                                               
CHSEND   CR    R2,R3               AT END COMPARE TOTALS                        
         BE    OKXIT                                                            
         MVI   FERN,X'FE'          OWN MESSAGE                                  
         MVC   KWXHEAD(23),=C'*** ERROR *** CHECKSUM='                          
         LA    RF,KWXHEAD+23                                                    
         LR    R0,R3                                                            
         BAS   RE,CHSEDIT                                                       
         MVC   0(6,RF),=C', SUM='                                               
         LA    RF,6(RF)                                                         
         LR    R0,R2                                                            
         BAS   RE,CHSEDIT                                                       
         LH    R0,DISPLOM          REDISPLAY                                    
         GOTO1 ADISSCRN,PARAS,(R0),0                                            
         B     ERROR                                                            
*                                                                               
CHSEDIT  EDIT  (R0),(12,0(RF)),2,ALIGN=LEFT                                     
         AR    RF,R0                                                            
         BR    RE                                                               
*                                                                               
CHSTOBIN XC    PARAS+8(4),PARAS+8  CONVERT VALUE AT 8(R6) TO BINARY             
         IC    R4,FLDLEN           PENNIES IN PARAS+8                           
         LA    RF,0(R4,R6)         OR, IF QUANTITY, TO STRAIGHT BINARY          
         BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R6                                                            
         SH    RF,=H'7'                                                         
         BNPR  R8                                                               
         TM    FLDILEN,QUANTITY                                                 
         BO    CHSTOB2                                                          
         ST    RF,PARAS+8                                                       
         GOTO1 ACASHVAL,PARAS+4,8(R6)                                           
         CLI   0(R1),0                                                          
         BER   R8                                                               
         XC    PARAS+8(4),PARAS+8                                               
         BR    R8                                                               
*                                                                               
CHSTOB2  BCTR  RF,0                STRAIGHT BINARY                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R6)                                                      
         CVB   R0,DUB                                                           
         ST    R0,PARAS+8                                                       
         BR    R8                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              EXITS BACK TO ROOT                                               
*                                                                               
ERROR    SR    R0,R0               CC = EQU FOR ERROR                           
         B     EXIT                                                             
*                                                                               
MOREXIT  LNR   RB,RB               CC = NEG FOR MORE INPUT                      
*                                                                               
OKXIT    LTR   RB,RB               CC = POS OK COMPLETED                        
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
* NESTED INCLUDES                                                               
* GEKWXDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE GEKWXDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004GEKWX04   05/01/02'                                      
         END                                                                    
