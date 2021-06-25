*          DATA SET RECNT4C    AT LEVEL 037 AS OF 05/01/02                      
*PHASE T8024CA,+0                                                               
         TITLE 'T8024C - EPL DISPLAY/EDIT - COMBO VERSION'                      
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT4C (T8024C) --- EPL DISPLAY/EDIT - COMBO VERSION    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 06AUG92 (BU ) --- ORIGINAL ENTRY                                *             
*                                                                 *             
* 23OCT92 (BU ) --- CHANGE DATE ENTRY/DISPLAY FACILITY FOR EPL    *             
*                                                                 *             
* 22MAR93 (BU ) --- PROTECT EPL DATE, ADD TRUE ACTIVITY DATE.     *             
*                                                                 *             
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 23JUL97 (SKU) --- 4K CONTRACT SUPPORT                           *             
*                                                                 *             
* 06JAN98 (RHV) --- EPL=NNNNN COMMENT IMPORTING                   *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8024C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T8024C*,R9                                                    
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         L     R2,4(R1)                                                         
*                                                                               
         GOTO1 SETCOMBO,DMCB,(R2)  RETRIEVE AND SET COMBO ELEMENT               
*                                                                               
         CLC   =C'DISP',0(R2)                                                   
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)                                                   
         BE    EDIT0000                                                         
         DC    H'0'                                                             
         EJECT                                                                  
DISP     DS    0H                                                               
         LA    R2,CONBACTH                                                      
         LA    R3,BACERR                                                        
         TM    RCONREC+29,X'01'    COMPRESSED                                   
         BO    ERROR                                                            
         SPACE 1                                                                
         CLI   RCONTYPE,C'B'                                                    
         BNE   DISP0010                                                         
         LA    R2,CONBACTH                                                      
         LA    R3,191              NO SPL/EPL WORK ON TYPE B K'S                
         B     ERROR                                                            
         SPACE 1                                                                
DISP0010 DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWASTAT,X'80'                                                    
         DROP  RF                                                               
         GOTO1 VFOUTBLK,DMCB,EP2MONTH,EP2LAST,0                                 
         SPACE 1                                                                
         LA    R4,EP2CMMTH                                                      
         LA    R2,EP2COMPH                                                      
DISP0020 TM    1(R2),X'20'         FOUT THE PROTECTED FIELDS                    
         BO    DISP0040                                                         
DISP0030 SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         AR    R2,R5                                                            
         CR    R2,R4                                                            
         BL    DISP0020                                                         
         B     DISP0050                                                         
         SPACE 1                                                                
DISP0040 XR    R3,R3                                                            
         IC    R3,0(R2)                                                         
         SH    R3,=H'8'                                                         
         FOUT  (R2),MYSPACES,(R3)                                               
         B     DISP0030                                                         
         EJECT                                                                  
DISP0050 FOUT  EP2STA1H,CONSTA,7        BLAIR STATION                           
         MVC   EP2TDAT,MYSPACES                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'08'        GET TRUE ACT DATE FIRST                      
         BAS   RE,GETEL                                                         
         BNE   DISP0060                                                         
         GOTO1 DATCON,DMCB,(3,5(R6)),(5,EP2TDAT)                                
DISP0060 EQU   *                                                                
         FOUT  EP2TDATH                                                         
         BAS   RE,SETCOMTS         SET APPROPRIATE COMMENT CONTRL               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0170                                                         
         SPACE 1                                                                
         USING RCONSPEL,R6                                                      
         TM    RCONSPES,X'08'      TEST DATE FORMAT                             
         BO    DISP0070            ON  = NEW FORMAT                             
         MVC   FULL,RCONSPYR                                                    
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),(9,EP2MONT)                                 
         B     DISP0080                                                         
DISP0070 EQU   *                                                                
         GOTO1 DATCON,DMCB,(2,RCONSPYR),(5,EP2MONT)                             
DISP0080 EQU   *                                                                
         FOUT  EP2MONTH                                                         
         SPACE 1                                                                
         CLC   RCONSPAM(2),=C'NA'                                               
         BE    DISP0100                                                         
         CLC   RCONSPAM(3),=C'CAN'                                              
         BNE   DISP0090                                                         
         MVC   EP2AMT1,MYSPACES                                                 
         MVC   EP2AMT1(3),=C'CAN'                                               
         FOUT  EP2AMT1H                                                         
         B     DISP0100                                                         
         SPACE                                                                  
DISP0090 EDIT  (B4,RCONSPAM),(7,EP2AMT1),ALIGN=LEFT                             
         FOUT  EP2AMT1H                                                         
         B     DISP0100                                                         
         SPACE                                                                  
DISP0100 EQU   *                                                                
         TM    RCONSPES,X'80'                                                   
         BZ    DISP0110                                                         
         LA    R2,EP2COMPH         DISPLAY STATION WON ALL KEYWORD              
         MVC   8(3,R2),=C'100'                                                  
         OI    6(R2),X'80'                                                      
         B     DISP0130                                                         
         SPACE                                                                  
DISP0110 XR    R3,R3                                                            
         IC    R3,RCONSPNU         NUMBER OF MINIS                              
         CH    R3,=H'1'            TEST FOR NO COMPETING STATIONS               
         BE    DISP0130            NONE                                         
         LA    R6,RCONSPST+9-RCONSPEL(R6)                                       
         BCTR  R3,0                LESS 1 FOR CONTRACT'S STATION                
         LA    R2,EP2COMPH                                                      
         DROP  R6                                                               
         SPACE 1                                                                
         USING RCONSPST,R6                                                      
DISP0120 MVC   8(4,R2),RCONSPST    CALL LETTERS                                 
         LA    R5,12(R2)                                                        
         CLI   RCONSPST+3,C' '                                                  
         BNE   *+6                                                              
         BCTR  R5,0                                                             
         MVI   0(R5),C'-'                                                       
         MVC   1(1,R5),RCONSPST+4  BAND                                         
         CLI   1(R5),C' '                                                       
         BNE   *+8                                                              
         MVI   1(R5),C'T'                                                       
         SPACE 1                                                                
         CLC   RCONSPST(5),=C'OTHER'                                            
         BNE   *+10                                                             
         MVC   8(6,R2),=C'OTHERS'                                               
         FOUT  (R2)                                                             
         SPACE 1                                                                
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         AR    R2,R5               NEXT FIELD                                   
         LA    R6,9(R6)            NEXT MINI                                    
         BCT   R3,DISP0120                                                      
         SPACE 1                                                                
DISP0130 DS    0H                                                               
         LA    R2,EP2COM1H         TURN ON PREV VAL FOR ALL COMMTS              
         LA    R3,4                FOUR FIELDS                                  
DISP0140 EQU   *                                                                
         OI    4(R2),X'20'         PREV VALID                                   
         XC    8(65,R2),8(R2)      WIPE OUT FIELD                               
         FOUT  (R2)                INDICATE 'SEND FIELD'                        
         ZIC   RF,0(R2)            POINT TO NEXT FIELD                          
         AR    R2,RF                                                            
         BCT   R3,DISP0140         GO BACK FOR NEXT                             
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0160                                                         
         LA    R2,EP2COM1H                                                      
         LA    R4,4                LOOP CONTROL                                 
DISP0150 ZIC   R3,1(R6)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),2(R6)       COMMENT OUT                                  
*                                                                               
         L     R8,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(1,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
         BZ    *+10                                                             
         MVC   22(24,R2),=C'***CMNT REC NOT FOUND***'                           
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DISP0160                                                         
         ZIC   R3,0(R2)                                                         
         AR    R2,R3               NEXT FIELD                                   
         BCT   R4,DISP0150         DISPLAY NEXT                                 
         SPACE 1                                                                
DISP0160 EQU   *                                                                
         BAS   RE,OTHRAMTS         INSERT OTHER AMTS INTO SCREEN                
         B     EXXMOD                                                           
         EJECT                                                                  
DISP0170 XC    KEY,KEY             GET STATION RECORD                           
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    DISP0180                                                         
         DC    H'0',C'STA REC MISSING'                                          
DISP0180 GOTO1 VGETREC,DMCB,IOAREA                                              
         SPACE 1                                                                
         MVC   EP2MONT,MYSPACES    CLEAR AND FOUT MONTH FOR CURSOR              
         FOUT  EP2MONTH                                                         
         SPACE                                                                  
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0200                                                         
         SPACE 1                                                                
         USING RCONBKEL,R6                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SR    R3,R3                                                            
DISP0190 MVC   DMCB(4),RCONBKAM    CONTRACT AMOUNT                              
         A     R1,DMCB                                                          
         BAS   RE,NEXTEL                                                        
         BE    DISP0190                                                         
         SPACE 1                                                                
         LTR   R1,R1                                                            
         BZ    DISP0200                                                         
         SLDA  R0,1                                                             
         D     R0,=F'100'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    RF,R1                                                            
         EDIT  (RF),(7,EP2AMT1),ALIGN=LEFT    TOTAL REP BUYS                    
         FOUT  EP2AMT1H                                                         
         SPACE 1                                                                
DISP0200 LA    R2,CONBACTH                                                      
         BAS   RE,OTHRAMT2         GET OTHER CONTRACT $                         
         B     EXXMOD              DO NOT PUT OUT STATIONS                      
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*  OTHRAMTS:  RETRIEVES OTHER CONTRACTS IN COMBO ORDER AND INSERTS              
*    THE EPL AMOUNTS INTO THE PROPER SLOTS                                      
*                                                                               
OTHRAMTS NTR1                                                                   
         MVI   STACTR,2            SET STATION COUNTER TO 1ST 'OTHER'           
OTHR0010 EQU   *                                                                
         GOTO1 NEXTCON#,DMCB,0     RETRIEVE NEXT CONTRACT NUMBER                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'        RETRIEVE ORDER ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   OTHR0030            NOT FOUND                                    
         USING RCONSPEL,R6                                                      
*        SPACE 1                                                                
         LA    R2,EP2AMT2H         AMOUNT                                       
         CLI   STACTR,2            1ST OTHER STATION?                           
         BE    OTHR0020            YES                                          
         LA    R2,EP2AMT3H         AMOUNT                                       
         CLI   STACTR,3            2ND OTHER STATION?                           
         BE    OTHR0020            YES                                          
         LA    R2,EP2AMT4H         AMOUNT                                       
         CLI   STACTR,4            3RD OTHER STATION?                           
         BE    OTHR0020            YES                                          
         DC    H'0'                                                             
*                                                                               
OTHR0020 EQU   *                                                                
         CLC   RCONSPAM(2),=C'NA'                                               
         BE    OTHR0030                                                         
         CLC   RCONSPAM(3),=C'CAN'                                              
         BNE   OTHR0022                                                         
         MVC   EP2AMT1,MYSPACES                                                 
         MVC   EP2AMT1(3),=C'CAN'                                               
         FOUT  EP2AMT1H                                                         
         B     OTHR0030                                                         
         SPACE                                                                  
OTHR0022 EQU   *                                                                
         LR    R3,R2               LOAD A(HEADER FIELD)                         
         LA    R3,8(R3)            POINT TO DATA FIELD                          
         EDIT  (B4,RCONSPAM),(7,(R3)),ALIGN=LEFT                                
         FOUT  (R2)                                                             
*                                                                               
OTHR0030 EQU   *                                                                
         ZIC   RF,STACTR           BUMP STATION IN PROGRESS                     
         LA    RF,1(RF)                                                         
         STC   RF,STACTR                                                        
         CLC   STACTR,CONCTR       ALL CONTRACTS DONE?                          
         BNH   OTHR0010            NO  - GO BACK AND DO NEXT                    
         LR    RF,RA               YES - RELOAD ORIGINAL CONTRACT               
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         XIT1                                                                   
         EJECT                                                                  
         DROP  R6                                                               
*                                                                               
*  OTHRAMT2:  RETRIEVES OTHER CONTRACTS IN COMBO ORDER AND INSERTS              
*    THE CONTRACT DOLLARS IN THE PROPER SLOTS, AS THERE IS NO EPL               
*    ELEMENT PRESENT IN ANY OF THE RECORDS                                      
*                                                                               
OTHRAMT2 NTR1                                                                   
         MVI   STACTR,2            SET STATION COUNTER TO 1ST 'OTHER'           
OTAM0010 EQU   *                                                                
         GOTO1 NEXTCON#,DMCB,0     RETRIEVE NEXT CONTRACT NUMBER                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'        RETRIEVE ORDER ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   OTHR0030            NOT FOUND                                    
         USING RCONBKEL,R6                                                      
*        SPACE 1                                                                
         LA    R2,EP2AMT2H         AMOUNT                                       
         CLI   STACTR,2            1ST OTHER STATION?                           
         BE    OTAM0020            YES                                          
         LA    R2,EP2AMT3H         AMOUNT                                       
         CLI   STACTR,3            2ND OTHER STATION?                           
         BE    OTAM0020            YES                                          
         LA    R2,EP2AMT4H         AMOUNT                                       
         CLI   STACTR,4            3RD OTHER STATION?                           
         BE    OTAM0020            YES                                          
         DC    H'0'                                                             
*                                                                               
OTAM0020 EQU   *                                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SR    R3,R3                                                            
OTAM0030 EQU   *                                                                
         MVC   DMCB(4),RCONBKAM    CONTRACT AMOUNT                              
         A     R1,DMCB             ACCUMULATE                                   
         BAS   RE,NEXTEL           GET NEXT BUCKET                              
         BE    OTAM0030            FOUND - ADD IT ALSO                          
*                                                                               
         LTR   R1,R1               ANY VALUE?                                   
         BZ    OTAM0040            NO                                           
         SLDA  R0,1                                                             
         D     R0,=F'100'          DIVIDE VALUE BY 100                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    RF,R1                                                            
         LR    R5,R2               LOAD A(HEADER FIELD)                         
         LA    R5,8(R5)            POINT TO DATA FIELD                          
         EDIT  (RF),(7,(R5)),ALIGN=LEFT                                         
         FOUT  (R2)                                                             
*                                                                               
OTAM0040 EQU   *                                                                
         ZIC   RF,STACTR           BUMP STATION IN PROGRESS                     
         LA    RF,1(RF)                                                         
         STC   RF,STACTR                                                        
         CLC   STACTR,CONCTR       ALL CONTRACTS DONE?                          
         BNH   OTAM0010            NO  - GO BACK AND DO NEXT                    
         LR    RF,RA               YES - RELOAD ORIGINAL CONTRACT               
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   SETCOMTS:  TURN ON APPROPRIATE BITS IN COMMENT FIELD                        
*                                                                               
SETCOMTS NTR1                                                                   
         LA    R2,EP2COM1H         TURN ON PREV VAL FOR ALL COMMTS              
         LA    R3,4                FOUR FIELDS                                  
SECO0010 DS    0H                                                               
         OI    4(R2),X'20'         PREV VALID                                   
         XC    8(65,R2),8(R2)      WIPE OUT FIELD                               
         FOUT  (R2)                INDICATE 'SEND FIELD'                        
         ZIC   RF,0(R2)            POINT TO NEXT FIELD                          
         AR    R2,RF                                                            
         BCT   R3,SECO0010         GO BACK FOR NEXT                             
         XIT1                                                                   
         EJECT                                                                  
EDIT0000 DS    0H                                                               
*                                                                               
*        TEST IF RECORD CAN BE CHANGED                                          
*                                                                               
         MVI   STACTR,1            INITIALIZE STATION IN PROGRESS               
         MVI   DELEPL,C'N'         SET DELETE FLAG TO 'NO'                      
EDIT0010 EQU   *                                                                
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BE    EDIT0020            YES                                          
         GOTO1 NEXTCON#,DMCB,1     NO  - RETRIEVE NEXT FOR UPDATE               
EDIT0020 EQU   *                                                                
         SR    R3,R3                                                            
         LA    R2,RCONELEM                                                      
EDIT0030 IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    EDIT0040            YES                                          
         CLI   0(R2),6             EPL ELEMENT?                                 
         BNE   EDIT0030            NO  - LOOK AT NEXT ELT                       
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   2(2,R2),TWASPLMN                                                 
         DROP  RF                                                               
         BNL   EDIT0040                                                         
         CLI   1(RA),C'*'          DDS TERMINAL                                 
         BE    EDIT0040                                                         
*                                                                               
         LA    R3,SPLERR1                                                       
         LA    R2,CONBACTH                                                      
         B     ERROR                                                            
*                                                                               
EDIT0040 XC    KEY,KEY             GET STATION RECORD                           
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    EDIT0050                                                         
         DC    H'0',C'MISSING STA REC'                                          
EDIT0050 GOTO1 VGETREC,DMCB,IOAREA                                              
         SPACE 1                                                                
         XC    WORK2,WORK2         CLEAR ELEMENT AREA                           
         GOTO1 DATCON,DMCB,(5,WORK2),(2,WORK2+2)                                
*                                  INSERT TODAY'S DATE (COMPRESSED)             
*                                     INTO SPL DATE                             
         OI    WORK2+4,X'08'       TURN ON NEW DATE FORMAT FLAG                 
         MVC   WORK2+5(3),TODAY    INITIALIZE ENTRY DATE W TODAY                
EDIT0060 EQU   *                                                                
         LA    R2,EP2AMT1H         SET A(AMOUNT FIELD)                          
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BE    EDIT0070            YES                                          
         LA    R2,EP2AMT2H                                                      
         CLI   STACTR,2            2ND STATION IN PROGRESS?                     
         BE    EDIT0070            YES                                          
         LA    R2,EP2AMT3H                                                      
         CLI   STACTR,3            3RD STATION IN PROGRESS?                     
         BE    EDIT0070            YES                                          
         LA    R2,EP2AMT4H                                                      
         CLI   STACTR,4            4TH STATION IN PROGRESS?                     
         BE    EDIT0070            YES                                          
         DC    H'0'                CAN'T HAPPEN                                 
EDIT0070 EQU   *                                                                
         XC    RBUYREC(250),RBUYREC                                             
         LA    R6,RBUYREC          KEEP AMOUNTS IN RBUYREC                      
         LA    R8,250(R6)          CALL LETTERS/BAND/                           
         MVC   0(5,R8),RSTAKSTA                                                 
         LA    R3,INVINP                                                        
         CLC   CONACT,=C'EPLR'     GO TO SPECIAL LOGIC FOR EPLR                 
         BE    EDIT0090                                                         
         CLI   DELEPL,C'Y'         DELETE FLAG SET TO 'YES'?                    
         BE    EDIT0071            YES - JUST DO IT!                            
         CLI   5(R2),0                                                          
         BE    EDIT0180            NO INPUT                                     
         TM    4(R2),X'08'                                                      
         BO    EDIT0130            NUMERIC INPUT                                
         TM    4(R2),X'04'                                                      
         BZ    ERROR               NON-ALPHA INPUT                              
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         CH    R1,=H'3'                                                         
         BH    ERROR               CAN'T BE MORE THAN 3                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   EP2AMT1(0),=C'DEL'  DELETE REQUEST?                              
         BNE   EDIT0080            NO                                           
*                                                                               
*   DELETE EPL AND COMMENT ELEMENTS                                             
*                                                                               
         MVI   DELEPL,C'Y'         SET DELETE FLAG TO 'YES'                     
EDIT0071 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(6,RCONREC)                                        
         LA    R2,CONBACTH                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,59                                                  
         CLI   STACTR,1            1ST STATION IN ORDER?                        
         BNE   EDIT0072            NO                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  USE ORIGINAL REC DISK ADDR                   
         DROP  RF                                                               
         B     EDIT0074                                                         
EDIT0072 EQU   *                                                                
         MVC   KEY+28(4),SAVEDADD  USE OTHER REC DISK ADDR                      
EDIT0074 EQU   *                                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         B     EDIT0380                                                         
*                                                                               
EDIT0080 EQU   *                                                                
         MVC   0(4,R6),=C'CAN '                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   EP2AMT1(0),=C'CAN'                                               
         BE    EDIT0140            CHECK THAT CONTRACT CAN BE CANCELLED         
         B     ERROR                                                            
         SPACE                                                                  
* SPECIAL LOGIC FOR ACTION 'EPLR' TO INSERT BUY DOLLARS IN                      
* CONTRACT STATION'S AMOUNT FIELD BY SUMMING THE CONTRACT BUCKETS               
*                                                                               
EDIT0090 EQU   *                                                                
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         SR    R1,R1               CLEAR ACCUMULATOR                            
         LA    RE,RCONELEM                                                      
EDIT0100 CLI   0(RE),0                                                          
         BE    EDIT0110            END OF RECORD                                
         CLI   0(RE),3                                                          
         BNE   *+14                                                             
         SPACE                                                                  
         USING RCONBKEL,RE                                                      
         MVC   FULL,RCONBKAM                                                    
         A     R1,FULL                                                          
         DROP  RE                                                               
         SPACE                                                                  
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     EDIT0100                                                         
         SPACE                                                                  
EDIT0110 EQU   *                                                                
         LTR   R1,R1                                                            
         BZ    EDIT0120                                                         
         SR    R0,R0               DIVIDE AND ROUND OFF THE PENNIES             
         SLDA  R0,1                                                             
         D     R0,=F'100'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R6)                                                         
         SPACE                                                                  
EDIT0120 EQU   *                                                                
         ZIC   R5,STACTR           CON # IN PROGRESS                            
         LA    R2,EP2STA1H         SET A(STATION FIELD)                         
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BE    EDIT0122            YES                                          
         LA    R2,EP2STA2H                                                      
         CLI   STACTR,2            2ND STATION IN PROGRESS?                     
         BE    EDIT0122            YES                                          
         LA    R2,EP2STA3H                                                      
         CLI   STACTR,3            3RD STATION IN PROGRESS?                     
         BE    EDIT0122            YES                                          
         LA    R2,EP2STA4H                                                      
         CLI   STACTR,4            4TH STATION IN PROGRESS?                     
         BE    EDIT0122            YES                                          
         DC    H'0'                CAN'T HAPPEN                                 
EDIT0122 EQU   *                                                                
         GOTO1 INSERTIT,DMCB,RCONKSTA,(R2),(R5)                                 
         OI    6(R2),X'80'                                                      
         B     EDIT0180                                                         
         SPACE 2                                                                
* NUMERIC AMOUNT ROUTINE                                                        
*                                                                               
EDIT0130 GOTO1 VPACK                                                            
         ST    R0,0(R6)                                                         
         B     EDIT0180                                                         
         SPACE 2                                                                
* CANCELLATION ROUTINE                                                          
*                                                                               
EDIT0140 LA    RE,EP2COMPH         FOR INPUT OF CANCEL, CHECK THAT              
         LA    RF,EP2CPENH         ALL COMPETING STATIONS FIELDS                
         MVC   SAVETOT(2),=H'1'    INITIALIZE COUNTER OF MINI ELS               
EDIT0150 CLI   5(RE),0             ARE BLANK.                                   
         BE    *+10                                                             
         LR    R2,RE                                                            
         B     ERROR                                                            
         ZIC   R1,0(RE)                                                         
         AR    RE,R1                                                            
         CR    RE,RF                                                            
         BL    EDIT0150                                                         
         SPACE                                                                  
* CHECK THAT THERE ARE NO BUY DOLLARS FOR A CONTRACT TO BE CANCELLED            
*                                                                               
         SR    R4,R4               CLEAR ACCUMULATOR                            
         LA    R5,RCONELEM         POINT TO FIRST ELEMENT                       
EDIT0160 CLI   0(R5),0                                                          
         BE    EDIT0170                                                         
         CLI   0(R5),3                                                          
         BNE   *+14                                                             
         SPACE                                                                  
         USING RCONBKEL,R5                                                      
         MVC   FULL,RCONBKAM                                                    
         A     R4,FULL                                                          
         DROP  R5                                                               
         SPACE                                                                  
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     EDIT0160                                                         
         SPACE                                                                  
EDIT0170 LTR   R4,R4               TEST FOR ZERO DOLLARS                        
         BZ    EDIT0230            GO AHEAD AND CANCEL                          
         LA    R3,352              CANNOT CANCEL K W/BUYS                       
         B     ERROR                                                            
         SPACE                                                                  
EDIT0180 LA    R6,4(R6)            NEXT AMOUNT AREA                             
         LA    R8,5(R8)            NEXT STATION AREA                            
*                                                                               
         LA    R2,EP2COMPH         COMPETING STATIONS                           
         MVC   SAVETOT(2),=H'1'    INITIALIZE COUNTER OF MINI ELS               
         SPACE 1                                                                
* '100' IN FIRST COMPETING FIELD SIGNALS THAT STATION HAS                       
* WON 100 PERCENT OF BUSINESS                                                   
*                                                                               
         CLC   8(3,R2),=C'100'     LOOK FOR STATION WON ALL KEYWORD             
         BNE   EDIT0190                                                         
         LA    RE,WORK2                                                         
         USING RCONSPEL,RE                                                      
         OI    RCONSPES,X'80'                                                   
         DROP  RE                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               POINT TO NEXT FIELD                          
         LA    RE,EP2CPENH                                                      
         SPACE                                                                  
         SR    R1,R1                                                            
         CLI   5(R2),0             MAKE SURE REST OF COMPETING                  
         BNE   ERROR               FIELDS ARE BLANK                             
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,RE                                                            
         BL    *-16                                                             
         B     EDIT0230            ALL DONE                                     
*                                                                               
* COMPETING STATION VALIDATION                                                  
*                                                                               
EDIT0190 EQU   *                                                                
         CLI   5(R2),0                                                          
         BE    EDIT0220            TRY NEXT FIELD                               
*                                                                               
         MVC   0(5,R8),=C'OTHER'                                                
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'OTHERS'                                               
         BE    EDIT0210                                                         
*                                                                               
         MVC   0(5,R8),MYSPACES    STATION SAVE AREA                            
         LR    R1,R8               TO ADDRESS                                   
         LA    R4,8(R2)            FIELD                                        
         LA    R5,6                LENGTH                                       
         LA    R3,150              INVALID STATION                              
         BAS   RE,TESTAN                                                        
*                                                                               
         CH    R5,=H'3'            LENGTH                                       
         BL    ERROR                                                            
         CLI   0(R4),C'-'          STOP CHARACTER                               
         BNE   ERROR                                                            
         MVC   4(1,R8),1(R4)       BAND                                         
         CLI   4(R8),C'T'                                                       
         BNE   *+8                                                              
         MVI   4(R8),C' '                                                       
*                                                                               
         LA    R3,125              CHECK DUPLICATE STATION                      
         LA    R7,RBUYREC                                                       
         LA    R7,250(R7)                                                       
EDIT0200 CR    R7,R8                                                            
         BE    EDIT0210                                                         
*                                                                               
         CLC   0(5,R7),0(R8)       LAST INPUT IS DUPLICATE                      
         BE    ERROR                                                            
         LA    R7,5(R7)                                                         
         B     EDIT0200                                                         
*                                                                               
EDIT0210 LA    R6,4(R6)            NEXT AMOUNT AREA                             
         LA    R8,5(R8)            NEXT STATION AREA                            
         LH    R1,SAVETOT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,SAVETOT                                                       
*                                                                               
EDIT0220 LA    RF,EP2CPENH         TEST FOR END OF COMPETING                    
         ZIC   R1,0(R2)            STATIONS FIELDS                              
         AR    R2,R1                                                            
         CR    R2,RF                                                            
         BL    EDIT0190                                                         
         B     EDIT0230                                                         
         EJECT                                                                  
EDIT0230 EQU   *                                                                
         LA    R3,RCONELEM                                                      
EDIT0240 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    EDIT0260            YES - NO OLD EPL                             
         CLI   0(R3),6             EPL ELEMENT?                                 
         BE    EDIT0250            YES -                                        
         ZIC   R4,1(R3)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R3,R4                                                            
         B     EDIT0240                                                         
         SPACE 1                                                                
         USING RCONSPEL,R4                                                      
EDIT0250 LA    R4,WORK2                                                         
         MVC   RCONSPDT,RCONSPDT-RCONSPEL(R3)     SAVE OLD ENTRY DATE           
         NI    RCONSPES,X'F7'      TURN OFF DATE FORMAT BIT                     
         MVC   RCONSPYR(2),RCONSPYR-RCONSPEL(R3)                                
*                                  LOAD EPL DATE FROM OLD EPL ELEM              
         TM    RCONSPES-RCONSPEL(R3),X'08'                                      
*                                  TEST DATE FORMAT BIT                         
         BNO   EDIT0252            NOT ON = OLD FORMAT                          
         OI    RCONSPES,X'08'      ON  - NEW FORMAT                             
EDIT0252 EQU   *                                                                
         OC    RCONSPDT,RCONSPDT                                                
         BNZ   *+10                                                             
         MVC   RCONSPDT,TODAY      IF NO OLD USE TODAY                          
         SPACE 1                                                                
EDIT0260 GOTO1 VDELELEM,DMCB,(6,RCONREC)    DELETE OLD SPL                      
         SPACE 1                                                                
         LA    R4,WORK2                                                         
         MVI   0(R4),6             ELEMENT CODE                                 
         MVI   RCONSPNU,0                                                       
         DROP  R4                                                               
         SPACE 1                                                                
         LA    R6,RBUYREC          DOLLARS                                      
         LA    R7,250(R6)          CALL LETTERS                                 
         LA    R3,RCONELEM                                                      
         SR    RE,RE                                                            
EDIT0270 IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    EDIT0280                                                         
         CLI   0(R3),6                                                          
         BH    EDIT0280                                                         
         B     EDIT0270                                                         
         SPACE 1                                                                
EDIT0280 LA    R5,9(R4)                                                         
         SR    R2,R2                                                            
         LH    RF,SAVETOT          COUNTER OF MINI ELS                          
EDIT0290 MVC   0(5,R5),0(R7)      STATION                                       
         MVC   5(4,R5),0(R6)      AMOUNT                                        
         AH    R2,=H'1'                                                         
         LA    R7,5(R7)                                                         
         LA    R6,4(R6)                                                         
         LA    R5,9(R5)                                                         
         BCT   RF,EDIT0290                                                      
         SPACE 1                                                                
         STC   R2,8(R4)         NUMBER OF MINIS                                 
         MH    R2,=H'9'            9 EACH                                       
         AH    R2,=H'9'            PLUS 9 FOR HEADER                            
         STC   R2,1(R4)         ELEMENT LENGTH                                  
         GOTO1 VADDELEM,DMCB,RCONREC,(R4)                                       
         SR    RE,RE                                                            
         IC    RE,1(R4)                                                         
         AR    R3,RE                                                            
         LA    R4,WORK2                                                         
         OC    0(4,R6),0(R6)       ANY MORE                                     
         BNZ   EDIT0280                                                         
         SPACE 1                                                                
*                                                                               
* SET FLAG FOR EPL DATA UPDATED                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    4(R6),X'04'         RCONSENF                                     
         EJECT                                                                  
         LA    R2,EP2COM1H         ADD COMMENTS                                 
         LA    R4,4                                                             
         MVI   FLAGCOMS,C'N'       SET 'NO COMMENT CHANGE' FLAG                 
EDIT0300 EQU   *                                                                
         TM    4(R2),X'20'         LINE PREVIOUSLY VALID?                       
         BNO   EDIT0310            NO  - PROCESS COMMENTS                       
         ZIC   RF,0(R2)            YES - BUMP TO NEXT LINE                      
         AR    R2,RF                                                            
         BCT   R4,EDIT0300         GO BACK AND CHECK NEXT                       
         B     EDIT0350            NO COMMENT CHANGES                           
EDIT0310 EQU   *                                                                
         L     R8,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
         BZ    *+12                                                             
         L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
         GOTO1 VDELELEM,DMCB,(7,RCONREC)                                        
*                                                                               
         LA    R2,EP2COM1H                                                      
         BAS   RE,GETCMT                                                        
         BZ    *+12                NO IMPORTED COMMENT                          
         MVI   FLAGCOMS,C'Y'       COMMENTS CHANGED                             
         B     EDIT0345                                                         
*                                                                               
         LA    R4,4                                                             
EDIT0320 EQU   *                                                                
         CLI   0(R2),0                                                          
         BE    EDIT0340                                                         
         TM    4(R2),X'20'         LINE PREVIOUSLY VALID?                       
         BO    EDIT0330            YES - NO CHANGE IN COMMENTS                  
         MVI   FLAGCOMS,C'Y'       NO  - COMMENTS CHANGED                       
EDIT0330 EQU   *                                                                
         CLI   5(R2),0             NO INPUT, KEEP CHECKING                      
         BE    EDIT0340                                                         
         ZIC   R3,5(R2)                                                         
         XC    WORK2(67),WORK2                                                  
         MVI   WORK2,7                                                          
         LA    R4,2(R3)                                                         
         STC   R4,WORK2+1                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),8(R2)                                                 
*                                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
EDIT0340 ZIC   R3,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R3                                                            
         BCT   R4,EDIT0320         GO BACK FOR NEXT                             
         CLI   FLAGCOMS,C'N'       COMMENTS CHANGED?                            
EDIT0345 DS    0H                                                               
         BE    EDIT0350            NO  - NO DATE CHANGE FOR THIS                
         BAS   RE,TRUDATE          YES - UPDATE TRUE ACT DATE                   
EDIT0350 EQU   *                                                                
         CLI   STACTR,1            1ST STATION IN ORDER?                        
         BNE   EDIT0360            NO                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  USE ORIGINAL REC DISK ADDR                   
         DROP  RF                                                               
         B     EDIT0370                                                         
EDIT0360 EQU   *                                                                
         MVC   KEY+28(4),SAVEDADD  USE OTHER REC DISK ADDR                      
EDIT0370 EQU   *                                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
EDIT0380 EQU   *                                                                
         ZIC   RF,STACTR           CON# IN PROGRESS                             
         LA    RF,1(RF)                                                         
         STC   RF,STACTR                                                        
         CLC   STACTR,CONCTR       ALL CONTRACTS PROCESSED?                     
         BNH   EDIT0010            NO  - GO BACK FOR NEXT                       
         LR    RF,RA               YES - RELOAD ORIGINAL CONTRACT               
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* COMMENT IMPORTING ROUTINE                                                     
*                                                                               
GETCMT   NTR1                                                                   
         CLC   =C'EPL=',8(R2)       HAVE 'EPL=NNNNNNNN' SYNTAX?                 
         BE    GCMT010              YES                                         
         SR    R0,R0                NO - SET CC                                 
         B     EXXMOD               BYE                                         
*                                                                               
GCMT010  DS    0H                                                               
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
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BE    GCMT020                                                          
         LA    R3,762              NO CMT ON SOURCE K                           
         B     ERROR                                                            
GCMT020  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,0(R6)                                      
         BAS   RE,NEXTEL                                                        
         BE    GCMT020                                                          
         LTR   RB,RB                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
*                                                                               
TRUDATE  NTR1                                                                   
         LA    R2,RCONELEM         A(1ST ELEMENT)                               
TDAT0010 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    TDAT0030            YES - NO X'08' FOUND - ADD IT                
         CLI   0(R2),X'08'         X'08'?                                       
         BNE   TDAT0010            NO  - GO BACK FOR NEXT                       
TDAT0020 EQU   *                   YES - ADD TODAYS DATE                        
         USING RCONACEL,R2                                                      
         GOTO1 DATCON,DMCB,(5,RCONACTA),(3,RCONACTA)                            
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         DROP  R2                                                               
*                                                                               
         B     TDAT0040            FINISHED                                     
TDAT0030 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,RCONDATE),(3,TDATELT+5)                           
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         GOTO1 VADDELEM,DMCB,RCONREC,TDATELT                                    
TDAT0040 EQU   *                                                                
         XIT1                                                                   
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
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
         B     ERROR                                                            
TESTANX  BCTR  R0,0                ADJUST COUNT                                 
         SR    R5,R0               GIVES CHARACTER COUNT                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*   RETRIEVE AND SAVE COMBO CONTROL ELEMENT FROM CONTRACT RECORD.               
*      ABORT IF NOT FOUND, BECAUSE WE SHOULDN'T EVEN BE IN THIS                 
*      OVERLAY.                                                                 
*                                                                               
SETCOMBO NTR1                                                                   
         L     R5,0(R1)            RUN OPTION: EDIT OR DISPLAY                  
         MVC   CONKSTA,RCONKSTA    SAVE STATION OF ORIG ORDER                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'        COMBO CONTROL ELEMENT                        
         BAS   RE,GETEL                                                         
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
         CLC   =C'DISP',0(R5)      'DISPLAY' RUN?                               
         BNE   SETC0030            NO                                           
         BAS   RE,DISPSTAS         INSERT STATION CALLS INTO SCREEN             
SETC0030 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*  INSERT STATION CALL LETTERS FOR THIS CONTRACT INTO THE SCREEN                
*                                                                               
         DC    0H'0'                                                            
DISPSTAS NTR1                                                                   
         LA    R5,RCONELEM         A(DESCRIPTIVE ELEMENT)                       
DIST0010 EQU   *                                                                
         ZIC   R0,1(R5)            FIND X'17' ELEMENT                           
         AR    R5,R0                                                            
         CLI   0(R5),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                MUST HAVE AN X'17'                           
         CLI   0(R5),X'17'         X'17' ELEMENT?                               
         BNE   DIST0010            NO                                           
         ZIC   R3,CONCTR           YES - # OF CONTRACTS                         
*                                     IN COMBO ORDER                            
*                                                                               
         BAS   RE,REINSCRN         RESET SCREEN BEFORE HDG LOAD                 
*                                                                               
*  SET STATIONS IN COMBO CONTROL ELEMENT INTO HEADING                           
*                                                                               
         LA    R2,EP2STA1H         A(HDR OF 1ST STATION)                        
         LA    R4,RCONKSTA         A(STATION OF ORDER)                          
         GOTO1 INSERTIT,DMCB,(R4),(R2),1                                        
         LA    R5,2(R5)            1ST STATION IN COMBO CTL ELT                 
         LA    R4,2                SET STATION COUNTER                          
DIST0020 EQU   *                                                                
         CLC   RCONKSTA,0(R5)      STA OF ORDER VS COMBO CONTROL                
         BE    DIST0030            SAME - ALREADY INSERTED - SKIP IT            
         ZIC   R0,0(R2)            BUMP TO NEXT SCRN FLD (AMOUNT)               
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            BUMP TO NEXT SCRN FLD (CALL LTRS)            
         AR    R2,R0                                                            
*                                                                               
*  MOVE STATION TO HEADING                                                      
*                                                                               
         GOTO1 INSERTIT,DMCB,(R5),(R2),(R4)                                     
         LA    R4,1(R4)            INCREMENT STATION COUNTER                    
DIST0030 EQU   *                                                                
         LA    R5,9(R5)            NEXT STATION IN CONTROL ELEMENT              
         BCT   R3,DIST0020         GO BACK FOR NEXT                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   INSERTIT - INSERTS STATION CALL LETTERS INTO SCREEN, TURNS ON               
*     TRANSMIT BIT.  AFTER CALLS ARE INSERTED, THE AMOUNT FIELD FOR             
*     THAT STATION IS UNPROTECTED TO PERMIT ENTRY OF DATA.  THIS                
*     ELIMINATES ANY UNNECESSARY TABBING.                                       
*          P1    =  A(STATION CALL LETTERS IN COMBO CONTROL ELEMENT)            
*          P2    =  SCREEN FIELD HEADER                                         
*          P3    =  STATION COUNTER FOR UNPROTECT                               
*                                                                               
         DC    0H'0'                                                            
FRSTAMNT EQU   EP2AMT1H-EP2STA1H   DISPLACEMENT: HDG TO 1ST AMOUNT              
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
         LA    R1,EP2AMT1H         YES - SET AMOUNT ADDRESS                     
         B     INSE0020                                                         
INSE0011 EQU   *                                                                
         CLI   FULL,2              2ND STATION?                                 
         BNE   INSE0012            NO                                           
         LA    R1,EP2AMT2H         YES - SET AMOUNT ADDRESS                     
         B     INSE0020                                                         
INSE0012 EQU   *                                                                
         CLI   FULL,3              3RD STATION?                                 
         BNE   INSE0013            NO                                           
         LA    R1,EP2AMT3H         YES - SET AMOUNT ADDRESS                     
         B     INSE0020                                                         
INSE0013 EQU   *                                                                
         CLI   FULL,4              4TH STATION?                                 
         BNE   INSE0014            NO                                           
         LA    R1,EP2AMT4H         YES - SET AMOUNT ADDRESS                     
         B     INSE0020                                                         
INSE0014 EQU   *                                                                
         DC    H'00'               ABORT:  INVALID VALUE                        
INSE0020 EQU   *                                                                
         NI    6(R1),X'FF'-X'20'   TURN OFF PROTECT BIT                         
         OI    6(R1),X'80'         TURN ON TRANSMIT BIT                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   REINITIALIZE ALL STATION HEADINGS, PROTECT ALL AMOUNT FIELDS.               
*     THE AMOUNT FIELD FOR A STATION IS UNPROTECTED WHEN STATION                
*     CALL LETTERS ARE INSERTED, BASED ON THE NUMBER OF CONTRACTS               
*     IN THE COMBO CONTROL ELEMENT.  THIS PREVENTS UNNECESSARY                  
*     TABBING.                                                                  
*                                                                               
REINSCRN NTR1                                                                   
         LA    R4,4                LOOP CONTROL                                 
         LA    R1,EP2STA1H                                                      
REIN0010 EQU   *                                                                
         XC    8(7,R1),8(R1)       BLANK OUT HEADING                            
         OI    6(R1),X'80'         TRANSMIT SPACES                              
         ZIC   RF,0(R1)            BUMP TO NEXT FIELD                           
         AR    R1,RF                                                            
         XC    8(7,R1),8(R1)       BLANK OUT AMOUNT                             
         OI    6(R1),X'20'+X'80'   TURN ON PROTECT/TRANSMIT BITS                
         ZIC   R0,0(R1)            BUMP TO NEXT FIELD ON SCREEN                 
         AR    R1,R0                                                            
         BCT   R4,REIN0010         GO BACK FOR NEXT                             
         XIT1                                                                   
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
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+21(2),TWAAGY    INSERT REP ID                                
         DROP  RF                                                               
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
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   EDITTOTS:  CHECK ALL TOTALS FOR THIS SCREEN.  IF ANY ARE IN                 
*     ERROR, SEND BACK NON-ZERO CONDITION CODE, AND ADDRESS OF                  
*     FIELD IN ERROR IN FULL.                                                   
*                                                                               
EDITTOTS NTR1                                                                   
         ZIC   R3,CONCTR           # OF CONTRACTS IN ORDER                      
         LA    R2,EP2AMT1H         1ST AMOUNT HEADER                            
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
         AR    R2,R0               BUMP TO NEXT FIELD (CALL LTRS)               
         ZIC   R0,0(R2)            LENGTH OF FIELD                              
         AR    R2,R0               BUMP TO NEXT FIELD (AMOUNT )                 
         BCT   R3,EDTO0010         GO BACK FOR NEXT                             
         LA    RF,0                SET ZERO CC                                  
         B     EDTO0050            EXIT WITH NO ERROR                           
EDTO0040 EQU   *                                                                
         LA    RF,1                SET NON-ZERO CC                              
EDTO0050 EQU   *                                                                
         LTR   RF,RF                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   LOCAL STORAGE                                                               
*                                                                               
COMBOCTL DS    CL60                STORAGE FOR COMBO CONTROL ELEMENT            
CONCTR   DS    XL1                 CONTRACT COUNTER                             
STACTR   DS    XL1                 STATION COUNT IN PROGRESS                    
CONKSTA  DS    CL5                 STATION OF ORIGINAL ORDER                    
SAVEDADD DS    CL4                 SAVE AREA FOR DISK ADDRESS                   
DELEPL   DS    CL1                 Y  =  DELETE FOR ALL CONTRACTS               
*                                                                               
*        EQUATES       *                                                        
YES      EQU   C'Y'                                                             
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTE1D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037RECNT4C   05/01/02'                                      
         END                                                                    
