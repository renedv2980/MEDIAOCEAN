*          DATA SET RECNT51    AT LEVEL 030 AS OF 05/01/02                      
*PHASE T80251A,+0                                                               
         TITLE 'T80251 - EPL DISPLAY/EDIT'                                      
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT51 (T80251) --- EPL DISPLAY/EDIT                    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 26MAR90 (EFJ) --- ADDED TOMBSTONE (HISTORY LOST)                *             
*                                                                 *             
* CNT HISTORY:                                                    *             
*                                                                 *             
* 05APR90 (EFJ) --- USE GETEL MACRO                               *             
*                                                                 *             
* 03AUG90 (EFJ) --- COMBINE WITH CNT52                            *             
*                                                                 *             
* 10JAN92 (SKU) --- STORED COMMENTS SUPPORT                       *             
*                   *** THIS CHANGE NOT YET RELEASED ***          *             
*                                                                 *             
* 23OCT92 (BU ) --- CHANGE DATE ENTRY/DISPLAY FACILITY FOR EPL    *             
*     NOTE!!!     ++INCLUDE RECNTFAAD MUST BE CHANGED TO RECNTFAD *             
*                     BEFORE PROGRAM RELEASED, AND LIBRARY ALSO   *             
*                     UPDATED  !!!                                *             
*                                                                 *             
* 08APR93 (SKU) --- BUG FIX EPL COMMENTS                          *             
*                                                                 *             
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 23JUL97 (SKU) --- 4K CONTRACT SUPPORT                           *             
*                                                                 *             
* 06JAN98 (RHV) --- EPL=NNNNN COMMENT IMPORTING                   *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80251   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80251*                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R9,RA                                                            
         AH    R9,=H'4096'         4K                                           
         USING TWAWORK,R9                                                       
         L     R2,4(R1)                                                         
         CLC   =C'DISP',0(R2)                                                   
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)                                                   
         BE    EDIT                                                             
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
         NI    TWASTAT,X'80'                                                    
         GOTO1 VFOUTBLK,DMCB,EPLMONTH,EPLLAST,0                                 
         SPACE 1                                                                
         LA    R4,EPLCMMTH                                                      
         LA    R2,EPLCOMPH                                                      
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
DISP0050 EQU   *                                                                
         FOUT  EPLSTANH,CONSTA,7        BLAIR STATION                           
         MVC   EPLTDAT,MYSPACES    WIPE OUT FIELD                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'08'        RETRIEVE TRUE ACT DATE FIRST                 
         BAS   RE,GETEL                                                         
         BNE   DISP0060            NOT FOUND                                    
         GOTO1 DATCON,DMCB,(3,5(R6)),(5,EPLTDAT)                                
DISP0060 EQU   *                                                                
         FOUT  EPLTDATH                                                         
         BAS   RE,SETCOMTS         SET APPROPRIATE COMMENT CONTRL               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0170                                                         
         SPACE 1                                                                
         USING RCONSPEL,R6                                                      
         TM    RCONSPES,X'08'      TEST DATE FORMAT                             
         BO    DISP0070            ON  =  NEW FORMAT                            
         MVC   FULL,RCONSPYR       OFF  =  OLD FORMAT                           
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),(9,EPLMONT)                                 
         B     DISP0080                                                         
DISP0070 EQU   *                                                                
         GOTO1 DATCON,DMCB,(2,RCONSPYR),(5,EPLMONT)                             
DISP0080 EQU   *                                                                
         FOUT  EPLMONTH                                                         
*                                                                               
         CLC   RCONSPAM(2),=C'NA'                                               
         BE    DISP0100                                                         
         CLC   RCONSPAM(3),=C'CAN'                                              
         BNE   DISP0090                                                         
         MVC   EPLAMNT,MYSPACES                                                 
         MVC   EPLAMNT(3),=C'CAN'                                               
         FOUT  EPLAMNTH                                                         
         B     DISP0100                                                         
         SPACE                                                                  
DISP0090 EDIT  (B4,RCONSPAM),(7,EPLAMNT),ALIGN=LEFT                             
         FOUT  EPLAMNTH                                                         
         B     DISP0100                                                         
         SPACE                                                                  
DISP0100 EQU   *                                                                
         TM    RCONSPES,X'80'                                                   
         BZ    DISP0110                                                         
         LA    R2,EPLCOMPH         DISPLAY STATION WON ALL KEYWORD              
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
         LA    R2,EPLCOMPH                                                      
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
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0160                                                         
         LA    R2,EPLCOM1H                                                      
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
         BAS   RE,NEXTEL                                                        
         BNE   DISP0160                                                         
         ZIC   R3,0(R2)                                                         
         AR    R2,R3               NEXT FIELD                                   
         BCT   R4,DISP0150         DISPLAY NEXT                                 
         SPACE 1                                                                
DISP0160 B     EXXMOD                                                           
*                                                                               
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
         MVC   EPLMONT,MYSPACES    CLEAR AND FOUT MONTH FOR CURSOR              
         FOUT  EPLMONTH                                                         
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
         EDIT  (RF),(7,EPLAMNT),ALIGN=LEFT    TOTAL REP BUYS                    
         FOUT  EPLAMNTH                                                         
         SPACE 1                                                                
DISP0200 LA    R2,CONBACTH                                                      
         B     DISP0160            DO NOT PUT OUT STATIONS                      
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   SETCOMTS:  TURN ON APPROPRIATE BITS IN COMMENT FIELD                        
*                                                                               
SETCOMTS NTR1                                                                   
         LA    R2,EPLCOM1H         TURN ON PREV VAL FOR ALL COMMTS              
         LA    R3,4                FOUR FIELDS                                  
SETC0010 DS    0H                                                               
         OI    4(R2),X'20'         PREV VALID                                   
         XC    8(65,R2),8(R2)      WIPE OUT FIELD                               
         FOUT  (R2)                INDICATE 'SEND FIELD'                        
         ZIC   RF,0(R2)            POINT TO NEXT FIELD                          
         AR    R2,RF                                                            
         BCT   R3,SETC0010         GO BACK FOR NEXT                             
         XIT1                                                                   
         EJECT                                                                  
EDIT     DS    0H                                                               
*        TEST IF RECORD CAN BE CHANGED                                          
         SR    R3,R3                                                            
         LA    R2,RCONELEM                                                      
EDIT0010 IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BE    EDIT0020                                                         
         CLI   0(R2),6                                                          
         BNE   EDIT0010                                                         
*                                                                               
         CLC   2(2,R2),TWASPLMN                                                 
         BNL   EDIT0020                                                         
         CLI   1(RA),C'*'          DDS TERMINAL                                 
         BE    EDIT0020                                                         
*                                                                               
         LA    R3,SPLERR1                                                       
         LA    R2,CONBACTH                                                      
         B     ERROR                                                            
         EJECT                                                                  
EDIT0020 XC    KEY,KEY             GET STATION RECORD                           
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    EDIT0030                                                         
         DC    H'0',C'MISSING STA REC'                                          
         DS    0H                                                               
EDIT0030 EQU   *                   BUILD NEW X'06' ELEMENT                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         XC    WORK2,WORK2         CLEAR ELEMENT AREA                           
         GOTO1 DATCON,DMCB,(5,WORK2),(2,WORK2+2)                                
*                                  INSERT TODAY'S DATE (COMPRESSED)             
*                                     INTO SPL DATE                             
         OI    WORK2+4,X'08'       TURN ON NEW DATE FORMAT FLAG                 
         MVC   WORK2+5(3),TODAY    INITIALIZE ENTRY DATE W TODAY                
*                                                                               
EDIT0040 LA    R2,EPLAMNTH                                                      
         XC    RBUYREC(250),RBUYREC                                             
         LA    R6,RBUYREC          KEEP AMOUNTS IN RBUYREC                      
         LA    R8,250(R6)          CALL LETTERS/BAND/                           
         MVC   0(5,R8),RSTAKSTA                                                 
         LA    R3,INVINP                                                        
         CLC   CONACT,=C'EPLR'     GO TO SPECIAL LOGIC FOR EPLR                 
         BE    EDIT0050                                                         
         CLI   5(R2),0             ANY DATA IN FIELD?                           
         BE    EDIT0140            NO  -                                        
         TM    4(R2),X'08'         IS INPUT NUMERIC?                            
         BO    EDIT0090            YES                                          
         TM    4(R2),X'04'                                                      
         BZ    ERROR               NON-ALPHA INPUT                              
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         CH    R1,=H'3'                                                         
         BH    ERROR               CAN'T BE MORE THAN 3                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   EPLAMNT(0),=C'DEL'                                               
         BE    EDIT0190            DELETE EPL AND COMMENT ELEMENTS              
         MVC   0(4,R6),=C'CAN '                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   EPLAMNT(0),=C'CAN'                                               
         BE    EDIT0100            CHECK THAT CONTRACT CAN BE CANCELLED         
         B     ERROR                                                            
         SPACE                                                                  
* SPECIAL LOGIC FOR ACTION 'EPLR' TO INSERT BUY DOLLARS IN                      
* CONTRACT STATION'S AMOUNT FIELD BY SUMMING THE CONTRACT BUCKETS               
*                                                                               
EDIT0050 EQU   *                                                                
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         SR    R1,R1               CLEAR ACCUMULATOR                            
         LA    RE,RCONELEM                                                      
EDIT0060 CLI   0(RE),0                                                          
         BE    EDIT0070            END OF RECORD                                
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
         B     EDIT0060                                                         
         SPACE                                                                  
EDIT0070 EQU   *                                                                
         LTR   R1,R1                                                            
         BZ    EDIT0080                                                         
         SR    R0,R0               DIVIDE AND ROUND OFF THE PENNIES             
         SLDA  R0,1                                                             
         D     R0,=F'100'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R6)                                                         
         SPACE                                                                  
EDIT0080 EQU   *                                                                
         LA    R2,EPLSTANH                                                      
         MVC   EPLSTAN,CONSTA      DISPLAY BACK THE BLAIR STATION               
         OI    6(R2),X'80'                                                      
         B     EDIT0140                                                         
         SPACE 2                                                                
* NUMERIC AMOUNT ROUTINE                                                        
*                                                                               
EDIT0090 GOTO1 VPACK                                                            
         ST    R0,0(R6)                                                         
         B     EDIT0140                                                         
         SPACE 2                                                                
* CANCELLATION ROUTINE                                                          
*                                                                               
EDIT0100 LA    RE,EPLCOMPH         FOR INPUT OF CANCEL, CHECK THAT              
         LA    RF,EPLCPENH         ALL COMPETING STATIONS FIELDS                
         MVC   SAVETOT(2),=H'1'    INITIALIZE COUNTER OF MINI ELS               
EDIT0110 CLI   5(RE),0             ARE BLANK.                                   
         BE    *+10                                                             
         LR    R2,RE                                                            
         B     ERROR                                                            
         ZIC   R1,0(RE)                                                         
         AR    RE,R1                                                            
         CR    RE,RF                                                            
         BL    EDIT0110                                                         
         SPACE                                                                  
* CHECK THAT THERE ARE NO BUY DOLLARS FOR A CONTRACT TO BE CANCELLED            
*                                                                               
         SR    R4,R4               CLEAR ACCUMULATOR                            
         LA    R5,RCONELEM         POINT TO FIRST ELEMENT                       
EDIT0120 CLI   0(R5),0                                                          
         BE    EDIT0130                                                         
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
         B     EDIT0120                                                         
         SPACE                                                                  
EDIT0130 LTR   R4,R4               TEST FOR ZERO DOLLARS                        
         BZ    EDIT0200            GO AHEAD AND CANCEL                          
         LA    R3,352              CANNOT CANCEL K W/BUYS                       
         B     ERROR                                                            
         SPACE                                                                  
EDIT0140 LA    R6,4(R6)            NEXT AMOUNT AREA                             
         LA    R8,5(R8)            NEXT STATION AREA                            
*                                                                               
         LA    R2,EPLCOMPH         COMPETING STATIONS                           
         MVC   SAVETOT(2),=H'1'    INITIALIZE COUNTER OF MINI ELS               
*                                                                               
* '100' IN FIRST COMPETING FIELD SIGNALS THAT STATION HAS                       
* WON 100 PERCENT OF BUSINESS                                                   
*                                                                               
         CLC   8(3,R2),=C'100'     LOOK FOR STATION WON ALL KEYWORD             
         BNE   EDIT0150                                                         
         LA    RE,WORK2                                                         
         USING RCONSPEL,RE                                                      
         OI    RCONSPES,X'80'                                                   
         DROP  RE                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               POINT TO NEXT FIELD                          
         LA    RE,EPLCPENH                                                      
         SPACE                                                                  
         SR    R1,R1                                                            
         CLI   5(R2),0             MAKE SURE REST OF COMPETING                  
         BNE   ERROR               FIELDS ARE BLANK                             
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,RE                                                            
         BL    *-16                                                             
         B     EDIT0200            ALL DONE                                     
*                                                                               
* COMPETING STATION VALIDATION                                                  
*                                                                               
EDIT0150 EQU   *                                                                
         CLI   5(R2),0                                                          
         BE    EDIT0180            TRY NEXT FIELD                               
*                                                                               
         MVC   0(5,R8),=C'OTHER'                                                
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'OTHERS'                                               
         BE    EDIT0170                                                         
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
EDIT0160 CR    R7,R8                                                            
         BE    EDIT0170                                                         
*                                                                               
         CLC   0(5,R7),0(R8)       LAST INPUT IS DUPLICATE                      
         BE    ERROR                                                            
         LA    R7,5(R7)                                                         
         B     EDIT0160                                                         
*                                                                               
EDIT0170 LA    R6,4(R6)            NEXT AMOUNT AREA                             
         LA    R8,5(R8)            NEXT STATION AREA                            
         LH    R1,SAVETOT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,SAVETOT                                                       
*                                                                               
EDIT0180 LA    RF,EPLCPENH         TEST FOR END OF COMPETING                    
         ZIC   R1,0(R2)            STATIONS FIELDS                              
         AR    R2,R1                                                            
         CR    R2,RF                                                            
         BL    EDIT0150                                                         
         B     EDIT0200                                                         
         EJECT                                                                  
EDIT0190 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(6,RCONREC)                                        
         LA    R2,CONBACTH                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,59                                                  
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO2                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         B     EXIT                                                             
         EJECT                                                                  
EDIT0200 EQU   *                                                                
         LA    R3,RCONELEM                                                      
EDIT0210 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    EDIT0240            YES - NO OLD EPL                             
         CLI   0(R3),6             EPL ELEMENT?                                 
         BE    EDIT0220            YES -                                        
         ZIC   R4,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R4                                                            
         B     EDIT0210                                                         
         SPACE 1                                                                
         USING RCONSPEL,R4                                                      
EDIT0220 EQU   *                                                                
         LA    R4,WORK2                                                         
         MVC   RCONSPDT,RCONSPDT-RCONSPEL(R3)     SAVE OLD ENTRY DATE           
         NI    RCONSPES,X'F7'      TURN OFF DATE FORMAT BIT                     
         MVC   RCONSPYR(2),RCONSPYR-RCONSPEL(R3)                                
*                                  LOAD EPL DATE FROM OLD EPL ELEM              
         TM    RCONSPES-RCONSPEL(R3),X'08'                                      
*                                  TEST DATE FORMAT BIT                         
         BNO   EDIT0230            NOT ON = OLD FORMAT                          
         OI    RCONSPES,X'08'      ON  - NEW FORMAT                             
EDIT0230 EQU   *                                                                
         OC    RCONSPDT,RCONSPDT                                                
         BNZ   *+10                                                             
         MVC   RCONSPDT,TODAY      IF NO OLD USE TODAY                          
         SPACE 1                                                                
EDIT0240 GOTO1 VDELELEM,DMCB,(6,RCONREC)    DELETE OLD SPL                      
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
EDIT0250 IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    EDIT0260                                                         
         CLI   0(R3),6                                                          
         BH    EDIT0260                                                         
         B     EDIT0250                                                         
         SPACE 1                                                                
EDIT0260 LA    R5,9(R4)                                                         
         SR    R2,R2                                                            
         LH    RF,SAVETOT          COUNTER OF MINI ELS                          
EDIT0270 MVC   0(5,R5),0(R7)      STATION                                       
         MVC   5(4,R5),0(R6)      AMOUNT                                        
         AH    R2,=H'1'                                                         
         LA    R7,5(R7)                                                         
         LA    R6,4(R6)                                                         
         LA    R5,9(R5)                                                         
         BCT   RF,EDIT0270                                                      
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
         BNZ   EDIT0260                                                         
         SPACE 1                                                                
*                                                                               
* SET FLAG FOR EPL DATA UPDATED                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    4(R6),X'04'         RCONSENF                                     
         EJECT                                                                  
         LA    R2,EPLCOM1H         ADD COMMENTS                                 
         LA    R4,4                                                             
         MVI   FLAGCOMS,C'N'       SET 'NO COMMENT CHANGE' FLAG                 
EDIT0280 EQU   *                                                                
         TM    4(R2),X'20'         LINE PREVIOUSLY VALID?                       
         BNO   EDIT0290            NO  - PROCESS COMMENTS                       
         ZIC   RF,0(R2)            YES - BUMP TO NEXT LINE                      
         AR    R2,RF                                                            
         BCT   R4,EDIT0280         GO BACK AND CHECK NEXT                       
         B     EDIT0330            NO COMMENT CHANGES                           
         SPACE 1                                                                
EDIT0290 EQU   *                                                                
         L     R8,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
         BZ    *+12                                                             
         L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
         GOTO1 VDELELEM,DMCB,(7,RCONREC)                                        
*                                  DELETE ALL COMMENT ELEMENTS                  
         LA    R2,EPLCOM1H                                                      
*                                                                               
         BAS   RE,GETCMT           IMPORTED COMMENT CHECK ROUTINE               
         BZ    *+12                NO IMPORTED COMMENT                          
         MVI   FLAGCOMS,C'Y'       COMMENTS CHANGED                             
         B     EDIT0325                                                         
*                                                                               
         LA    R5,4                                                             
EDIT0300 EQU   *                                                                
         CLI   0(R2),0                                                          
         BE    EDIT0320                                                         
         TM    4(R2),X'20'         LINE PREVIOUSLY VALID?                       
         BO    EDIT0310            YES - NO CHANGE IN COMMENTS                  
         MVI   FLAGCOMS,C'Y'       NO  - COMMENTS CHANGED                       
EDIT0310 EQU   *                                                                
         CLI   5(R2),0             NO INPUT, KEEP CHECKING                      
         BE    EDIT0320                                                         
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
EDIT0320 ZIC   R3,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R3                                                            
         BCT   R5,EDIT0300         GO BACK FOR NEXT                             
EDIT0325 DS    0H                                                               
         CLI   FLAGCOMS,C'N'       COMMENTS CHANGED?                            
         BE    EDIT0330            NO  - NO DATE CHANGE FOR THIS                
         BAS   RE,TRUDATE          YES - UPDATE TRUE ACT DATE                   
EDIT0330 MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO2                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
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
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
*                                                                               
*   NOTE:  LIBRARY CHANGED FOR TESTING!!!   SEE NOTE IN TOMBSTONE!!!            
*                                                                               
       ++INCLUDE RECNTFAAD                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030RECNT51   05/01/02'                                      
         END                                                                    
