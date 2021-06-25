*          DATA SET SPSFM73    AT LEVEL 012 AS OF 11/13/08                      
*PHASE T21773A                                                                  
*INCLUDE SPOMCOM                                                                
T21773   TITLE 'SPSFM73 - ORDER MANAGER COMMENT PREVIEW'                        
                                                                                
*=========================================================                      
* DISPLAY COMMENTS IN THE FOLLOWING SEQUENCE                                    
* OCOMS         MD/CLT/PRD/EST FLT                                              
* ORDCOMS       FROM THIS ORDER                                                 
* STDCOMS       STANDARD COMMENTS                                               
* BYRCOMS       BUYER COMMENTS                                                  
*=========================================================                      
                                                                                
T21773   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21773*,R7,RR=R3                                              
         ST    R3,RELO                                                          
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R8,SYSSPARE                                                      
         USING MYAREAD,R8                                                       
         SPACE 1                                                                
*===========================================================                    
* NOTE THAT RECACT3 FLAG CAUSES ONLY VALKEY/VALREC                              
*===========================================================                    
         SPACE 1                                                                
         GOTO1 INITPFKY,DMCB,PFKEYTAB  INITIALIZE THE PFKEYS                    
*                                                                               
         CLI   CALLSP,0            TEST NEED RETURN PFKEY                       
         BE    CHKMODE                                                          
         MVC   COMPFK+24(6),=C'PF12=R'                                          
         MVC   COMPFK+30(5),=X'85A3A49995'  AVOID LOWERCASE !                   
         LA    R2,COMPFKH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
CHKMODE  CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
*                                                                               
EXIT     XIT1                                                                   
RELO     DS    A                                                                
         EJECT                                                                  
*==========================================================                     
* VALIDATE THE KEY                                                              
*==========================================================                     
         SPACE 1                                                                
VK       DS    0H                                                               
         TM    COMMEDH+4,X'20'     TEST ANY KEY FIELDS CHANGED                  
         BZ    VK0                                                              
         TM    COMORDH+4,X'20'                                                  
         BZ    VK0                                                              
         B     EXIT                                                             
*                                                                               
VK0      MVI   SVFIRST,0           RESET                                        
         MVI   SVLAST,0                                                         
*                                                                               
         BAS   RE,CLRCOM           CLEAR PREVIOUS COMMENTS                      
         XC    COMCOUNT,COMCOUNT   SET NO COMMENTS SAVED                        
*                                                                               
VK10     LA    R2,COMEXPH                                                       
         XC    COMEXP,COMEXP       CLEAR EXPANDED KEY                           
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    SVKEY,SVKEY                                                      
K        USING DOKEY,SVKEY                                                      
         MVC   K.DOKTYPE(2),=X'0D34'                                            
*                                                                               
         LA    R2,COMMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
         MVC   K.DOKAGMD,BAGYMD                                                 
*                                                                               
         LA    R2,COMORDH          VALIDATE ORDER NUMBER                        
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   5(R2),8             YJJJSSSS - YEAR,JULIAN,SEQ                   
         BNE   INVLFLD                                                          
         TM    4(R2),X'08'         HAS TO BE NUMERIC?                           
         BZ    INVLFLD                                                          
*                                                                               
         BAS   RE,VALORDR                                                       
         MVC   K.DOKORDER,BINORD                                                
         OI    4(R2),X'20'                                                      
         DROP  K                                                                
*                                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE      TEST ORDER FOUND                             
         BNE   BADORDER                                                         
         MVC   SVKEY,KEY           SAVE ENTIRE KEY                              
         EJECT                                                                  
*========================================================                       
* READ THE ORDER AND DISPLAY MD/CLT/PRD/EST/STATION/FLT                         
*========================================================                       
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOIDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING DOIDEL,R6                                                        
*                                                                               
         MVC   BCLT,DOIDCLT        SAVE IN SAVED STORAGE                        
         MVC   BPRD,DOIDPRD                                                     
         MVC   BPRD2,DOIDPRD2                                                   
         MVC   BEST,DOIDEST                                                     
         MVC   BSLN,DOIDFLTN                                                    
         MVC   SVBUYER,DOIDBYR                                                  
         MVC   SVSCOM1,DOISCOM1    SAVE STANDARD COMMENT CODES                  
         MVC   SVSCOM2,DOISCOM2                                                 
         MVC   SVSCOM3,DOISCOM3                                                 
*                                                                               
         LA    R4,COMEXP           POINT TO EXPANSION AREA                      
         MVC   0(1,R4),COMMED      MEDIA                                        
         LA    R4,2(R4)                                                         
         MVC   0(3,R4),DOIDBYR                                                  
         LA    R4,4(R4)                                                         
* READ CLIENT RECORD                                                            
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),DOIDCLT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AIO                                                           
         SR    R0,R0                                                            
         IC    R0,CPROF+6-CLTHDR(RE)   GET CLUNPK FORMAT                        
         GOTO1 CLUNPK,DMCB,((R0),DOIDCLT),0(R4)                                 
*                                                                               
         LA    R4,3(R4)                                                         
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,2(R4)            POINT TO NEXT OUTPUT POSN                    
*                                                                               
         LA    R1,DOIDPRD          POINT TO PRODUCT 1                           
         BAS   RE,GETPRD                                                        
         MVC   0(3,R4),0(RF)       MOVE EBCDIC PRD                              
         LA    R4,3(R4)                                                         
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,2(R4)                                                         
*                                                                               
         CLI   DOIDPRD2,0                                                       
         BE    VK22                                                             
         BCTR  R4,0                                                             
         MVI   0(R4),C'-'                                                       
         LA    R1,DOIDPRD2                                                      
         BAS   RE,GETPRD                                                        
         MVC   1(3,R4),0(RF)                                                    
         LA    R4,4(R4)                                                         
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
*                                                                               
VK22     SR    R0,R0                                                            
         IC    R0,DOIDEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         LA    R4,4(R4)                                                         
         CLI   DOIDFLTN,0                                                       
         BE    VK24                                                             
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C'/'                                                       
         SR    R0,R0                                                            
         IC    R0,DOIDFLTN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R4),DUB                                                      
         LA    R4,4(R4)                                                         
*                                                                               
VK24     GOTO1 MSUNPK,DMCB,DOISTA-2,FULL,0(R4)                                  
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 2                                                                
GETPRD   L     RF,AIO2                                                          
         LA    RF,CLIST-CLTHDR(RF)                                              
*                                                                               
GETPRD2  CLC   0(1,R1),3(RF)       MATCH PRDCODE                                
         BER   RE                                                               
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   GETPRD2                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*============================================================                   
* CONVERTS EBCDIC ORDER NUMBER TO BINARY ORDER NUMBER                           
* ON ENTRY R2 POINTS TO ORDER NUMBER FLDHDR                                     
* RETURN BINARY ORDER NUMBER IN BINORD                                          
*============================================================                   
         SPACE 1                                                                
VALORDR  NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,0),(15,TODAYJ)     GET CURRENT DATE               
         ZAP   FULL,TODAYJ         CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,8(R2),DUB,8   GET ORDER AS HEX                        
*                                                                               
         MVC   PACKOF4B,DUB        CONVERT IT TO PACKED                         
         OI    PACKOF4B+3,X'0F'                                                 
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2   COPY CURRENT CENTURY                      
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),DUB   STICK IN DAYS IN YEAR                        
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=X'FFFF'                                                
*                                                                               
         PACK  DUB,8+4(4,R2)       SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=X'FFFF'                                                
         B     EXIT                                                             
         EJECT                                                                  
*==============================================                                 
* RETRIEVE AND DISPLAY THE COMMENTS                                             
*==============================================                                 
         SPACE 1                                                                
VR       DS    0H                                                               
         OC    COMCOUNT,COMCOUNT   TEST SAVED COMMENTS                          
         BZ    VR0                 NO - BUILD NOW                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',(1,0),ATIA                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VR10                                                             
*                                                                               
VR0      XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OMCOMD,R1                                                        
*                                                                               
         XC    WORK,WORK                                                        
         MVI   OMCACT,C'G'                                                      
         MVC   OMCLBUFF,=Y(7000)                                                
         MVC   OMCABUFF,ATIA                                                    
         MVC   OMCACOMF,ACOMFACS                                                
         MVC   OMCAGMD,BAGYMD                                                   
         MVC   OMCCLT,BCLT                                                      
         MVC   OMCPRD,BPRD                                                      
         MVC   OMCEST,BEST                                                      
         MVC   OMCFLT,BSLN                                                      
         GOTO1 =V(SPOMCOM),WORK                                                 
         CLI   OMCERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   COMCOUNT,OMCCOUNT   SAVE COMMENT COUNT                           
         SPACE 1                                                                
*==========================================================                     
* NOW READ OTHER COMMENTS - BUYER/STANDARD/ORDER/STATION                        
* THEN WRITE OFF TEMPSTR PAGE 1 WITH COMMENT DATA                               
*==========================================================                     
         SPACE 1                                                                
         BAS   RE,GETORD                                                        
         BAS   RE,GETSTD                                                        
         BAS   RE,GETBYR                                                        
*                                                                               
         OC    COMCOUNT,COMCOUNT   ANY DATA                                     
         BZ    NOCOMMNT                                                         
*                                                                               
         LH    R4,COMCOUNT                                                      
         MHI   R4,80                                                            
         A     R4,ATIA             POINT TO CURRENT END OF BUFFER               
         MVC   0(8,R4),=C'XXXXXXXX'                                             
         MVC   8(23,R4),=C'*** END OF COMMENTS ***'                             
         LH    R0,COMCOUNT                                                      
         AHI   R0,1                                                             
         STH   R0,COMCOUNT                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(1,0),ATIA                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*============================================================                   
* COMMENT BUFFER BUILT/RESTORED IN TIA                                          
* NOW DISPLAY COMMENTS                                                          
*============================================================                   
         SPACE 1                                                                
VR10     LA    R2,COMSRC1H                                                      
         USING LINED,R2                                                         
         XC    SVLINEID,SVLINEID   ALWAYS DISPLAY SRCE ON LINE 1                
*                                                                               
         CLI   SVFIRST,0           TEST FIRST TIME                              
         BE    VR20                YES - IGNORE PFKEYS                          
         CLI   PFKEY,6                                                          
         BNE   VR12                                                             
         MVI   SVFIRST,1           SET TO START AT TOP                          
         MVI   SVLAST,1            LAST GOES ON LINE 1                          
         B     VR20                                                             
*                                                                               
VR12     CLI   PFKEY,7             TEST SCROLL UP                               
         BNE   VR14                                                             
         SR    R5,R5                                                            
         IC    R5,SVFIRST          GET CURRENT SCREEN START                     
         AHI   R5,-9               BACK UP 1/2 SCREEN (9 LINES)                 
         BP    *+8                                                              
         LHI   R5,1                ELSE START AT LINE 1                         
         B     VR16                                                             
*                                                                               
VR14     CLI   PFKEY,8             TEST SCROLL DOWN                             
         BNE   VR20                                                             
*                                                                               
         SR    R5,R5                                                            
         IC    R5,SVLAST                                                        
         CH    R5,COMCOUNT         TEST ALL ON THE SCREEN ALREADY               
         BNL   EXIT                YES - IGNORE PFKEY                           
*                                                                               
         SR    R5,R5                                                            
         IC    R5,SVFIRST                                                       
         AHI   R5,9                SCROLL DOWN 1/2 SCREEN                       
*                                                                               
         CH    R5,COMCOUNT         COMPARE TO ACTUAL LINES                      
         BH    EXIT                IF HIGH, LEAVE CURRENT DSPLY                 
*                                                                               
VR16     STC   R5,SVFIRST                                                       
         STC   R5,SVLAST                                                        
*                                                                               
VR20     L     R4,ATIA                                                          
         SR    R5,R5                                                            
         ICM   R5,1,SVFIRST        TEST FIRST TIME                              
         BNZ   VR22                                                             
         LHI   R5,1                                                             
         STC   R5,SVFIRST                                                       
         STC   R5,SVLAST                                                        
         B     VR30                                                             
*                                                                               
VR22     IC    R5,SVLAST                                                        
         BCTR  R5,0                                                             
         MHI   R5,80               DSPL TO LAST ENTRY                           
         AR    R4,R5               SET TO START WITH LAST LINE                  
         CLI   80(R4),0            TEST ANY MORE DATA                           
         BE    EXIT                                                             
*                                                                               
         BAS   RE,CLRCOM           CLEAR SCREEN                                 
*                                                                               
         MVC   SVFIRST,SVLAST      LAST BECOMES NEW FIRST                       
         SR    R5,R5                                                            
         IC    R5,SVFIRST          SET COUNTER                                  
*                                                                               
VR30     CLC   SVLINEID,0(R4)      SAME SOURCE AS PREVIOUS                      
         BE    VR34                                                             
*                                                                               
VR32     MVC   LINEID,0(R4)        MOVE SOURCE                                  
         OI    LINEIDH+6,X'80'                                                  
*                                                                               
VR34     MVC   SVLINEID,0(R4)      SAVE CURRENT                                 
         MVC   LINEDATA,8(R4)                                                   
         OC    LINEDATA,=80C' '    MAKE UPPERCASE FOR NOW                       
         OI    LINEDATH+6,X'80'                                                 
*                                                                               
         STC   R5,SVLAST           SET LAST LINE NUMBER DISPLAYED               
         AHI   R5,1                                                             
*                                                                               
         LA    R4,80(R4)           NEXT BUFFER ENTRY                            
         CLI   0(R4),0                                                          
         BE    EXIT                                                             
         AHI   R2,LINELEN                                                       
         LA    R0,COMCOMXH                                                      
         CR    R2,R0                                                            
         BNH   VR30                                                             
         B     EXIT                                                             
         EJECT                                                                  
CLRCOM   NTR1                                                                   
         LA    R2,COMSRC1H                                                      
         USING LINED,R2                                                         
         LA    R0,COMCOMXH                                                      
*                                                                               
CLRCOM2  XC    LINEID,LINEID                                                    
         OI    LINEIDH+6,X'80'                                                  
         XC    LINEDATA,LINEDATA                                                
         OI    LINEDATH+6,X'80'                                                 
         AHI   R2,LINELEN                                                       
         CR    R2,R0                                                            
         BNH   CLRCOM2                                                          
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*============================================================                   
* ADD ORDER COMMENTS TO BUFFER                                                  
*============================================================                   
         SPACE 1                                                                
GETORD   NTR1                                                                   
K        USING DOKEY,KEY                                                        
         MVC   KEY,SVKEY           READ THE ORD AGY COMMENT REC                 
         MVI   K.DOKCMT,1                                                       
         GOTO1 HIGH                                                             
         DROP  K                                                                
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EXIT                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOCOMELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
         LH    R4,COMCOUNT                                                      
         MHI   R4,80                                                            
         A     R4,ATIA             POINT TO END OF COMMENT BUFFER               
*                                                                               
         USING DOCOMELD,R6                                                      
GETORD2  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AHI   RF,-(DOCOMOVH+1)    SET FOR EX                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),DOCOMTXT                                                 
         MVC   0(8,R4),=C'*ORDCOM*'                                             
*                                                                               
         AHI   R4,80                                                            
         BAS   RE,NEXTEL                                                        
         BE    GETORD2                                                          
*                                                                               
         S     R4,ATIA                                                          
         SRDL  R4,32                                                            
         D     R4,=F'80'                                                        
         STH   R5,COMCOUNT         SET NEW COUNT                                
         B     EXIT                                                             
         EJECT                                                                  
*============================================================                   
* GET STANDARD COMMENTS                                                         
*============================================================                   
         SPACE 1                                                                
GETSTD   NTR1                                                                   
         MVC   COMTYPE,=CL8'++ORDSTD'  SET COMMENT ID                           
         LA    R5,SVSCOM1          POINT TO FIRST STD COMMENT                   
*                                                                               
GETSTD10 BAS   RE,MVSTDCOM         MOVE STANDARD COMMENT TO BUFFER              
*                                                                               
         LA    R5,8(R5)            NEXT STANDARD COMMENT CODE                   
         LA    R0,SVSCOM3                                                       
         CR    R5,R0                                                            
         BH    GETSTDX                                                          
         OC    0(8,R5),0(R5)                                                    
         BNZ   GETSTD10                                                         
*                                                                               
GETSTDX  B     EXIT                                                             
         EJECT                                                                  
*============================================================                   
* ADD BUYER COMMENTS TO BUFFER                                                  
*============================================================                   
         SPACE 1                                                                
GETBYR   NTR1                                                                   
K        USING BYRKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D31'                                                  
         MVC   K.BYRKAM,BAGYMD                                                  
         MVC   K.BYRKBYR,SVBUYER                                                
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRCMCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
         USING BYRCMTD,R6                                                       
*                                                                               
         MVC   COMTYPE,=CL8'++BUYSTD'  SET COMMENT ID                           
*                                                                               
         LA    R5,BYRSCMNT         POINT TO BUYER STD COMMENT                   
         BAS   RE,MVSTDCOM         MOVE STANDARD COMMENT TO BUFFER              
*                                                                               
* NOW MOVE BUYER COMMENTS                                                       
*                                                                               
         LH    R4,COMCOUNT                                                      
         MHI   R4,80                                                            
         A     R4,ATIA             POINT TO END OF COMMENT BUFFER               
*                                                                               
         USING BYRCMTD,R6                                                       
*                                                                               
         CLC   BYROCMT1(8),=8C' '                                               
         BNH   GETBYR10                                                         
         MVC   0(8,R4),=C'*BYRCOM*'                                             
         MVC   8(70,R4),BYROCMT1                                                
         AHI   R4,80                                                            
*                                                                               
GETBYR10 CLC   BYROCMT2(8),=8C' '                                               
         BNH   GETBYRX                                                          
         MVC   0(8,R4),=C'*BYRCOM*'                                             
         MVC   8(70,R4),BYROCMT2                                                
         AHI   R4,80                                                            
*                                                                               
GETBYRX  S     R4,ATIA                                                          
         SRDL  R4,32                                                            
         D     R4,=F'80'                                                        
         STH   R5,COMCOUNT         SET NEW COUNT                                
         B     EXIT                                                             
*================================================================               
* SUBR READS STANDARD COMMENT RECORD AT 0(R5) INTO AIO2                         
* COMMENT DATA IS EXTRACTED INTO BUFFER                                         
*================================================================               
         SPACE 1                                                                
MVSTDCOM NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
K        USING COMKEY,KEY                                                       
*                                                                               
         MVI   K.COMKTYP,COMKTYPQ                                               
         MVI   K.COMKSUB,COMKSUBQ                                               
         MVC   K.COMKAM,BAGYMD                                                  
         MVC   K.COMKCOM,0(R5)                                                  
         OC    K.COMKCOM,=CL8' '                                                
         CLC   K.COMKCOM,=CL8' '                                                
         BNH   EXIT                                                             
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         LH    R4,COMCOUNT                                                      
         MHI   R4,80                                                            
         A     R4,ATIA             POINT TO CURRENT END OF BUFFER               
*                                                                               
         L     R6,AIO                                                           
         LA    R6,24(R6)           FIRST COMMENT ELEMENT                        
*                                                                               
MVSTD10  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AHI   RF,-4               ADJUST FOR OVERHEAD                          
         BM    MVSTD14             IN CASE REC IS BAD                           
*                                                                               
         CLI   COMTYPE,C' '        TEST STD COMMENT FLAG MOVED IN YET           
         BNH   MVSTD12                                                          
         MVC   0(8,R4),COMTYPE                                                  
         MVC   8(8,R4),=8C' '                                                   
         AHI   R4,80                                                            
         XC    COMTYPE,COMTYPE                                                  
*                                                                               
MVSTD12  EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),3(R6)       MOVE COMMENT                                 
         MVC   0(8,R4),0(R5)       MOVE COMMENT ID                              
         AHI   R4,80               NEXT OUTPUT POSN                             
*                                                                               
MVSTD14  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    MVSTDX                                                           
         CLI   0(R6),X'10'                                                      
         BE    MVSTD10                                                          
         B     MVSTD14                                                          
*                                                                               
MVSTDX   S     R4,ATIA                                                          
         SRDL  R4,32                                                            
         D     R4,=F'80'                                                        
         STH   R5,COMCOUNT         SET NEW COUNT                                
         B     EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* PFKEY TABLE PF6=TOP, PF7=SCROLL UP, PF8=SCROLL DOWN                           
*=============================================================                  
         SPACE 1                                                                
PFKEYTAB DS    0D                                                               
*                                                                               
         DC    AL1(PF06X-*,06,0,0,0,PFTRETRN)   TOP                             
         DC    CL3' ',CL8' ',CL8' '                                             
PF06X    EQU   *                                                                
*                                                                               
         DC    AL1(PF07X-*,07,0,0,0,PFTRETRN)   UP                              
         DC    CL3' ',CL8' ',CL8' '                                             
PF07X    EQU   *                                                                
*                                                                               
         DC    AL1(PF08X-*,08,0,0,0,PFTRETRN)   DOWN                            
         DC    CL3' ',CL8' ',CL8' '                                             
PF08X    EQU   *                                                                
*                                                                               
         DC    AL1(PF12X-*,12,PFTRPROG,0,0,0)   RETURN                          
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'               END OF TABLE                                 
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
BADORDER MVI   ERROR,BADORD                                                     
         B     ERREXIT                                                          
*                                                                               
NOCOMMNT MVI   ERROR,NOCOMS                                                     
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
* ERROR EQUATES                                                                 
*                                                                               
BADORD   EQU   215                                                              
NOCOMS   EQU   216                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPOMCOMD                                                       
       ++INCLUDE SPOMSDSCTS                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE SPSFMFFD          (BASE SCREEN FOR SYSTEM)                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM34D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
*                                                                               
BINORD   DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                     DATE PORTION                             
BINORDSQ DS    XL2                     SEQUENCE PORTION                         
*                                                                               
TODAYJ   DS    PL4                 TODAY'S JULIAN DATE                          
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
ACOMBUFF DS    A                   START OF COMMENT BUFFER                      
*                                                                               
SVFIRST  DS    X                   FIRST LINE NUMBER ON SCREEN                  
SVLAST   DS    X                   LAST LINE NUMBER ON SCREEN                   
COMTYPE  DS    CL8                                                              
SVLINEID DS    CL8                                                              
SVSCOM1  DS    CL8                                                              
SVSCOM2  DS    CL8                                                              
SVSCOM3  DS    CL8                                                              
SVBUYER  DS    CL3                                                              
*                                                                               
SVORDCNT DS    H                   NUMBER OF ORDER COMMENTS                     
SVBYRCNT DS    H                   NUMBER OF BUYER COMMENTS                     
SVSTDCNT DS    H                   NUMBER OF STD COMMENTS                       
COMCOUNT DS    H                   TOTAL NUMBER OF COMMENTS                     
         SPACE                                                                  
LINED    DSECT                     DSECT FOR DISPLAY LINE                       
*                                                                               
LINEIDH  DS    CL8                                                              
LINEID   DS    CL8                                                              
LINEDATH DS    CL8                                                              
LINEDATA DS    CL68                                                             
*                                                                               
LINELEN  EQU   *-LINED                                                          
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENDRORD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPSFM73   11/13/08'                                      
         END                                                                    
