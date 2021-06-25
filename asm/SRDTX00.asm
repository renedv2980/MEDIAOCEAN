*          DATA SET SRDTX00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T11600A                                                                  
         TITLE 'GENERAL FACPAK ENQUIRY SERVICE REQ'                             
         PRINT NOGEN                                                            
TERMS    CSECT                                                                  
         NMOD1 WRKX-WRKD,**$ENQ**,RR=RE                                         
         USING WRKD,RC                                                          
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     RA,SRPARM1                                                       
         USING SYSFACD,RA          RA=A(SYS FAC LIST)                           
         L     R3,SRPARM6                                                       
         USING SRDTXFFD,R3         R3=A(TWA)                                    
         L     R4,SRPARM4                                                       
         USING COMFACSD,R4         GET A (COMMON FACILITIES)                    
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ACALLOV,CCALLOV                                                  
         DROP  R4                                                               
         ST    RE,RELO                                                          
         XC    MSG,MSG             CLEAR ERROR MSG AREA                         
         EJECT                                                                  
ENQUIRE  LA    R4,SRVP1H           R4=A(SCR FLD HDR)                            
         ST    R4,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,SRVP1H+5                                                    
         BZ    INF0                ENTER ENQUIRY                                
         LA    RF,ENQTAB                                                        
         BCTR  R1,0                                                             
         CLI   SRVP1,C'?'          TEST FOR HELP                                
         BE    ENQ020                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),SRVP1       MATCH ENQUIRY                                
         BE    ENQ010                                                           
         LA    RF,16(RF)                                                        
         CLI   0(RF),0                                                          
         BE    ERR1                INVALID INPUT                                
         SPACE 1                                                                
ENQ010   L     RF,8(RF)            GET A(ROUTINE)                               
         A     RF,RELO                                                          
         BR    RF                                                               
         EJECT                                                                  
ENQ020   MVC   SRVP5,HLPHDR        SET HEADER                                   
         OI    SRVP5H+6,X'80'                                                   
         LA    RF,ENQTAB           HELP FOR ENQUIRYS                            
         LA    R4,SRVLINEH                                                      
ENQ022   MVC   8(76,R4),SPACES     CLEAR LINE                                   
         MVC   8(8,R4),0(RF)       ENQUIRY KEYWORD                              
         L     R1,12(RF)                                                        
         A     R1,RELO                                                          
         MVC   18(50,R4),0(R1)     KEYWORD HELP TEXT CL50                       
         OI    6(R4),X'80'         XMIT                                         
         LA    R4,93(R4)                                                        
         LA    R1,SRVXLINH         TEST FOR SCREEN FULL                         
         CR    R4,R1                                                            
         BH    EXIT                WHOEVER FILL IT WRITES ?,NEXT CODE           
         LA    RF,16(RF)                                                        
         CLI   0(RF),0                                                          
         BNE   ENQ022                                                           
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************                       
* DISPLAY MESSAGE BUFFER,  MESSAGES OR STATS            *                       
*********************************************************                       
         SPACE 1                                                                
MSGBUFF  GOTO1 ACALLOV,DMCB,0,X'D9000AA0'                                       
         L     R1,0(R1)            GET A(MSGBUFF IN R1)                         
         ST    R1,AMSGBUFF                                                      
         MVC   SRVL2,=CL16'Messages/Stats'                                      
         OI    SRVL2H+6,X'80'                                                   
         MVC   SRVL3,=CL16'Buffer pos''n'                                       
         OI    SRVL3H+6,X'80'                                                   
         LA    R1,SRVP2H                                                        
         ST    R1,CURSOR                                                        
         CLI   5(R1),0                                                          
         BE    MSGMSG                                                           
         CLI   SRVP2,C'M'          MESSAGES                                     
         BE    MSGMSG                                                           
         CLI   SRVP2,C'S'          STATS                                        
         BE    MSGSTAT                                                          
         B     ERR1                INVALID INPUT                                
         EJECT                                                                  
*********************************************************                       
* DISPLAY MESSAGE BUFFER,  MESSAGES                     *                       
*********************************************************                       
         SPACE 1                                                                
MSGMSG   MVC   SRVP5,MSGHDR        SET HEADER                                   
         OI    SRVP5H+6,X'80'                                                   
         LA    R1,SRVP3H                                                        
         ST    R1,CURSOR                                                        
         XC    FULL,FULL                                                        
         CLI   5(R1),0             TEST FOR ANY INPUT                           
         BE    *+12                                                             
         BAS   RE,VALNUM                                                        
         BNE   ERR2                FIELD NOT NUMERIC                            
         LA    R4,SRVLINEH                                                      
         USING MSGLINE,R4                                                       
         L     R5,AMSGBUFF         POINT R5 TO MESSAGE BUFFER                   
         USING MSGBUFFD,R5                                                      
         LH    R6,MSGBNUM          R6 = NUMBER OF MESSAGES                      
         LA    R1,MSGBUFFS                                                      
         ST    R1,ABUFFER                                                       
         LA    R5,MSGBNDX          R5 = A(INDEX)                                
         L     R1,FULL                                                          
         SR    R6,R1                                                            
         LTR   R6,R6                                                            
         BZ    ERR3                NOTHING TO LIST                              
         SLL   R1,3                8*FULL IF NUMBER ENTERED                     
         AR    R5,R1                                                            
         DROP  R5                                                               
MSG000   L     R1,VSSB             GET LANGUAGE TABLE                           
         L     R1,SSBALANG-SSBD(R1)                                             
         LA    R1,6(R1)                                                         
MSG010   CLC   0(1,R1),2(R5)                                                    
         BE    MSG020                                                           
         LA    R1,40(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BNE   MSG010                                                           
         B     ERR4                INVALID KEY                                  
*                                                                               
MSG020   MVC   LINLANG,8(R1)       INSERT LANGUAGE                              
*                                                                               
         EDIT  (B1,5(R5)),(3,LINFREQ),FILL=0                                    
*                                                                               
         MVC   LINID(1),1(R5)      BUILD ??/0000                                
         SR    RF,RF                                                            
         IC    RF,0(R5)                                                         
         LA    RF,SYSTAB(RF)                                                    
         MVC   LINID+1(1),0(RF)                                                 
         MVI   LINID+2,C'/'                                                     
         EDIT  (B2,3(R5)),(4,LINID+3),FILL=0                                    
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,6(R5)                                                       
         A     RF,ABUFFER                                                       
         SR    R1,R1                                                            
         IC    R1,1(RF)                                                         
         SH    R1,=H'3'                                                         
         CH    R1,=H'58'           MAX LEN IS 59                                
         BNH   *+8                                                              
         LH    R1,=H'58'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINMSG(0),3(RF)                                                  
*                                                                               
         OI    LINHDR+6,X'80'                                                   
         LA    R4,93(R4)                                                        
         LA    R1,SRVXLINH         TEST FOR SCREEN FULL                         
         CR    R4,R1                                                            
         BH    MSG050                                                           
         LA    R5,8(R5)            NEXT INDEX ENTRY                             
         BCT   R6,MSG000                                                        
         B     EXIT                                                             
MSG050   LA    R1,19               NEXT SCREEN                                  
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(3,SRVP3),ALIGN=LEFT                                   
         OI    SRVP3H+6,X'80'                                                   
         LA    R1,SRVSELH                                                       
         ST    R1,CURSOR                                                        
         B     INF1                                                             
         EJECT                                                                  
*********************************************************                       
* DISPLAY MESSAGE BUFFER,  STATS                        *                       
*********************************************************                       
         SPACE 1                                                                
MSGSTAT  MVC   SRVL3,=CL16'Chng stat +/-'                                       
         LA    R4,SRVLINEH                                                      
         ST    R4,CURSOR                                                        
         USING MSGSTATD,R4                                                      
         L     R5,AMSGBUFF         POINT R5 TO MESSAGE BUFFER                   
         USING MSGBUFFD,R5                                                      
         LA    R1,SRVP3H                                                        
         ST    R1,CURSOR                                                        
         CLI   SRVP3H+5,0                                                       
         BE    STAT005                                                          
         CLI   SRVP3H+5,1                                                       
         BH    ERR1                                                             
         CLI   SRVP3,C'-'          DISABLED                                     
         BE    STAT004                                                          
         CLI   SRVP3,C'+'          ENABLED                                      
         BNE   ERR1                                                             
STAT004  MVC   MSGBSTAT,SRVP3                                                   
STAT005  OI    STAHDR+6,X'80'                                                   
         MVC   STATL1,=CL15'Buffer address'                                     
         MVC   STATL2,=CL15'#GETTXT calls'                                      
         MVC   STATL3,=CL15'Buffer status'                                      
         LA    RF,AMSGBUFF                                                      
         GOTO1 AHEXOUT,DMCB,(RF),STATV1,4,=C'TOG'                               
         EDIT  (B4,MSGBTOTL),(8,STATV2),ALIGN=LEFT                              
*                                                                               
         MVC   STATV3(1),MSGBSTAT                                               
*                                                                               
STAT010  LA    R4,93(R4)                                                        
         OI    STAHDR+6,X'80'                                                   
         MVC   STATL1,=CL15'Buffer size'                                        
         MVC   STATL2,=CL15'#Buffer reads'                                      
         MVC   STATL3,=CL15'Cut off'                                            
         L     R1,=A(MSGBUFFX-MSGBUFFS)                                         
         EDIT  (R1),(8,STATV1),ALIGN=LEFT                                       
         EDIT  (B4,MSGBCORE),(8,STATV2),ALIGN=LEFT                              
         EDIT  (B1,MSGBCUT),(3,STATV3),ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
STAT020  LA    R4,93(R4)                                                        
         OI    STAHDR+6,X'80'                                                   
         MVC   STATL1,=CL15'Available'                                          
*        MVC   STATL2,=cl15'         '                                          
         L     R1,MSGBEND                                                       
         S     R1,MSGBNXT                                                       
         EDIT  (R1),(8,STATV1),ALIGN=LEFT                                       
         SR    RE,RE                                                            
         L     RF,MSGBCORE                                                      
         MH    RF,=H'1000'                                                      
         L     R1,MSGBTOTL                                                      
         DR    RE,R1                                                            
STAT021  EDIT  (RF),(5,STATV2),1,TRAIL=C'%',ALIGN=LEFT,ZERO=NOBLANK             
*                                                                               
STAT030  LA    R4,93(R4)           BLANK LINE                                   
         LA    R4,93(R4)                                                        
         OI    STAHDR+6,X'80'                                                   
         MVC   STATL1,=CL15'Index entries'                                      
         EDIT  (B2,MSGBNUM),(4,STATV1),ALIGN=LEFT                               
STAT040  LA    R4,93(R4)                                                        
         OI    STAHDR+6,X'80'                                                   
         MVC   STATL1,=CL15'Index maximum'                                      
         EDIT  (B2,MSGBMAX),(4,STATV1),ALIGN=LEFT                               
         B     INF3                                                             
         B     EXIT                                                             
         EJECT                                                                  
VALNUM   NTR1                                                                   
         LR    R4,R1                                                            
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         LR    RF,R1                                                            
         LA    RE,8(R4)                                                         
VALN1    CLI   0(RE),X'F0'         TEST NUMERIC                                 
         BL    VALNERR                                                          
         CLI   0(RE),X'F9'                                                      
         BH    VALNERR                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,VALN1                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)         PACK INTO DUB                                
         CVB   R1,DUB                                                           
         ST    R1,FULL             BINARY INTO FULL                             
         CR    RB,RB                                                            
         B     VALNX                                                            
VALNERR  LTR   RB,RB               SET CC NEQ FOR ERR                           
VALNX    XIT1                                                                   
         EJECT                                                                  
INF0     MVC   MSG(13),=C'Enter enquiry'                                        
         B     INFX                                                             
INF1     MVC   MSG(36),=C'Buffer displayed. Hit ENTER for next'                 
         B     INFX                                                             
INF3     MVC   MSG(15),=C'Stats displayed'                                      
         B     INFX                                                             
ERR0     MVC   MSG(13),=C'Missing input field'                                  
         B     ERRX                                                             
ERR1     MVC   MSG(13),=C'Invalid input field'                                  
         B     ERRX                                                             
ERR2     MVC   MSG(17),=C'Field not numeric'                                    
         B     ERRX                                                             
ERR3     MVC   MSG(15),=C'Nothing to list'                                      
         B     ERRX                                                             
ERR4     MVC   MSG(16),=C'Invalid key data'                                     
         B     ERRX                                                             
         SPACE 2                                                                
INFX     MVC   SRVMSG(48),MSG                                                   
         FOUT  SRVMSGH                                                          
         L     R1,CURSOR           SET CURSOR                                   
         OI    6(R1),X'40'                                                      
         B     EXIT                                                             
         SPACE 2                                                                
ERRX     MVC   SRVMSG(12),=C'***ERROR*** '                                      
         MVC   SRVMSG+12(48),MSG                                                
         FOUT  SRVMSGH                                                          
         L     R1,CURSOR           SET CURSOR                                   
         OI    6(R1),X'40'                                                      
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
         DS    0F                                                               
ENQTAB   DC    CL8'MSGBUFF ',AL4(MSGBUFF),AL4(HELP1)                            
         DC    X'00'                                                            
         EJECT                                                                  
HELP1    DC    CL50'Display core message buffer data'                           
         EJECT                                                                  
SPACES   DC    CL80' '                                                          
       ++INCLUDE FASYSLST                                                       
SYSTAB   DS    0CL1                ALSO IN FAGETTXT                             
*&&UK*&& DC    C'GD23MLAF8BCZ??I?'                                              
*&&US*&& DC    C'GDSNPLATRBCZWENSP????LLSNP??????'                              
MSGHDR   DC    CL78'S-Frq-Lng-Ref#     ----------------Message data----X        
               ---------------------------'                                     
HLPHDR   DC    CL78'S-Keyword --------------Description----------------X        
               ---------'                                                       
         LTORG                                                                  
         SPACE 2                                                                
         EJECT                                                                  
         SPACE 2                                                                
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
WORK     DS    CL32                                                             
DMCB     DS    6F                                                               
RELO     DS    A                                                                
AHEXIN   DS    A                                                                
AHEXOUT  DS    A                                                                
ACALLOV  DS    A                                                                
AMSGBUFF DS    A                                                                
ABUFFER  DS    A                                                                
CURSOR   DS    A                                                                
MSG      DS    CL60                                                             
WRKX     DS    0C                                                               
         EJECT                                                                  
MSGLINE  DSECT                                                                  
LINHDR   DS    CL8   HEADER                                                     
LINDATA  DS    0CL76                                                            
LINFREQ  DS    CL3   SYS                                                        
         DS    CL1                                                              
LINLANG  DS    CL3   LANG                                                       
         DS    CL1                                                              
LINID    DS    CL7   ??/0000                                                    
         DS    CL2                                                              
LINMSG   DS    CL59  MESSAGE                                                    
         SPACE 2                                                                
MSGSTATD DSECT                                                                  
STAHDR   DS    CL8                                                              
STADATA  DS    0CL76                                                            
STATL1   DS    CL15                LABEL 1                                      
         DS    CL1                                                              
STATV1   DS    CL8                 VALUE 1                                      
         DS    CL2                                                              
STATL2   DS    CL15                LABEL 2                                      
         DS    CL1                                                              
STATV2   DS    CL8                 VALUE 2                                      
         DS    CL2                                                              
STATL3   DS    CL15                LABEL 3                                      
         DS    CL1                                                              
STATV3   DS    CL8                 VALUE 3                                      
         DS    CL2                                                              
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* FAMSGBUFFD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAMSGBUFFD                                                     
         PRINT ON                                                               
         EJECT                                                                  
SRDTXFFD DSECT                                                                  
         DS    CL64                                                             
* SRDTXFFD                                                                      
       ++INCLUDE SRDTXFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRDTX00   05/01/02'                                      
         END                                                                    
