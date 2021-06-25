*          DATA SET HUTLOAD    AT LEVEL 059 AS OF 05/01/02                      
*PHASE HUTLD50,*,NOAUTO                                                         
*INCLUDE IJFVZZWZ                                                               
*INCLUDE DMISLDDS                                                               
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE IJDFYZZZ                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE LOGIO                                                                  
         TITLE 'HUTLOAD - SORT + EDIT + LOAD SVI FILE'                          
SORTLOAD DDSRT1 HUTLOAD,40                                                      
HUTLOAD  CSECT                                                                  
         NMOD1 0,**HUTSORT                                                      
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         LM    R2,R4,0(R1)                                                      
         LA    R2,BRANCH(R2)                                                    
         BR    R2                                                               
*                                                                               
BRANCH   B     INITIAL                                                          
         B     INPUT                                                            
         B     OUTPUT                                                           
         B     FINAL                                                            
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
INITIAL  MVC   0(36,R3),=C'SORT FIELDS=(5,9,A),FORMAT=BI,WORK=1'                
         MVC   0(24,R4),=C'RECORD TYPE=V,LENGTH=200'                            
         OPEN  IN,OUT                                                           
         L     R1,=A(HUTFL)                                                     
         LA    R2,HUTFLX-HUTFL(R1)                                              
         OPEN  (1)                                                              
         XC    ERRMSG,ERRMSG                                                    
         MVC   TITLE(19),=C'SVIFLIE LOAD REPORT'                                
         MVC   MID1+38(26),=C'A U D I E N C E    T Y P E'                       
         MVC   MID2(90),=C'SER/SRC    RECORDS    01      02      03    X        
                 04      05      06      07      08   MARKETS'                  
         MVC   MID3(90),=C'-------    -------  ------  ------  ------  X        
               ------  ------  ------  ------  ------ -------'                  
         B     EXIT                                                             
         EJECT                                                                  
INPUT    DS    0H                                                               
         ST    R1,SAVER1                                                        
IN2      CLI   REC,X'FF'                                                        
         BNE   GET                                                              
         L     R1,SAVER1                                                        
         MVC   0(4,R1),=F'8'     END OF INPUT                                   
         B     EXIT                                                             
*                                                                               
GET      XC    REC-4(250),REC-4                                                 
         GET   IN,REC-4                                                         
         AP    INCNT,=P'1'                                                      
*                                                                               
GETX     L     R1,SAVER1                                                        
         LA    R2,REC-4                                                         
         ST    R2,4(R1)                                                         
         B     EXIT                                                             
         EJECT                                                                  
OUTPUT   ST    R1,SAVER1                                                        
         LA    R6,REC-4                                                         
         MVC   0(200,R6),0(R3)                                                  
         LA    R6,REC-4                                                         
         LH    R7,REC-4                                                         
         AR    R6,R7                                                            
         XC    0(2,R6),0(R6)                                                    
         CLC   REC(7),LASTMD                                                    
         BE    SAMEDAY                                                          
         XC    LASTQH,LASTQH                                                    
         CLC   REC(4),LASTMD                                                    
         BE    *+8                                                              
         BAS   R9,TOTALS                                                        
         CLI   LASTMD,0                                                         
         BE    *+8                                                              
         BAS   R9,OUTX                                                          
         CLC   REC+7(1),REC+8                                                   
         BH    INVQH                                                            
         B     CKSVI                                                            
*                                                                               
SAMEDAY  CLC   REC(9),LASTMD                                                    
         BE    DUPKEY        DUPLICATE KEY                                      
         CLI   LASTMD,0                                                         
         BE    *+8                                                              
         BAS   R9,OUTX                                                          
         CLC   REC+7(1),REC+8                                                   
         BH    INVQH                                                            
         CLC   REC+7(1),LASTQH                                                  
         BE    CHGQH        LAST END QH SAME ASS THIS START QH                  
         BL    QHERR           LAST QH END LATER THAN THIS START                
         B     CKSVI                                                            
*                                                                               
*                                                                               
DUPKEY   MVC   ERRMSG(38),=C'DUPLICATE KEY - LOAD ABORTED'                      
         BAS   R9,PNTREC                                                        
         B     ERRXIT                                                           
*                                                                               
*                                                                               
CHGQH    SR    R5,R5                                                            
         IC    R5,REC+7                                                         
         AH    R5,=H'1'       ADVANCE START QH ONE QH                           
         STC   R5,REC+7                                                         
         MVC   ERRMSG(34),=C'START QH ADVANCED TO AVOID OVERLAP'                
         BAS   R9,PNTREC                                                        
         B     ERRXIT                                                           
*                                                                               
INVQH    MVC   ERRMSG(28),=C'START QH     PRECEEDS END QH'                      
         SR    R5,R5                                                            
         IC    R5,REC+7                                                         
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRMSG+9(3),DUB+6(2)                                             
         IC    R5,REC+8                                                         
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRMSG+29(3),DUB+6(2)                                            
         BAS   R9,PNTREC                                                        
         B     ERRXIT                                                           
*                                                                               
*                                                                               
QHERR    MVC   ERRMSG(37),=C'LAST END QH     EXCEEDS THIS START QH'             
         SR    R5,R5                                                            
         IC    R5,LASTQH                                                        
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRMSG+12(3),DUB+6(2)                                            
         BAS   R9,PNTREC                                                        
         B     ERRXIT                                                           
*                                                                               
*                                                                               
*                                                                               
CKSVI    LA    R4,REC+11                                                        
         MVI   LASTAUD,0                                                        
CKAUD1   CLC   2(1,R4),LASTAUD                                                  
         BE    AUDERR                                                           
         MVC   LASTAUD,2(R4)                                                    
         B     NXTEL                                                            
*                                                                               
AUDERR   MVC   ERRMSG(36),=C'DUPLICATE SETS OF SVIS EXIST FOR AUD'              
         SR    R5,R5                                                            
         IC    R5,LASTAUD                                                       
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRMSG+37(3),DUB+6(2)                                            
         BAS   R9,PNTREC                                                        
         B     ERRXIT                                                           
*                                                                               
*                                                                               
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   CKAUD1                                                           
         B     COUNT                                                            
*                                                                               
COUNT    CLC   REC+4(2),LASTMKT                                                 
         BE    *+10                                                             
         AP    MKTCNT,=P'1'                                                     
         AP    SRCCNT,=P'1'                                                     
         LA    R4,REC+11                                                        
COUNT2   CLI   0(R4),X'02'                                                      
         BNE   NXT                                                              
         CLI   2(R4),08                                                         
         BH    NXT                                                              
         SR    R6,R6                                                            
         IC    R6,2(R4)                                                         
         SLL   R6,2                MULTIPLY BY 4                                
         LA    R6,SVI01-4(R6)                                                   
         AP    0(4,R6),=P'1'                                                    
         B     NXT                                                              
*                                                                               
NXT      SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   COUNT2                                                           
         B     OUTXIT                                                           
*                                                                               
*                                                                               
*                                                                               
PNTREC   MVC   P+1(1),REC+2                                                     
         MVC   P+5(1),REC+3                                                     
         SR    R0,R0                                                            
         IC    R0,REC+4                                                         
         SLL   R0,8                                                             
         IC    R0,REC+5                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+10(4),DUB+5(3)                                                 
         MVC   P+16(3),=C'M-F'                                                  
         CLI   REC+6,X'15'                                                      
         BE    PNT1                                                             
         MVC   P+16(3),=C'SAT'                                                  
         CLI   REC+6,X'66'                                                      
         BE    PNT1                                                             
         MVC   P+16(3),=C'SUN'                                                  
PNT1     SR    R0,R0                                                            
         IC    R0,REC+7                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+22(3),DUB+6(2)                                                 
         IC    R0,REC+8                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+27(3),DUB+6(2)                                                 
         MVC   P+38(60),ERRMSG                                                  
         GOTO1 =V(PRINTER)                                                      
         AP    ERRCNT,=P'1'                                                     
         XC    ERRMSG,ERRMSG                                                    
         BR    R9         RETURN                                                
*                                                                               
*                                                                               
*                                                                               
PNTSVIS  LA    R6,12                                                            
         LA    R7,MONTHS                                                        
         LA    R5,P+30                                                          
PNTS1    SR    R0,R0                                                            
         IC    R0,0(R3)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R5),DUB+6(2)                                                 
         MVC   0(3,R5),0(R7)                                                    
         LA    R5,8(R5)                                                         
         LA    R7,3(R7)                                                         
         LA    R3,1(R3)                                                         
         BCT   R6,PNTS1                                                         
         MVC   P+14(12),=C'DELETED SET-'                                        
         GOTO1 =V(PRINTER)                                                      
         BR    R9                                                               
*                                                                               
*                                                                               
TOTALS   MVC   P+1(1),LASTMD+2                                                  
         MVC   P+5(1),LASTMD+3                                                  
         LA    R3,10                                                            
         LA    R4,P+12                                                          
         LA    R5,SRCCNT                                                        
TOT1     EDIT  (P4,(R5)),(6,(R4)),0                                             
         LA    R5,4(R5)                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,TOT1                                                          
         GOTO1 =V(PRINTER)                                                      
         LA    R3,10                                                            
         LA    R5,SRCCNT                                                        
TOT2     ZAP   0(4,R5),=P'0'                                                    
         LA    R5,4(R5)                                                         
         BCT   R3,TOT2                                                          
         BR    R9                                                               
*                                                                               
*                                                                               
*                                                                               
OUTX     PUT   OUT,LASTMD-4                                                     
         MVC   CHAR,LASTMD+8       INTERCHANGE START AND END TIMES              
         MVC   LASTMD+8(1),LASTMD+7                                             
         MVC   LASTMD+7(1),CHAR                                                 
         CLI   LOADSW,1                                                         
         BNE   OUTXA                                                            
         GOTO1 =V(ISLDDS),PLIST,0,LASTMD,0,A(HUTFL)                             
OUTXA    AP    OUTCNT,=P'1'                                                     
         XC   LASTMD-4(254),LASTMD-4                                            
         BR    R9        RETURN                                                 
OUTXIT   MVC   LASTMD-4(254),REC-4                                              
         MVC   LASTQH,REC+8                                                     
         MVC   LASTMKT,REC+4                                                    
ERRXIT   L     R1,SAVER1                                                        
         B     EXIT                                                             
*                                                                               
*                                                                               
ELEMENT  DC    XL23'00'                                                         
*                                                                               
*                                                                               
         DS    D                                                                
LASTMD   DC    XL250'00'                                                        
LASTQH   DC    XL2'00'                                                          
LASTMKT  DC    XL2'00'                                                          
SAVER1   DS    F                                                                
DUB      DS    D                                                                
LASTAUD  DS    CL1          AUD TYPE OF LAST ELEMENT                            
NEXTQH   DS    CL1                                                              
AUDLIST  DC    XL10'00'                                                         
ERROR    DC    X'00'                                                            
ERRMSG   DS    CL60                                                             
HALF     DS    H                                                                
WORK     DS    CL20                                                             
PLIST    DS    6F                                                               
CHAR     DS    C                                                                
*                                                                               
*                                                                               
*                                                                               
BLDXTNT  MVC   0(14,R2),0(R1)                                                   
         LA    R2,14(R2)                                                        
         MVI   0(R2),X'FF'                                                      
         LBRET 2                                                                
         EJECT                                                                  
FINAL    CLI   LASTMD,0                                                         
         BE    *+12                                                             
         BAS   R9,TOTALS                                                        
         BAS   R9,OUTX             OUTPUT LAST REC                              
         ZAP   LINE,=P'99'         FORCE NEW PAGE                               
         MVC   MID1(43),=CL43'TOTAL RECORD COUNTS'                              
         MVC   MID1+43(50),SPACES                                               
         MVC   MID2(90),SPACES                                                  
         MVC   MID3(90),SPACES                                                  
         LA    R5,TITLES                                                        
         LA    R2,CNTRS                                                         
         LA    R3,3                                                             
END1     OI    3(R2),X'0F'                                                      
         UNPK  P+13(7),0(4,R2)                                                  
         MVC   P(12),0(R5)                                                      
         GOTO1 =V(PRINTER)                                                      
         LA    R2,4(R2)                                                         
         LA    R5,12(R5)                                                        
         BCT   R3,END1                                                          
         CLI   ERROR,1                                                          
         BNE   END2                                                             
         MVC   P(23),=C'FATAL ERROR ENCOUNTERED'                                
         GOTO1 =V(PRINTER)                                                      
END2     CLOSE OUT                                                              
         CLI   LOADSW,1                                                         
         BNE   ENDJOB                                                           
*                                                                               
         MVI   REC,X'FF'                                                        
         GOTO1 =V(ISLDDS),PLIST,0,REC,0,A(HUTFL)                                
         EOJ                                                                    
ENDJOB   EOJ                                                                    
*                                                                               
END      CLOSE IN                                                               
         GOTO1 =V(LOGIO),DUB,1,=C'ENTER Y FOR ANOTHER REEL'                     
         GOTO1 =V(LOGIO),DUB,0,(1,HALF)                                         
         CLI   HALF,C'Y'                                                        
         BE    MOREIN                                                           
         CLI   HALF,X'A8'                                                       
         BE    MOREIN                                                           
         MVI   REC,X'FF'                                                        
         B     IN2                                                              
MOREIN   OPEN  IN                                                               
         B     GET                                                              
         LTORG                                                                  
         EJECT                                                                  
TITLES   DC    CL12'RECORDS IN'                                                 
         DC    CL12'RECORDS OUT'                                                
         DC    CL12'ERRORS'                                                     
LOADSW   DC    X'01'                                                            
*                                                                               
CNTRS    DS    0D                                                               
INCNT    DC    PL4'0'                                                           
OUTCNT   DC    PL4'0'                                                           
ERRCNT   DC    PL4'0'                                                           
*                                                                               
SRCCNT   DC    PL4'0'                                                           
SVI01    DC    PL4'0'                                                           
SVI02    DC    PL4'0'                                                           
SVI03    DC    PL4'0'                                                           
SVI04    DC    PL4'0'                                                           
SVI05    DC    PL4'0'                                                           
SVI06    DC    PL4'0'                                                           
SVI07    DC    PL4'0'                                                           
SVI08    DC    PL4'0'                                                           
MKTCNT   DC    PL4'0'                                                           
*                                                                               
*                                                                               
*                                                                               
         DS    D                                                                
REC      DS    250C                                                             
*                                                                               
IN1      DS    8000C                                                            
OUT1     DS    8000C                                                            
         EJECT                                                                  
IN       DTFMT DEVADDR=SYS004,BLKSIZE=8000,RECFORM=VARBLK,             X        
               TYPEFLE=INPUT,IOAREA1=IN1,WORKA=YES,FILABL=STD,         X        
               EOFADDR=END,REWIND=UNLOAD                                        
         EJECT                                                                  
OUT      DTFMT DEVADDR=SYS005,BLKSIZE=8000,RECFORM=VARBLK,             X        
               TYPEFLE=OUTPUT,IOAREA1=OUT1,WORKA=YES,FILABL=STD,       X        
               REWIND=UNLOAD                                                    
         EJECT                                                                  
HUTFL    DMIS  KEYLEN=09,RECSIZE=V,BLKSIZE=3024,SPARE=0,INDSIZE=500,   X        
               ADD=NO,DEVICE=3350                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059HUTLOAD   05/01/02'                                      
         END                                                                    
