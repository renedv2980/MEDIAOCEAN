*          DATA SET SPMELRCV   AT LEVEL 010 AS OF 11/29/84                      
*PHASE SPRCVX8,*                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'SPRCVX1 - EXTRACT RECORDS FROM SPOT1 RECOVERY TAPE'             
SPRCVX1  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SPRCVX1,=V(REGSAVE)                                            
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         OPEN  (IN,(INPUT))                                                     
*                                                                               
CHKIN    L     R1,=A(IN)                                                        
         LA    R0,REC-4                                                         
         GET   (1),(0)                                                          
*                                                                               
         CLI   REC,5               TEST REQUEST FILE                            
         BNE   CHKIN                                                            
*                                                                               
         CLC   REC+24+26(5),=C'30FCN'                                           
         BE    PRNT                                                             
         B     CHKIN                                                            
*                                                                               
         LA    R4,KEYLIST                                                       
         LA    R5,KEYLISTX                                                      
*                                                                               
CHKCLC   CLC   REC+24(13),0(R4)                                                 
         BE    PRNT                                                             
         LA    R4,17(R4)                                                        
         CR    R4,R5                                                            
         BL    CHKCLC                                                           
         B     CHKIN                                                            
         SPACE 2                                                                
PRNT     L     R4,=A(REC)                                                       
         GOTO1 =V(PRNTBL),P1,(16,RCVTITLE),(R4),C'DUMP',24,=C'2D'               
         LA    R4,24(R4)                                                        
         MVC   DUB(2),13(R4)                                                    
         LH    R0,DUB                                                           
         GOTO1 =V(PRNTBL),P1,(16,RCVDATA),(R4),C'DUMP',(R0),=C'2D'              
*                                                                               
         GOTO1 =V(PRINTER)         SKIP 2 LINES                                 
         GOTO1 =V(PRINTER)                                                      
         B     CHKIN                                                            
*                                                                               
* MOVE THIS COPY OF RECORD TO OUTPUT SAVE AREA *                                
*                                                                               
         LH    RF,REC-4            GET LENGTH TO SAVE                           
*                                                                               
         LA    R1,REC-4            GET START ADDRESS                            
*                                                                               
         ICM   RE,15,13(R4)        GET SAVE AREA ADDRESS                        
*                                                                               
         LA    R0,256                                                           
*                                                                               
SAVE2    CR    RF,R0                                                            
         BNH   SAVE4                                                            
         MVC   0(256,RE),0(RF)                                                  
         AR    RE,R0                                                            
         SR    RF,R0                                                            
         B     SAVE2                                                            
*                                                                               
SAVE4    BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         B     CHKIN                                                            
*                                                                               
CHKEOF   DC    0H'0'                                                            
         CLOSE (IN)                                                             
         B     EXIT                                                             
         SPACE 1                                                                
* NOW WRITE SAVED RECORDS TO OUTPUT FILE *                                      
         SPACE 1                                                                
         OPEN  (OUT,(OUTPUT))                                                   
*                                                                               
         LA    R4,KEYLIST                                                       
         LA    R5,KEYLISTX                                                      
*                                                                               
PUT      ICM   R6,15,13(R4)                                                     
         OC    0(4,R6),0(R6)                                                    
         BZ    PUT2                                                             
*                                                                               
         LH    R0,0(R6)            GET LEN (INCLUDING RCVHDR)                   
         SH    R0,=H'24'                                                        
         SLL   R0,16                                                            
         ST    R0,20(R6)                                                        
         LR    R0,R6                                                            
         PUT   OUT,(R0)                                                         
*                                                                               
PUT2     LA    R4,17(R4)                                                        
         CR    R4,R5                                                            
         BL    PUT                                                              
         CLOSE (OUT)                                                            
EXIT     XBASE                                                                  
         EJECT                                                                  
KEYLIST  DS    0D                                                               
*                                                                               
FC       DC    X'11855FFF00E7BBC3920D1E0100',AL4(0)                             
*        DC    X'11BE7FFF02DB5D931240190100',AL4(SV2)                           
*        DC    X'11BE7FFF02DB5D9312401E0100',AL4(SV3)                           
*                                                                               
*TB      DC    X'31A27FFF06EBBC988282050100',AL4(SV4)                           
*        DC    X'31D97FFF019BBD82827C010100',AL4(SV5)                           
*                                                                               
*CM      DC    X'41D8BFFF06EBBDC5423C010100',AL4(SV6)                           
*                                                                               
*MC      DC    X'6184CCFF0529BB69620E010100',AL4(SV7)                           
*        DC    X'6184CCFF0529BB69620E020100',AL4(SV8)                           
*        DC    X'6184CCFF0529BB69620E030100',AL4(SV9)                           
*        DC    X'6184CCFF0529BB69620E070100',AL4(SVA)                           
KEYLISTX EQU   *-1                                                              
*                                                                               
RCVTITLE DC    CL16'RECOVERY HEADER'                                            
RCVDATA  DC    CL16'RECOVERY DATA  '                                            
         EJECT                                                                  
DMCB     DC    6A(0)                                                            
*                                                                               
GETWORK  DS    12D                                                              
*                                                                               
P1       DC    F'0'                                                             
P2       DC    F'0'                                                             
P3       DC    F'0'                                                             
P4       DC    F'0'                                                             
P5       DC    F'0'                                                             
P6       DC    F'0'                                                             
*                                                                               
DUB      DS    D                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    D                                                                
REC      DS    2100C                                                            
*                                                                               
IN       DCB   DDNAME=IN,DSORG=PS,RECFM=VB,LRECL=2024,BLKSIZE=2028,    X        
               MACRF=GM,EODAD=CHKEOF                                            
         SPACE 2                                                                
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=VB,LRECL=2024,BLKSIZE=2028,   X        
               MACRF=PM                                                         
         EJECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPMELRCV  11/29/84'                                      
         END                                                                    
