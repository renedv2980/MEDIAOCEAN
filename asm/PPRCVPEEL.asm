*          DATA SET PPRCVPEEL  AT LEVEL 051 AS OF 05/01/02                      
*PHASE PRCVPEEA PRCVPEEL                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE MSUNPK                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PDUMPER                                                                
         TITLE 'PPRCVPEEL - PEEL/SORT/SAVE PRINT RECOVERY DATA'                 
PRCVPEEL CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,PRCVPEEL,VREGSAVE                                              
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING PRCVPEEL+4096,RC                                                 
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT2    DS    0H                                                               
         LA    RE,PRCVPEEL         SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
*                                                                               
****     GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY PEEL PROGRAM'                           
         B     IN2                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,16,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2100'                                  
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
**NEW 1/30/90                                                                   
**       NEW FILES    WAS      NOW                                              
*        PRTDIR       X'10'    X'40'                                            
*        PUBDIR       X'11'    X'41'                                            
*        PRTFILE      X'12'    X'42'                                            
*        PUBFILE      X'13'    X'43'                                            
*        PRECOVE      X'14'    X'44'                                            
*        PREQUES      X'15'    X'45'                                            
**NEW 1/30/90         WAS X'12'                                                 
         CLI   RFILTY,X'42'        TEST PRINTFILE                               
         BNE   IN2                                                              
         CLC   RECVR(07),=X'C6E3D508D4E940C4C140'                               
         BE    IN5                                                              
         CLC   RECVR(07),=X'D9D9D408D6C5C4D6C5C4'                               
         BE    IN5                                                              
         CLC   RECVR(07),=X'D9D9D508C2C340D4E240'                               
         BNE   IN2                                                              
IN5      BAS   RE,DMPREC                                                        
         B     IN2                                                              
*                                                                               
*****    CLC   RECVR(07),=X'E6C5D520E3E740D3F0F3'                               
*****    BE    IN5                                                              
*****    CLC   RECVR(07),=X'E6C5D508E3E740D3F0F3'                               
*****    BNE   IN2                                                              
         SPACE 3                                                                
DMPREC   NTR1                                                                   
*                                  R5 POINTS TO REC                             
         LA    R5,RECVHDR-4                                                     
         LH    R2,0(R5)            LENGTH                                       
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 =V(HEXOUT),DMCB,(R5),WORK,(R4),=C'N'                             
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         GOTO1 =V(PRINTER)                                                      
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
*                                                                               
EXIT     DS    0H                 SKIP A LINE                                   
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         SPACE 3                                                                
ENDIN    CLOSE (RECVIN,)                                                        
         SPACE 1                                                                
         B     EOJ                                                              
         SPACE 1                                                                
* GET NEXT OUTPUT RECORD *                                                      
         SPACE 1                                                                
*******                                                                         
******     OLD CODE FROM SPOT PEEL                                              
******                                                                          
OUT2     GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R6,15,4(R1)                                                      
         BZ    EOJ                                                              
*                                                                               
OUT4     DS    0H                                                               
         MVC   RLEN(256),0(R6)     MOVE START OF RECORD                         
         ZIC   RE,RSORTKEY+1                                                    
         SRL   RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P(2),DUB                                                         
         GOTO1 =V(CLUNPK),DMCB,RSORTKEY+2,P+3                                   
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),RSORTKEY+4                                              
         GOTO1 =V(MSUNPK),DMCB,DUB,WORK,P+7                                     
         GOTO1 =V(DATCON),DMCB,(2,RSORTKEY+7),P+13                              
         GOTO1 =V(HEXOUT),DMCB,RSORTKEY+4,P+24,3,=C'N'                          
         GOTO1 =V(HEXOUT),DMCB,RVCHR,P+36,4                                     
         GOTO1 =V(PRINTER)                                                      
         B     OUT2                                                             
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=04628,           X        
               BLKSIZE=08000,MACRF=PM                                           
         EJECT                                                                  
DMCB     DS    6F                                                               
MYSEQ    DC    F'0'                                                             
OUTCNT   DC    PL4'0'                                                           
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
RSORTKEY DS    0XL13                                                            
*                                                                               
RSORTAM  DS    XL1                                                              
RSORTCLT DS    XL2                                                              
RSORTMKT DS    XL2                                                              
RSORTSTA DS    XL3                                                              
RSORTPRD DS    XL1                                                              
RSORTEST DS    XL1                                                              
RSORTBUY DS    XL3                                                              
*                                                                               
RSORTSEQ DS    XL3                                                              
RSPARE   DS    XL4                                                              
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
RECVR    DS    0CL25                                                            
         DS    4800C                                                            
*                                                                               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
ELEMTAB  DS    2000C                                                            
*                                                                               
WORKX    DS    0X                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051PPRCVPEEL 05/01/02'                                      
         END                                                                    
