*          DATA SET SRNOP00    AT LEVEL 003 AS OF 11/15/04                      
*PHASE T10300A                                                                  
         TITLE '$SYSNOP - DISCONNECT TERMINAL AFTER $START'                     
         PRINT NOGEN                                                            
SYSNOP   CSECT                                                                  
         NMOD1 5,*$SYSNOP                                                       
         USING SRWORKD,RC                                                       
         USING SRPARMD,R1                                                       
         L     RA,SRPARM6          A(TWA)                                       
         USING SRNOPFFD,RA                                                      
         L     R9,SRPARM1          R9=A(SYSFACS)                                
         USING SYSFACD,R9                                                       
*                                                                               
         L     R4,SRPARM3          A(UTL ENTRY)                                 
         USING UTLD,R4                                                          
*                                                                               
* RESET ALL FIELDS SET BY CONNECT PROGRAM                                       
*                                                                               
         TM    TSTAT1,TSTATDDS     RESTORE DDS TERMINAL OFFICE CODE             
         BZ    *+8                                                              
         MVI   TOFFCODE,C'*'                                                    
         MVC   DMCB(2),TSVCREQ                                                  
         XC    TCTDATA,TCTDATA                                                  
         MVC   TSVCREQ,DMCB                                                     
         OI    TSTAT+1,X'10'       SET TERM NOT INIT                            
         MVI   SRPARM1,X'FF'       WRITE-BACK TWA 0                             
         LA    R1,SRVSREQH         ERASE UNPROTS IN SYSNOP VIRGIN               
         SR    RE,RE                                                            
*                                                                               
SYS2     CLI   0(R1),0                                                          
         BE    SYS6                                                             
         TM    1(R1),X'20'                                                      
         BO    SYS4                                                             
         IC    RE,0(R1)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         OI    6(R1),X'80'         SET TRANSMIT BIT                             
*                                                                               
SYS4     IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         B     SYS2                                                             
*                                                                               
SYS6     MVC   SRVSREQ(3),=C'$CT'  FORCE CONNECT NEXT TIME                      
         OI    SRVP1H+6,X'40'                                                   
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
SRWORKD  DSECT                                                                  
TIME     DS    PL4                                                              
*                                                                               
DMCB     DS    6F                                                               
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SRNOPFFD DSECT                                                                  
         DS    CL64                                                             
* SRNOPFFD                                                                      
       ++INCLUDE SRNOPFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRNOP00   11/15/04'                                      
         END                                                                    
