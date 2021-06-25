*          DATA SET SRCTR00    AT LEVEL 002 AS OF 08/22/00                      
*PHASE T11C00A                                                                  
         TITLE '$CTRY - DISPLAY/SET COUNTRY '                                   
         PRINT NOGEN                                                            
CTRY     CSECT                                                                  
         NMOD1 SRWORKX-SRWORKD,*$CTRY**                                         
         USING SRWORKD,RC                                                       
         USING SRPARMD,R1                                                       
         L     RA,SRPARM6          RA=A(TWA)                                    
         USING SRLANFFD,RA                                                      
         L     R9,SRPARM1          R9=A(SYSFACS)                                
         USING SYSFACD,R9                                                       
         L     R5,SRPARM3          R5=A(UTL ENTRY)                              
         USING UTLD,R5                                                          
         SPACE 1                                                                
         L     RE,VSSB             GET A(COUNTRY TABLE)                         
         L     RE,SSBACTRY-SSBD(RE)                                             
         ST    RE,ACTRYTAB                                                      
         MVC   INPSHR,SPACES       CLEAR INPUT SHORT NAME                       
         EJECT                                                                  
CTRY1    LA    R4,SRVSRVH          TEST IF NEW CTRY INPUT $CTRY,XXX             
         LA    RF,8(R4)                                                         
         LA    R0,8                                                             
CTRY1A   CLI   0(RF),C','                                                       
         BE    CTRY2                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,CTRY1A                                                        
         B     CTRYX                                                            
*                                                                               
CTRY2    LA    RF,1(RF)            POINT TO START OF COUNTRY NAME               
         LR    RE,RF                                                            
         ST    RE,DUB              SAVE A(END OF S/R NAME)                      
         MVC   INPSHR(3),0(RE)     EXTRACT COUNTRY NAME                         
*                                                                               
CTRY3    L     RF,DUB              CLEAR NEW COUNTRY NAME FIELD                 
         BCTR  RF,0                                                             
         LA    R1,SRVSRV+L'SRVSRV-1                                             
         SR    R1,RF                                                            
         BM    CTRYX                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)                                                    
*                                                                               
CTRYX    EQU   *                                                                
         EJECT                                                                  
DISP     LA    R4,SRVL1H           POINT TO FIRST DISPLAY LINE                  
         USING DISPLD,R4                                                        
         L     R8,ACTRYTAB                                                      
         LA    R8,6(R8)            POINT TO FIRST COUNTRY ENTRY                 
         USING CTRYTABD,R8                                                      
         XC    SVOLD,SVOLD                                                      
         XC    SVNEW,SVNEW                                                      
         MVC   SVCTRY,TCTRY                                                     
*                                                                               
         MVC   DLBOX,STARS         SET TOP OF BOX                               
         MVC   DLLM,=C' --'                                                     
         MVC   DLRM,=C'-- '                                                     
         LA    R4,SRVL2-SRVL1(R4)                                               
*                                                                               
DISP1    CLI   0(R4),9             TEST END OF TWA                              
         BNH   DISP6                                                            
         CLI   CTRYCODE,X'FF'      TEST END OF TABLE                            
         BE    DISP6                                                            
*                                                                               
DISP2    MVC   DLLM,=C' : '        SET LEFT/CENTRE/RIGHT MARGINS                
         MVC   DLCM,=C' : '                                                     
         MVC   DLRM,=C' : '                                                     
         MVC   DLSHRN,CTRYSHRN     DISPLAY NATIVE SHORT AND FULL NAMES          
         MVI   DLSEPN,C'='                                                      
         MVC   DLFULN,CTRYNAMN                                                  
         MVC   DLSHR,CTRYSHR       DISPLAY ENGLISH SHORT AND FULL NAMES         
         MVI   DLSEP,C'='                                                       
         MVC   DLFUL,CTRYNAM                                                    
*                                                                               
DISP3    CLC   INPSHR,SPACES       TEST IF COUNTRY INPUT                        
         BE    DISP4                                                            
         CLC   CTRYSHRN(2),INPSHR  COMPARE WITH NATIVE SHORT NAME               
         BE    DISP3A                                                           
         CLC   CTRYSHR(2),INPSHR   COMPARE WITH ENGLISH SHORT NAME              
         BE    DISP3A                                                           
         B     DISP4                                                            
DISP3A   MVC   TCTRY,CTRYCODE      SET NEW CODE IN UTL                          
         MVC   DLLSHOW,LNEWSHOW    AND SHOW ON SCREEN                           
         MVC   DLRSHOW,RNEWSHOW                                                 
         OI    DLHDR+1,X'08'       SET HIGH INTENSITY                           
         ST    R4,SVNEW                                                         
         B     DISP5                                                            
*                                                                               
DISP4    CLC   SVCTRY,CTRYCODE     TEST TERMINAL COUNTRY CODE                   
         BNE   DISP5                                                            
         MVC   DLLSHOW,LOLDSHOW    SHOW ON SCREEN                               
         MVC   DLRSHOW,ROLDSHOW    SHOW ON SCREEN                               
         OI    DLHDR+1,X'08'       SET HIGH INTENSITY                           
         ST    R4,SVOLD                                                         
*                                                                               
DISP5    LA    R8,CTRYTABL(R8)     BUMP TO NEXT COUNTRY ENTRY                   
*&&UK*&& CLI   CTRYCODE,1                                                       
*&&US*&& CLI   CTRYCODE,2                                                       
         BE    DISP5                                                            
         LA    R4,SRVL2-SRVL1(R4)  BUMP TO NEXT DISPLAY LINE                    
         B     DISP1                                                            
*                                                                               
DISP6    OC    SVNEW,SVNEW         SET ONLY NEW CODE TO HIGH                    
         BZ    DISPX                                                            
         OC    SVOLD,SVOLD                                                      
         BZ    DISPX                                                            
         L     RE,SVOLD                                                         
         NI    1(RE),255-X'08'                                                  
*                                                                               
DISPX    CLI   0(R4),9             SET BOTTOM OF BOX                            
         BNH   EXIT                                                             
         MVC   DLBOX,STARS                                                      
         MVC   DLLM,=C' --'                                                     
         MVC   DLRM,=C'-- '                                                     
         B     EXIT                                                             
         EJECT                                                                  
* ERRORS & EXIT                                                                 
*                                                                               
ERROR1   LA    R1,ERRMSG1                                                       
         B     ERRXIT                                                           
ERROR2   LA    R1,ERRMSG2                                                       
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(11),=C'** ERROR **'                                       
         MVC   SRVMSG+12(40),0(R1)                                              
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
ERRMSG1  DC    CL40'MISSING INPUT FIELD'                                        
ERRMSG2  DC    CL40'INVALID INPUT FIELD'                                        
         SPACE 1                                                                
SPACES   DC    80C' '                                                           
STARS    DC    80C'-'                                                           
         SPACE 1                                                                
LNEWSHOW DC    CL8'    --->'                                                    
LOLDSHOW DC    CL8'      ->'                                                    
RNEWSHOW DC    CL8'<---    '                                                    
ROLDSHOW DC    CL8'<-      '                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
SRWORKD  DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
ACTRYTAB DS    A                                                                
SVOLD    DS    A                                                                
SVNEW    DS    A                                                                
INPSHR   DS    CL3                                                              
SVCTRY   DS    XL1                                                              
SRWORKX  DS    0C                                                               
         SPACE 2                                                                
DISPLD   DSECT                                                                  
DLHDR    DS    CL8                                                              
DLDATA   DS    0CL78                                                            
DLOFFSET DS    CL10                                                             
*                                                                               
DLLSHOW  DS    CL8                                                              
*                                                                               
DLBOX    DS    0CL43                                                            
DLLM     DS    CL3                                                              
DLSHRN   DS    CL3                                                              
DLSEPN   DS    CL1                                                              
DLFULN   DS    CL13                                                             
DLCM     DS    CL3                                                              
DLSHR    DS    CL3                                                              
DLSEP    DS    CL1                                                              
DLFUL    DS    CL13                                                             
DLRM     DS    CL3                                                              
*                                                                               
DLRSHOW  DS    CL8                                                              
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         EJECT                                                                  
SRLANFFD DSECT                                                                  
         DS    CL64                                                             
* SRCTRFFD                                                                      
       ++INCLUDE SRCTRFFD                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRCTR00   08/22/00'                                      
         END                                                                    
