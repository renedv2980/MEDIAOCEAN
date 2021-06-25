*          DATA SET SREOJ00    AT LEVEL 003 AS OF 08/22/00                      
*PHASE T11300A                                                                  
         TITLE '$EOJ - STOP ONLINE SYSTEM'                                      
         PRINT NOGEN                                                            
EOJ      CSECT                                                                  
         NMOD1 SRWORKX-SRWORKD,**$EOJ**                                         
         USING SRWORKD,RC                                                       
         USING SRPARMD,R1                                                       
         L     RA,SRPARM1          A(SYSFAC)                                    
         USING SYSFACD,RA                                                       
         L     R3,SRPARM6          A(TWA)                                       
         USING SREOJFFD,R3                                                      
         L     R4,SRPARM3          A(UTL ENTRY)                                 
         USING UTLD,R4                                                          
*                                                                               
         L     RE,VSSB             SET BTAM/VTAM APPLICATION                    
         MVC   SYSID,SSBSYSID-SSBD(RE)                                          
         MVC   SYSCHR,SSBSYSCH-SSBD(RE)                                         
         MVC   SYSNAME,SSBSYSN4-SSBD(RE)                                        
         MVI   VTAM,C'N'                                                        
         CLI   SSBVTID-SSBD(RE),C' '                                            
         BNH   BEOJ                                                             
         MVI   VTAM,C'Y'                                                        
         B     VEOJ                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* BTAM - TEST IF ANY LINES OTHER THAN THIS ONE STILL ACTIVE                     
*                                                                               
BEOJ     L     R5,VDECBLST                                                      
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING DECBD,R5                                                         
         TM    DECBSTAT,X'80'                                                   
         BNZ   BEOJ4                                                            
BEOJ2    BXLE  R5,R6,*-8                                                        
         B     BEOJX                                                            
BEOJ4    C     R5,TDECB            LINE IS ACTIVE - IS IT US                    
         BE    BEOJ2                                                            
BEOJERR  XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(33),=C'LINES STILL ACTIVE. NO EOJ ISSUED'                 
         MVC   TSVCREQ,=X'01EE'    INHIBIT TSKCNTL EOJ $BYE                     
*                                                                               
BEOJX    GOTO1 VTICTOC,DUB,C'SGET'                                              
         L     R1,0(R1)                                                         
         ST    R1,TIME                                                          
         MVC   SRVMSG+35(10),=X'402120204B20204B2020'                           
         ED    SRVMSG+35(10),TIME                                               
         B     EXIT                                                             
         EJECT                                                                  
* VTAM - MUST CONFIRM REQUEST WITH CORRECT FACPAC ID IN PARAM 1                 
*                                                                               
VEOJ     MVC   FULL(4),SRVP1       P1 MUST CONTAIN FACPAK ID NAME               
         OC    FULL,=CL8' '                                                     
         CLC   SYSNAME,FULL                                                     
         BNE   VEOJERR                                                          
         B     VEOJX                                                            
*                                                                               
VEOJERR  XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(34),=C'ED/9999 (XXXX) INVALID FACPAK NAME'                
         MVC   SRVMSG+2(1),SYSCHR                                               
         MVC   SRVMSG+9(4),SYSNAME                                              
         B     VEOJERRX                                                         
*                                                                               
VEOJERRX MVC   TSVCREQ,=X'01EE'    INHIBIT TSKCNTL EOJ $BYE                     
         B     EXIT                                                             
*                                                                               
VEOJX    XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(25),=C'FACPAK (XXXX) SYSTEM DOWN'                         
         MVC   SRVMSG+8(4),SYSNAME                                              
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SRWORKD  DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
TIME     DS    PL4                                                              
VTAM     DS    C                                                                
SYSID    DS    X                                                                
SYSCHR   DS    C                                                                
SYSNAME  DS    CL4                                                              
SRWORKX  DS    0C                                                               
*                                                                               
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SREOJFFD DSECT                                                                  
         DS    CL64                                                             
* SREOJFFD                                                                      
       ++INCLUDE SREOJFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SREOJ00   08/22/00'                                      
         END                                                                    
