*          DATA SET SPREPFXX3  AT LEVEL 044 AS OF 02/22/95                      
*PHASE SPFX02A                                                                  
         TITLE 'SPFX02 - BAD CANADIAN BUYS'                                     
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(3),1(R6)                                                   
         GOTO1 HIGH                                                             
         B     FX4                                                              
*                                                                               
FX2      GOTO1 SEQ                                                              
*                                                                               
FX4      CLC   KEY(5),KEYSAVE                                                   
         BNE   FX10                                                             
*                                                                               
         CLC   KEY+9(2),=X'48D3'                                                
         BNE   FX2                                                              
         GOTO1 HEXOUT,DMCB,KEY,P+10,13,=C'TOG'                                  
         GOTO1 REPORT                                                           
         B     FX2                                                              
*                                                                               
FX10     XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)        MOVE A-M/CLT                                 
         MVI   KEY+3,X'FF'         SET POL                                      
         GOTO1 HIGH                                                             
         B     FX22                                                             
*                                                                               
FX20     GOTO1 SEQ                                                              
*                                                                               
FX22     CLC   KEY(4),KEYSAVE      TEST A-M/CLT/PRD                             
         BE    FX25                                                             
         MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
*                                                                               
FX25     CLC   KEY+6(2),=X'48D3'   TEST SJNT                                    
         BNE   FX20                                                             
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+10,13,=C'TOG'                                  
         GOTO1 REPORT                                                           
         B     FX20                                                             
*                                                                               
         MVC   MYKEY,KEY           SAVE CURRENT KEY                             
***      MVC   KEY+6(2),=X'207A'   SET CORRECT STATION                          
***      GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         B     FX30                                                             
         BNE   FX30                                                             
         GOTO1 HEXOUT,DMCB,KEY,P+15,13,=C'TOG'                                  
         MVC   P(11),=C'*** DUP KEY'                                            
         GOTO1 REPORT                                                           
         B     FX20                                                             
*                                                                               
FX30     DS    0H                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         OI    15(R6),X'80'                                                     
         GOTO1 PUT                                                              
         MVC   P(7),=C'PUT BUY'                                                 
         GOTO1 HEXOUT,DMCB,(R6),P+10,24,=C'TOG'                                 
         GOTO1 REPORT                                                           
*                                                                               
***      NI    15(R6),X'7F'        RESET DELETED BIT                            
***      MVC   6(2,R6),=X'207A'    FIX THE KEY                                  
***      ST    R6,AREC                                                          
***      GOTO1 ADD                 ADD NEW RECORD                               
*                                                                               
***      MVC   P(7),=C'ADD BUY'                                                 
***      GOTO1 HEXOUT,DMCB,(R6),P+10,24,=C'TOG'                                 
***      GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        DELETE THE OLD BUY                           
         GOTO1 WRITE                                                            
*                                                                               
         MVC   P(7),=C'WRT DIR'                                                 
         GOTO1 HEXOUT,DMCB,KEY,P+10,24,=C'TOG'                                  
         GOTO1 REPORT                                                           
         B     FX20                                                             
* NOW NEED TO FIX X'68' ELEMENT IN NETWORK BUY                                  
         MVI   ELCODE,X'68'                                                     
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(4),2(R6)                                                     
         MVI   DUB+4,C'N'                                                       
         GOTO1 MSPACK,DMCB,=C'0000',DUB,KEY+4                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
FX32     BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   4(2,R6),=X'20A1'                                                 
         BNE   FX32                                                             
         MVC   4(2,R6),=X'207A'                                                 
         GOTO1 PUTBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'NET FIX',(R6),C'DUMP',(R0),=C'1D00'               
*                                                                               
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     FX20                                                             
*                                                                               
MYKEY    DS    XL24                                                             
*                                                                               
*X25     CLI   KEY+17,0            TEST ADDED THIS WEEK                         
         BE    FX20                YES - SKIP                                   
*                                                                               
         CLC   =C'YR',QAGY                                                      
         BNE   FX30                                                             
         CLI   KEY+8,X'20'         TEST BRAVO                                   
         BE    FXDEL                                                            
         B     FX200                                                            
*                                                                               
*X30     CLC   =C'MI',QAGY                                                      
         BNE   FX40                                                             
         CLI   KEY+8,X'20'                                                      
         BE    FXDEL                                                            
         CLI   KEY+8,X'21'                                                      
         BE    FXDEL                                                            
         CLI   KEY+8,X'22'                                                      
         BE    FXDEL                                                            
         CLI   KEY+8,X'23'                                                      
         BE    FXDEL                                                            
         CLI   KEY+8,X'25'                                                      
         BE    FXDEL                                                            
         B     FX200                                                            
*                                                                               
FX40     CLC   =C'HM',QAGY                                                      
         BNE   FX50                                                             
         B     FX200                                                            
*                                                                               
FX50     CLC   =C'ME',QAGY                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY+8,X'0E'                                                      
         BE    FXDEL                                                            
         CLI   KEY+8,X'1B'                                                      
         BE    FXDEL                                                            
         CLI   KEY+8,X'20'                                                      
         BE    FXDEL                                                            
         CLI   KEY+8,X'21'                                                      
         BE    FXDEL                                                            
         CLI   KEY+8,X'22'                                                      
         BE    FXDEL                                                            
         B     FX200                                                            
                                                                                
FXDEL    OI    KEY+13,X'80'                                                     
         BC    0,FXDEL2                                                         
         MVI   *-3,X'F0'                                                        
         GOTO1 PRNTBL,DMCB,=C'DIRKEY',KEY,C'DUMP',18,=C'1D00'                   
*                                                                               
FXDEL2   CLI   RCWRITE,C'Y'                                                     
         BNE   FXDEL4                                                           
         GOTO1 WRITE                                                            
*                                                                               
* NOW DELETE THE BUY RECORD                                                     
*                                                                               
FXDEL4   DS    0H                                                               
         GOTO1 GETBUY                                                           
         L     R6,ADBUY                                                         
         OI    15(R6),X'80'                                                     
*                                                                               
         BC    0,FXDEL6                                                         
         MVI   *-3,X'F0'                                                        
         GOTO1 PRNTBL,DMCB,=C'FILREC',(R6),C'DUMP',18,=C'1D00'                  
*                                                                               
FXDEL6   DS    0H                                                               
         GOTO1 PUTBUY                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+10,13,=C'TOG'                                  
         GOTO1 REPORT                                                           
         B     FX20                                                             
*                                                                               
* SKIP TO NEXT STATION                                                          
*                                                                               
FX200    MVC   KEY+9(4),=X'FFFFFFFF'                                            
         GOTO1 HIGH                                                             
         B     FX22                                                             
*                                                                               
*                                                                               
FXEND    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
ELCODE   DS    X                                                                
STAWORK  DS    XL31                                                             
BAGYTAB  DS    16XL4                                                            
         DS    F                                                                
RECORD   DS    CL50                                                             
SPACE    DS    XL2000                                                           
                                                                                
TABLE    DS    0CL4                                                             
         DC    C'CD',X'4213'                                                    
         DC    C'CD',X'4237'                                                    
         DC    C'CD',X'4249'                                                    
         DC    C'DN',X'2095'                                                    
         DC    C'GR',X'3BBE'                                                    
         DC    C'GR',X'3BBF'                                                    
         DC    C'GR',X'3BB7'                                                    
         DC    C'GR',X'3BB8'                                                    
         DC    C'GR',X'453C'                                                    
         DC    C'GR',X'453E'                                                    
         DC    C'GR',X'4542'                                                    
         DC    C'GR',X'48CE'                                                    
         DC    C'GR',X'48CF'                                                    
         DC    C'GR',X'48C7'                                                    
         DC    C'GR',X'48C8'                                                    
         DC    C'GR',X'60EB'                                                    
         DC    C'HM',X'6625'                                                    
         DC    C'HM',X'85B4'                                                    
         DC    C'HM',X'85CC'                                                    
         DC    C'HM',X'85D8'                                                    
         DC    C'ME',X'04BB'                                                    
         DC    C'ME',X'04B3'                                                    
         DC    C'ME',X'04B4'                                                    
         DC    C'ME',X'04CB'                                                    
         DC    C'ME',X'04D9'                                                    
         DC    C'ME',X'04F7'                                                    
         DC    C'ME',X'3FCC'                                                    
         DC    C'ME',X'43CD'                                                    
         DC    C'ME',X'43C7'                                                    
         DC    C'ME',X'43C9'                                                    
         DC    C'ME',X'4670'                                                    
         DC    C'ME',X'686B'                                                    
         DC    C'ME',X'686C'                                                    
         DC    C'ME',X'8AEA'                                                    
         DC    C'ME',X'8B17'                                                    
         DC    C'MI',X'0718'                                                    
         DC    C'MI',X'0722'                                                    
         DC    C'MI',X'3D71'                                                    
         DC    C'MI',X'3810'                                                    
         DC    C'MI',X'383C'                                                    
         DC    C'MI',X'4389'                                                    
         DC    C'MI',X'4391'                                                    
         DC    C'MI',X'530B'                                                    
         DC    C'MI',X'6DC1'                                                    
         DC    C'MI',X'6DC4'                                                    
         DC    C'MI',X'6DC5'                                                    
         DC    C'MI',X'6DC6'                                                    
         DC    C'MT',X'01A0'                                                    
         DC    C'MT',X'01BF'                                                    
         DC    C'MT',X'019C'                                                    
         DC    C'MT',X'414D'                                                    
         DC    C'MT',X'414E'                                                    
         DC    C'MT',X'417B'                                                    
         DC    C'MT',X'46D1'                                                    
         DC    C'MT',X'46D9'                                                    
         DC    C'MT',X'463C'                                                    
         DC    C'MT',X'4640'                                                    
         DC    C'MT',X'4706'                                                    
         DC    C'MT',X'6DC5'                                                    
         DC    C'MT',X'6EF8'                                                    
         DC    C'MT',X'6EF9'                                                    
         DC    C'MT',X'89BE'                                                    
         DC    C'MW',X'1DB6'                                                    
         DC    C'MW',X'1D8C'                                                    
         DC    C'MW',X'2D4C'                                                    
         DC    C'SM',X'0E42'                                                    
         DC    C'VH',X'06C5'                                                    
         DC    C'VH',X'7118'                                                    
         DC    C'YR',X'03C1'                                                    
         DC    C'YR',X'03FD'                                                    
         DC    C'YR',X'12D0'                                                    
         DC    X'FF'                                                            
*                                                                               
AGENCYD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044SPREPFXX3 02/22/95'                                      
         END                                                                    
