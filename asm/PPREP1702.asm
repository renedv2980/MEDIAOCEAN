*          DATA SET PPREP1702  AT LEVEL 012 AS OF 05/01/02                      
*PHASE PP1702A,+0                                                               
         TITLE 'PP1702 - AUTO RATE CHG - WORKER FILE READ'                      
PP1702   CSECT                                                                  
         ENTRY WRKRBUFF                                                         
         PRINT NOGEN                                                            
         NMOD1 0,PP1702,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         LA    R8,SPACEND                                                       
         USING PP17WRKD,R8                                                      
*                                                                               
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         AH    R9,=H'4096'                                                      
         USING PPFILED,RC,R9                                                    
         EJECT                                                                  
         CLI   MODE,PROCREQ                                                     
         BE    RDW                                                              
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RDW      DS    0H                                                               
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         XC    WORK,WORK                                                        
*                                                                               
         MVC   DUB+00(2),RCDATE+6                                               
         MVC   DUB+02(2),RCDATE+0                                               
         MVC   DUB+04(2),RCDATE+3                                               
         MVC   TODAY,DUB                                                        
         CLC   QSTART,SPACES                                                    
         BE    *+10                                                             
         MVC   TODAY,QSTART                                                     
         PACK  FULL(2),TODAY+4(3)                                               
         MVC   WRKDAY,FULL                                                      
         L     RF,=A(WRKRBUFF)                                                  
         A     RF,RELO                                                          
         ST    RF,MAPFILE                                                       
         XC    WID,WID                                                          
         LA    R7,WID                                                           
         USING UKRECD,R7                                                        
RDW10    DS    0H                                                               
         GOTO1 WORKER,DMCB,=C'INDEX',MAPFILE,WID                                
*                                                                               
         TM    DMCB+8,X'90'        REC NOT FOUND OR EOF                         
         BNZ   RDW50                                                            
         CLC   UKSYSPRG,=C'P16'                                                 
         BNE   RDW10                                                            
         CLC   UKDAY,WRKDAY                                                     
         BNE   RDW10                                                            
*                                                                               
         DROP  R7                                                               
*                                                                               
         XC    WRKREC,WRKREC                                                    
RDW20    GOTO1 WORKER,DMCB,=C'READ',MAPFILE,WID,WRKREC                          
         TM    DMCB+8,X'90'                                                     
         BNZ   RDW10               EOF GO GET NEXT INDEX                        
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'02'                                                     
         MVC   BUFPFLE(1),WRKPFLE  FILE                                         
         MVC   BUFAGY(6),WRKAGY    AGY/MED/CLT                                  
         MVC   BUFOGRS(32),WRKOGRS                                              
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFCLT,=3X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFMED,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFAGY,=2X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFPFLE,X'FF'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         B     RDW20                                                            
         EJECT                                                                  
RDW50    XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'02'                                                     
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     RDW60                                                            
RDW55    GOTO1 BUFFALO,DMCB,=C'SEQ',BUFFBUFF,BUFREC,0                           
*                                                                               
RDW60    CLI   DMCB+8,X'80'                                                     
         BE    RDW65                                                            
         CLI   BUFTYP,X'02'                                                     
         BE    RDW70                                                            
*                                                                               
RDW65    B     EXIT                                                             
         SPACE 2                                                                
RDW70    MVI   STARS,0                                                          
         MVC   P+2(1),BUFPFLE                                                   
         CLI   BUFPFLE,X'FF'                                                    
         BNE   *+18                                                             
         MVC   P+1(5),=C'TOTAL'                                                 
         MVI   SPACING,2                                                        
         B     RDW74                                                            
*                                                                               
         MVC   P+9(2),BUFAGY                                                    
         CLI   BUFAGY,X'FF'                                                     
         BNE   *+18                                                             
         MVC   P+7(8),=C'TOTAL***'                                              
         MVI   STARS,3                                                          
         B     RDW74                                                            
         MVC   P+16(1),BUFMED                                                   
         CLI   BUFMED,X'FF'                                                     
         BNE   *+18                                                             
         MVC   P+14(7),=C'TOTAL**'                                              
         MVI   STARS,2                                                          
         B     RDW74                                                            
         MVC   P+22(3),BUFCLT                                                   
         CLI   BUFCLT,X'FF'                                                     
         BNE   *+14                                                             
         MVC   P+20(6),=C'TOTAL*'                                               
         MVI   STARS,1                                                          
*                                                                               
RDW74    DS    0H                                                               
         LA    R4,P+26                                                          
         LA    R5,3                                                             
         LA    R6,BUFOGRS                                                       
*                                                                               
RDW75    EDIT  (P8,0(R6)),(14,0(R4)),2,COMMAS=YES,FLOAT=-                       
         LA    R4,20(R4)                                                        
         LA    R6,8(R6)                                                         
         BCT   R5,RDW75                                                         
         EDIT  (P8,BUFINS),(10,P+86),0,COMMAS=YES                               
         CLI   STARS,0                                                          
         BE    RDW80                                                            
         MVI   SPACING,2                                                        
         MVI   P+40,C'*'                                                        
         MVI   P+60,C'*'                                                        
         MVI   P+80,C'*'                                                        
         MVI   P+96,C'*'                                                        
         CLI   STARS,1                                                          
         BE    RDW80                                                            
         MVI   P+41,C'*'                                                        
         MVI   P+61,C'*'                                                        
         MVI   P+81,C'*'                                                        
         MVI   P+97,C'*'                                                        
         CLI   STARS,2                                                          
         BE    RDW80                                                            
         MVI   P+42,C'*'                                                        
         MVI   P+62,C'*'                                                        
         MVI   P+82,C'*'                                                        
         MVI   P+98,C'*'                                                        
*                                                                               
RDW80    BAS   RE,CLPRT                                                         
         B     RDW55               GO DO NEXT BUF REC                           
         SPACE 2                                                                
CLPRT    NTR1                                                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
WRKRBUFF DS    0H                                                               
         DS    4096X                                                            
         EJECT                                                                  
PP17WRKD DSECT                                                                  
TODAY    DS    CL6                                                              
WRKDAY   DS    CL1                                                              
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
WID      DS    CL16                                                             
MAPFILE  DS    A                                                                
*                                                                               
STARS    DS    CL1                                                              
*                                                                               
         DS    0D                                                               
BUFREC   DS    0CL40                                                            
BUFKEY   DS    0CL8                                                             
BUFTYP   DS    CL1                                                              
BUFPFLE  DS    CL1                                                              
BUFAGY   DS    CL2                                                              
BUFMED   DS    CL1                                                              
BUFCLT   DS    CL3                                                              
BUFOGRS  DS    D                                                                
BUFNGRS  DS    D                                                                
BUFCGRS  DS    D                                                                
BUFINS   DS    D                                                                
         DS    0H                                                               
WRKREC   DS    0CL72                                                            
WRKLEN   DS    H                                                                
         DS    H                                                                
WRKPFLE  DS    CL1                                                              
WRKID    DS    CL8                                                              
WRKAGY   DS    CL2                                                              
WRKMED   DS    CL1                                                              
WRKCLT   DS    CL3                                                              
WRKPUB   DS    CL11                                                             
WRKCON   DS    CL3                                                              
         DS    CL7                 SPARE                                        
*                                                                               
WRKOGRS  DS    D                                                                
WRKNGRS  DS    D                                                                
WRKCGRS  DS    D                                                                
WRKINS   DS    D                                                                
       ++INCLUDE DMWRKRK                                                        
*                                                                               
         BUFF  LINES=1000,ROWS=1,COLUMNS=4,FLAVOR=PACKED,KEYLIST=(8,A)          
*                                                                               
       ++INCLUDE DDBUFFALOD                                                     
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
         EJECT                                                                  
       ++INCLUDE PPWORKD                                                        
         EJECT                                                                  
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012PPREP1702 05/01/02'                                      
         END                                                                    
