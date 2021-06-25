*          DATA SET PETSTADD   AT LEVEL 093 AS OF 05/11/05                      
*PHASE PETSTADA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLPE                                                                
*INCLUDE FATABOFF                                                               
*                                                                               
         TITLE 'TEST PERSON SYSTEM OFFLINE ADDS '                               
PETSTADD CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**PETS**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),=C'DDSIOC  '                                             
*                                                                               
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
*                                                                               
         BAS   RE,INIT             OPEN FILES ECT                               
*                                                                               
         BAS   RE,MAIN             MAIN PROGRAM                                 
*                                                                               
         BAS   RE,CLOSE            CLOSE FILES ECT                              
*                                                                               
XBASE    XBASE                     PROG EXIT                                    
*                                                                               
EXITN    LTR   RB,RB               EXIT CC=NEQ                                  
         B     EXIT                                                             
EXITY    CR    RB,RB               EXIT CC=EQU                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        INITIALISATION / OPEN FILES                        *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,PERSON,PFILES,AIO1                       
*                                                                               
         L     RE,=V(SSB)                                                       
         OI    SSOFLAG1-SSOOFF(RE),SSOFXCPY   TURN OFF SSB COPY                 
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
*************************************************************                   
*        CLOSE FILES                                        *                   
*************************************************************                   
         SPACE 1                                                                
CLOSE    NTR1                                                                   
         GOTO1 =V(DATAMGR),DMCB,DMCLSE,PERSON,0,AIO1                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
*************************************************************                   
*        MAIN                                               *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
*                                                                               
         L     RF,AIO1             SET UP A RECORD                              
         XC    0(44,RF),0(RF)                                                   
         MVC   0(5,RF),=C'RMORA'                                                
         MVC   5(2,RF),=X'0001'                                                 
         MVC   36(2,RF),=X'0042'                                                
         MVC   44(2,RF),=X'0115'                                                
         MVC   46(19,RF),=C'TEST RECORD GROUP A'                                
         MVI   65(RF),0                                                         
*                                                                               
ADDL1    GOTO1 =V(DATAMGR),DMCB,ADDREC,PERFIL,DA,AIO1,IOW                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO1                                                          
         SR    R1,R1                                                            
         ICM   R1,3,5(RF)                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,5(RF)                                                       
         CH    R1,=H'200'                                                       
         BL    ADDL1                                                            
*                                                                               
*                                                                               
         L     RF,AIO1             SET UP A RECORD                              
         XC    0(44,RF),0(RF)                                                   
         MVC   0(5,RF),=C'RMORB'                                                
         MVC   5(2,RF),=X'0001'                                                 
         MVC   36(2,RF),=X'0042'                                                
         MVC   44(2,RF),=X'0115'                                                
         MVC   46(19,RF),=C'TEST RECORD GROUP B'                                
         MVI   65(RF),0                                                         
*                                                                               
ADDL2    GOTO1 =V(DATAMGR),DMCB,ADDREC,PERFIL,DA,AIO1,IOW                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO1                                                          
         SR    R1,R1                                                            
         ICM   R1,3,5(RF)                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,5(RF)                                                       
         CH    R1,=H'200'                                                       
         BL    ADDL2                                                            
*                                                                               
*                                                                               
         L     RF,AIO1             SET UP A RECORD                              
         XC    0(44,RF),0(RF)                                                   
         MVC   0(5,RF),=C'RMORC'                                                
         MVC   5(2,RF),=X'0001'                                                 
         MVC   36(2,RF),=X'0042'                                                
         MVC   44(2,RF),=X'0115'                                                
         MVC   46(19,RF),=C'TEST RECORD GROUP C'                                
         MVI   65(RF),0                                                         
*                                                                               
ADDL3    GOTO1 =V(DATAMGR),DMCB,ADDREC,PERFIL,DA,AIO1,IOW                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO1                                                          
         SR    R1,R1                                                            
         ICM   R1,3,5(RF)                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,5(RF)                                                       
         CH    R1,=H'200'                                                       
         BL    ADDL3                                                            
*                                                                               
*                                                                               
         L     RF,AIO1             SET UP A RECORD                              
         XC    0(44,RF),0(RF)                                                   
         MVC   0(5,RF),=C'RMORD'                                                
         MVC   5(2,RF),=X'0001'                                                 
         MVC   36(2,RF),=X'0042'                                                
         MVC   44(2,RF),=X'0115'                                                
         MVC   46(19,RF),=C'TEST RECORD GROUP D'                                
         MVI   65(RF),0                                                         
*                                                                               
ADDL4    GOTO1 =V(DATAMGR),DMCB,ADDREC,PERFIL,DA,AIO1,IOW                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO1                                                          
         SR    R1,R1                                                            
         ICM   R1,3,5(RF)                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,5(RF)                                                       
         CH    R1,=H'200'                                                       
         BL    ADDL4                                                            
*                                                                               
*                                                                               
         L     RF,AIO1             SET UP A RECORD                              
         XC    0(44,RF),0(RF)                                                   
         MVC   0(5,RF),=C'RMORE'                                                
         MVC   5(2,RF),=X'0001'                                                 
         MVC   36(2,RF),=X'0042'                                                
         MVC   44(2,RF),=X'0115'                                                
         MVC   46(19,RF),=C'TEST RECORD GROUP E'                                
         MVI   65(RF),0                                                         
*                                                                               
ADDL5    GOTO1 =V(DATAMGR),DMCB,ADDREC,PERFIL,DA,AIO1,IOW                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO1                                                          
         SR    R1,R1                                                            
         ICM   R1,3,5(RF)                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,5(RF)                                                       
         CH    R1,=H'200'                                                       
         BL    ADDL5                                                            
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LITERALS                               *                   
*************************************************************                   
         SPACE 1                                                                
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLSE   DC    CL8'DMCLSE'                                                      
DMUNLK   DC    CL8'DMUNLK'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
*                                                                               
PERSON   DC    CL8'PERSON'                                                      
PERDIR   DC    CL8'PERDIR'                                                      
PERFIL   DC    CL8'PERFIL'                                                      
*                                                                               
PFILES   DC    CL8'UPERDIR '                                                    
         DC    CL8'UPERFIL '                                                    
         DC    CL8'UPERREQ '                                                    
         DC    CL8'UPERRCV '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SSB      CSECT                                                                  
         DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'00'    RECOVERY                                  
         DC    5XL4'00000000'                                                   
         DC    A(0)                                                             
         DC    A(0)                                                             
         ORG   SSB+45                                                           
         DC    C'T'                TEST DSPACE                                  
         ORG   SSB+52                                                           
         DC    A(SSB)                                                           
         ORG                                                                    
*                                                                               
WORKC    CSECT                                                                  
         DS    (64*1024)X             WORKING STORAGE POOL                      
         EJECT                                                                  
*************************************************************                   
*        DSECT TO COVER WORKING STORAGE                     *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG1    DS    X                                                                
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    CL255                                                            
WORK1    DS    XL64                                                             
IOW      DS    12D                                                              
DA       DS    F                                                                
*                                                                               
AIO1     DS    A                                                                
*                                                                               
KEY      DS    CL64                                                             
*                                                                               
BUFF2    DS    CL14336                                                          
         ORG   BUFF2                                                            
IOAREA   DS    4096C                                                            
IOAREA1  DS    4096C                                                            
         ORG                                                                    
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
************************************************************                    
*        OTHER DSECTS                                      *                    
************************************************************                    
         SPACE 1                                                                
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093PETSTADD  05/11/05'                                      
         END                                                                    
