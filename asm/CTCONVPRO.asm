*          DATA SET CTCONVPRO  AT LEVEL 100 AS OF 09/23/91                      
*PHASE CONVPRO                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE GETFACT                                                                
         TITLE 'CONVJIM - CHECK P RECORDS FOR PRIORITY CODE'                    
CONVJIM  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CONV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         MVI   MVSPARM,0                                                        
         L     R1,0(R1)            GET MVS PARMS                                
         SR    RF,RF                                                            
         LH    RF,0(R1)                                                         
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         MVI   MVSPARM,1                                                        
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(34),=C'PROFILE RECORDS PRIORITY CODE DATA'                 
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
* ??     OPEN  (TAPEOUT,OUTPUT)                                                 
         LA    R2,IO                                                            
         USING CTPKEY,R2                                                        
         XC    CTPKEY,CTPKEY                                                    
         GOTO1 ,DMCB,DMRDHI,CTFILE,CTPKEY,CTPKEY                                
*                                                                               
CONV14   GOTO1 VDATAMGR,DMCB       GET FIRST/NEXT RECORD                        
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
*                                                                               
         CLI   CTPKTYP,C'P'                                                     
         BNE   CONV200                                                          
         B     CONV20                                                           
*                                                                               
CONV20   MVC   P+2(1),CTPKSYS                                                   
         MVC   P+4(2),CTPKPROG                                                  
         GOTO1 VHEXOUT,PARM,CTPKORIG,P+7,2,=C'TOG'                              
         LA    R5,P+14                                                          
         LA    R3,CTPDATA                                                       
         SR    R0,R0                                                            
         MVI   PRIFLAG,0                                                        
         USING CTPRID,R3                                                        
CONV22   CLI   CTPRIEL,0                                                        
         BE    CONV100                                                          
         CLI   CTPRIEL,CTPRIELQ                                                 
         BNE   CONV24                                                           
         B     CONV26                                                           
CONV24   IC    R0,CTPRILEN                                                      
         AR    R3,R0                                                            
         B     CONV22                                                           
*                                  GET CTPRID DATA                              
CONV26   EQU   *                                                                
         CLI   CTPRITY+1,0                                                      
         BE    CONV28                                                           
         CLI   CTPRITY+1,C' '                                                   
         BE    CONV28                                                           
         MVI   P,C'*'                                                           
         MVI   PRIFLAG,1                                                        
CONV28   MVC   0(6,R5),CTPRITYP                                                 
         LA    R5,8(R5)                                                         
         B     CONV24                                                           
         DROP  R3                                                               
*                                                                               
CONV100  CLI   MVSPARM,0                                                        
         BE    CONV110                                                          
         CLI   PRIFLAG,0                                                        
         BE    CONV200                                                          
CONV110  GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CONV200  SR    RE,RE                                                            
         ICM   RE,3,CTPLEN                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
* ??     PUT   TAPEOUT,IOL                                                      
         B     CONV14                                                           
*                                                                               
CONV300  MVC   P,SPACES                                                         
         MVC   P(10),=C'END OF RUN'                                             
         GOTO1 VPRINTER                                                         
* ??     CLOSE (TAPEOUT)                                                        
         XBASE                                                                  
         SPACE 2                                                                
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
MAXLEN   DC    H'999'                                                           
         SPACE 1                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VGETFACT DC    V(GETFACT)                                                       
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
         SPACE 1                                                                
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
         SPACE 1                                                                
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
ASYSLST  DS    A                                                                
PROGRAM  DS    XL1                                                              
PRIFLAG  DS    XL1                                                              
MVSPARM  DS    XL1                                                              
WORK1    DS    XL80                                                             
WORK     DS    XL256                                                            
IOL      DS    F                                                                
IO       DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'100CTCONVPRO 09/23/91'                                      
         END                                                                    
