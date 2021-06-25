*          DATA SET SECONVAUT  AT LEVEL 063 AS OF 05/01/02                      
*PHASE SECAUT                                                                   
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE GETFACT                                                                
         TITLE 'SECAUT- JOMU TEST CONTROL FILE OFFLINE PROGRAM'                 
SECAUT   CSECT                                                                  
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
         MVC   TITLE(34),=C'JIM TEST                          '                 
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         LA    R2,IO                                                            
         USING CT0KEY,R2                                                        
         XC    CT0KEY,CT0KEY                                                    
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CT0KEY,CT0KEY                        
         B     CONV18                                                           
*                                                                               
CONV14   MVC   CT0KEY(L'IOKEY),IOKEY                                            
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT0KEY,CT0KEY                        
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
*                                                                               
CONV16   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CT0KEY,CT0KEY                        
         B     CONV18                                                           
*                                                                               
CONV18   CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   IOKEY,CT0KEY                                                     
         CLI   CT0KTYP,C'0'                                                     
         BNE   CONV200                                                          
         TM    CT0STAT,X'20'                                                    
         BO    CONV200                                                          
         TM    CT0STAT,X'40'                                                    
         BZ    CONV200                                                          
         OC    CT0KEYS(20),CT0KEYS                                              
         BNZ   CONV200                                                          
         B     CONV20                                                           
         DC    H'00'                                                            
*                                                                               
CONV20   MVC   P,SPACES                                                         
         MVC   P+2(2),CT0KAGY                                                   
         MVC   AGENCY,CT0KAGY                                                   
         GOTO1 VHEXOUT,PARM,CT0KNUM,P+6,2,=C'TOG'                               
         MVC   IDNUM,CT0KNUM                                                    
         LA    R3,CT0DATA                                                       
         SR    R0,R0                                                            
         MVI   PRIFLAG,0                                                        
*                                                                               
CONV22   CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R3),X'03'                                                      
         BNE   CONV24                                                           
         CLI   1(R3),X'0C'                                                      
         BNE   CONV24                                                           
         B     CONV26                                                           
CONV24   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CONV22                                                           
*                                                                               
CONV26   MVC   IDALPH,2(R3)                                                     
         OC    IDALPH,IDALPH                                                    
         BNZ   *+6                                                              
         DC    H'00'                                                            
         BAS   RE,GETIDA                                                        
         BE    CONV20A                                                          
         MVI   PRIFLAG,1                                                        
         MVC   P+20(19),=C'AUTH ALPHA MISSING'                                  
         B     CONV20B                                                          
CONV20A  MVC   P+20(17),=C'AUTH ALPHA FOUND'                                    
CONV20B  MVC   P+42(10),IDALPH                                                  
*                                                                               
CONV100  CLI   MVSPARM,0                                                        
         BE    CONV110                                                          
         CLI   PRIFLAG,0                                                        
         BE    CONV200                                                          
CONV110  GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CONV200  SR    RE,RE                                                            
         ICM   RE,3,CT0LEN                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
         PUT   TAPEOUT,IOL                                                      
         B     CONV14                                                           
*                                                                               
CONV300  MVC   P,SPACES                                                         
         MVC   P(10),=C'END OF RUN'                                             
         GOTO1 VPRINTER                                                         
         CLOSE (TAPEOUT)                                                        
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT ID NUMBER TO ID ALPHA                                       *         
***********************************************************************         
GETIDA   NTR1  ,                                                                
         LA    R4,2000(R2)                                                      
         USING CT0REC,R4                                                        
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,C'0'                                                     
         MVC   CT0KAGY,AGENCY                                                   
         MVC   CT0KCODE,IDALPH                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT0KEY,CT0KEY                        
         BNE   GIDANO                                                           
         B     GIDAOK                                                           
*                                                                               
GIDANO   B     NO                                                               
*                                                                               
GIDAOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
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
AGENCY   DS    CL2                                                              
IDNUM    DS    XL2                                                              
IDALPH   DS    CL10                                                             
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CT0KEY)                                                     
IOL      DS    F                                                                
IO       DS    2000X                                                            
IO2      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063SECONVAUT 05/01/02'                                      
         END                                                                    
