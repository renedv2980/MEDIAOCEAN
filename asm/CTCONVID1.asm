*          DATA SET CTCONVID1  AT LEVEL 126 AS OF 05/01/02                      
*PHASE CONVID1                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE GETFACT                                                                
         TITLE 'CONVID1 - BUILD ACCESS RECORD LIST'                             
CONVID1  CSECT                                                                  
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
         XC    AGYCNT,AGYCNT                                                    
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(34),=C'ACCESS RECORD SUMMARY LIST BUILD'                   
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
* ??     OPEN  (TAPEOUT,OUTPUT)                                                 
         OPEN  (DDOUT,OUTPUT)                                                   
         LA    R2,IO                                                            
         USING CT5KEY,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   CT5KTYP,C'5'                                                     
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CT5KEY,CT5KEY                        
         B     CONV18                                                           
*                                                                               
CONV14   MVC   CT5KEY(L'IOKEY),IOKEY                                            
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT5KEY,CT5KEY                        
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
*                                                                               
CONV16   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CT5KEY,CT5KEY                        
         B     CONV18                                                           
*                                                                               
CONV18   CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   IOKEY,CT5KEY                                                     
         CLI   CT5KTYP,C'5'                                                     
         BNE   CONV300                                                          
         B     CONV20                                                           
*                                                                               
CONV20   MVC   P,SPACES                                                         
         MVI   P,C'+'                                                           
         MVC   P+2(2),CT5KALPH                                                  
         LA    R3,CT5DATA                                                       
         SR    R0,R0                                                            
         USING CTDSCD,R3                                                        
CONV22   CLI   CTDSCEL,0                                                        
         BE    CONV100                                                          
         CLI   CTDSCEL,CTDSCELQ                                                 
         BNE   CONV24                                                           
         B     CONV26                                                           
CONV24   IC    R0,CTDSCLEN                                                      
         AR    R3,R0                                                            
         B     CONV22                                                           
*                                  GET CTPRID DATA                              
CONV26   EQU   *                                                                
         MVC   IDNUM,CTDSC                                                      
         BAS   RE,GETIDA                                                        
         BE    *+14                                                             
         MVC   P+5(10),=C'??????????'                                           
         B     CONV24                                                           
         MVC   P+5(10),IDALPH                                                   
         MVC   P+16(33),IDAGYN                                                  
         B     CONV24                                                           
         DROP  R3                                                               
*                                                                               
CONV100  MVC   LINEBUFF,P                                                       
         PUT   DDOUT,LINEBUFF                                                   
         GOTO1 VPRINTER                                                         
         L     RF,AGYCNT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AGYCNT                                                        
         B     CONV200                                                          
*                                                                               
CONV200  SR    RE,RE                                                            
         ICM   RE,3,CT5LEN                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
* ??     PUT   TAPEOUT,IOL                                                      
         B     CONV14                                                           
*                                                                               
CONV300  MVC   P,SPACES                                                         
         MVC   P(10),=C'END OF RUN'                                             
         GOTO1 VPRINTER                                                         
         MVC   P(21),=C'NUMBER OF AGENCIES = '                                  
         EDIT  AGYCNT,(8,P+21),ALIGN=LEFT,ZERO=NOBLANK                          
         GOTO1 VPRINTER                                                         
* ??     CLOSE (TAPEOUT)                                                        
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT ID NUMBER TO ID ALPHA                                       *         
***********************************************************************         
GETIDA   NTR1  ,                                                                
         LA    R4,2000(R2)                                                      
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),IDNUM                                                
         NI    CTIKID+8,X'FF'-X'80'   TURN OFF GENERIC ID BIT                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY                        
         BNE   GIDANO                                                           
*                                                                               
GIDA010  LA    R1,CTIDATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
GIDA020  CLI   0(R1),0                                                          
         BE    GIDAOK                                                           
         CLI   0(R1),CTDSCELQ                                                   
         BNE   *+14                                                             
         MVC   IDALPH,CTDSC-CTDSCD(R1)                                          
         B     GIDA030                                                          
         CLI   0(R1),CTAGYELQ                                                   
         BNE   *+14                                                             
         MVC   IDAGYA,CTAGYID-CTAGYD(R1)                                        
         B     GIDA030                                                          
         CLI   0(R1),CTDSTELQ                                                   
         BNE   *+14                                                             
         MVC   IDAGYN,CTDSTNAM-CTDSTD(R1)                                       
         B     GIDA030                                                          
*                                                                               
GIDA030  IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     GIDA020                                                          
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
DDOUT    DCB   DDNAME=DDOUT,DSORG=PS,RECFM=FB,MACRF=PM                 *        
               BLKSIZE=3200,LRECL=80                                            
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
AGYCNT   DS    F                                                                
IDNUM    DS    XL2                 USER ID NUMBER                               
IDALPH   DS    CL10                USER ID ALPHA CODE                           
IDAGYA   DS    CL2                 USER ID AGENCY ALPHA CODE                    
IDAGYN   DS    CL33                USER ID AGENCY NAME                          
LINEBUFF DS    XL80                                                             
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CT5KEY)                                                     
IOL      DS    F                                                                
IO       DS    2000X                                                            
IO2      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'126CTCONVID1 05/01/02'                                      
         END                                                                    
