*          DATA SET CTCONVIDS  AT LEVEL 025 AS OF 07/10/98                      
*PHASE CONVIDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
         TITLE 'CONVIDS - CONVERT ID/ACCESS RECORDS'                            
CONVIDS  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CONV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(23),=C'CONTROL FILE CONVERSION'                            
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         LA    R2,IO                                                            
         USING CTIKEY,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         GOTO1 ,DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY                                
*                                                                               
CONV02   GOTO1 VDATAMGR,DMCB       GET FIRST/NEXT RECORD                        
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONVX                                                            
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,CTILEN                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
*                                                                               
         CLI   CTIKTYP,CTIKTYPQ    TEST ID RECORDS                              
         BNE   CONV90                                                           
         MVC   RECNAME,SPACES                                                   
         MVC   RECNAME(8),=C'IDALPHA='                                          
         MVC   RECNAME+8(L'CTIKID),CTIKID                                       
         CLI   CTIKID,C' '                                                      
         BH    CONV10                                                           
         MVC   RECNAME,SPACES                                                   
         MVC   RECNAME(9),=C'IDNUMBER='                                         
         SR    R0,R0                                                            
         ICM   R0,3,CTIKNUM                                                     
         BZ    CONV90                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECNAME+9(5),DUB                                                 
         B     CONV10                                                           
*                                                                               
CONV10   EQU   *                                                                
         ICM   R0,3,CTILEN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECNAME+20(5),DUB                                                
         CLC   CTILEN,=AL2(998)                                                 
         BL    CONV12                                                           
         MVC   RECNAME+30(20),=CL20'<<LONG RECORD>'                             
*                                                                               
CONV12   EQU   *                                                                
*                                                                               
         LA    R4,CTIDATA                                                       
         SR    R5,R5                                                            
         USING CTSYSD,R4                                                        
CONV14   CLI   0(R4),0             TEST E-O-R                                   
         BE    CONV17A                                                          
         CLI   0(R4),X'06'                                                      
         BE    CONV16                                                           
         SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     CONV14                                                           
*                                                                               
CONV16   EQU   *                                                                
         CLI   1(R4),6                                                          
         BNE   CONV17B                                                          
         CLI   5(R4),0                                                          
         BNE   CONV17C                                                          
         MVC   RECNAME+50(10),=CL10'**OK*     '                                 
         B     CONV18                                                           
*                                                                               
CONV17A  EQU   *                                                                
         MVC   RECNAME+50(10),=CL10'!!MISSING!'                                 
         B     CONV18                                                           
*                                                                               
CONV17B  EQU   *                                                                
         MVC   RECNAME+50(10),=CL10'!!LENGTH! '                                 
         B     CONV18                                                           
*                                                                               
CONV17C  EQU   *                                                                
         MVC   RECNAME+50(10),=CL10'!!DATA!   '                                 
         B     CONV18                                                           
*                                                                               
CONV18   EQU   *                                                                
         MVC   P(L'RECNAME),RECNAME                                             
         GOTO1 VPRINTER                                                         
         B     CONV90                                                           
*                                                                               
CONV20   CLI   CTIKTYP,CT5KTYPQ    TEST ID RECORDS                              
         BNE   CONV90                                                           
         MVC   RECNAME,SPACES                                                   
         MVC   RECNAME(9),=C'ACCESSID='                                         
         MVC   RECNAME+9(L'CT5KALPH),CTIREC+(CT5KALPH-CT5REC)                   
*                                                                               
CONV40   LA    R4,CTIDATA                                                       
         SR    R5,R5                                                            
         USING CTSYSD,R4                                                        
CONV50   CLI   CTSYSEL,0           TEST E-O-R                                   
         BE    CONV90                                                           
         CLI   CTSYSEL,CTSYSELQ    TEST SYSTEM ELEMENT                          
         BNE   CONV80                                                           
*                                                                               
         LA    R3,CONVTAB                                                       
CONV60   CLI   0(R3),0                                                          
         BE    CONV80                                                           
         CLC   CTSYSSE,0(R3)                                                    
         BNE   CONV70                                                           
         CLC   CTSYSAGB,1(R3)                                                   
         BNE   CONV70                                                           
         MVC   P(L'RECNAME),RECNAME                                             
         MVC   P+25(7),=C'BEFORE='                                              
         XOUT  CTSYSSE,P+32,3                                                   
         MVC   P+38(7),=C',AFTER='                                              
         MVC   CTSYSSE,2(R3)                                                    
         XOUT  CTSYSSE,P+45,3                                                   
         GOTO1 VPRINTER                                                         
         B     CONV80                                                           
CONV70   LA    R3,L'CONVTAB(R3)                                                 
         B     CONV60                                                           
*                                                                               
CONV80   IC    R5,CTSYSLEN                                                      
         AR    R4,R5                                                            
         B     CONV50                                                           
*                                                                               
CONV90   EQU   *                                                                
* ??     PUT   TAPEOUT,IOL                                                      
         B     CONV02                                                           
*                                                                               
CONVX    CLOSE (TAPEOUT)                                                        
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
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
RECNAME  DC    CL62' '                                                          
         SPACE 1                                                                
CONVTAB  DS    0XL3                                                             
         DC    AL1(MED6,M#CDP,MEDR)                                             
         DC    AL1(MED6,M#MSL,MEDR)                                             
         DC    AL1(ACC6,A#CDP,ACCR)                                             
         DC    AL1(ACC6,A#MSL,ACCR)                                             
         DC    AL1(ACCJ,A#KLB,ACCM)                                             
         DC    AL1(ACCJ,A#MMS,ACCM)                                             
         DC    AL1(MEDJ,M#KHRT,MEDM)                                            
         DC    AL1(MEDJ,M#PUB,MEDM)                                             
         DC    AL1(MEDJ,M#PUBFOC,MEDM)                                          
         DC    AL1(MEDJ,M#PUBOPT,MEDM)                                          
         DC    AL1(MEDJ,M#MMSOPT,MEDM)                                          
         DC    AL1(MEDJ,M#FMC,MEDM)                                             
         DC    AL1(MEDJ,M#KLB,MEDM)                                             
CONVTABX DC    AL1(0)                                                           
         SPACE 1                                                                
ACC6     EQU   X'26'                                                            
ACCJ     EQU   X'33'                                                            
ACCR     EQU   X'3B'                                                            
ACCM     EQU   X'36'                                                            
MEDJ     EQU   X'48'                                                            
MEDM     EQU   X'4B'                                                            
MED6     EQU   X'20'                                                            
MEDR     EQU   X'50'                                                            
M#CDP    EQU   X'70'                                                            
M#MSL    EQU   X'30'                                                            
M#KHRT   EQU   X'10'                                                            
M#PUB    EQU   X'20'                                                            
M#PUBFOC EQU   X'30'                                                            
M#PUBOPT EQU   X'70'                                                            
M#MMSOPT EQU   X'80'                                                            
M#FMC    EQU   X'A0'                                                            
M#KLB    EQU   X'E0'                                                            
A#CDP    EQU   X'92'                                                            
A#MSL    EQU   X'A9'                                                            
A#KLB    EQU   X'44'                                                            
A#MMS    EQU   X'51'                                                            
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
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
WORK     DS    XL256                                                            
IOL      DS    F                                                                
IO       DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025CTCONVIDS 07/10/98'                                      
         END                                                                    
