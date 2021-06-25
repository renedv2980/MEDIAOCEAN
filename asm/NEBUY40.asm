*          DATA SET NEBUY40    AT LEVEL 072 AS OF 03/03/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T31140C                                                                  
*&&ONLIN SET   Y                                                                
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 071 05DEC19 <SPEC-39994> ALLOW UNITS TO USE THE COS2 FACTOR    *         
*                               FROM THE PACKAGE                      *         
* VGUP 071 23DEC19 <SPEC-36941> ADDED MIRROR CODE Q AND R             *         
***********************************************************************         
         TITLE 'T31140 - BUY CABLE TRANSFER'                                    
T31140   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CABTR*,RA,RR=RE                                              
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING T31140+8192,RC                                                   
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
*                                                                               
         STCM  RE,15,WORK          MYRELO                                       
         STCM  R1,15,WORK+4        MYPARM                                       
*                                                                               
         L     R7,AOVWORK                                                       
         USING WORKD,R7                                                         
         MVI   UPCURTWA,2          READ TWA2 INTO TIA                           
         BAS   RE,READTW2                                                       
*                                                                               
         MVC   MYRELO,WORK                                                      
         MVC   MYPARM,WORK+4                                                    
*                                                                               
         MVI   UPCURTWA,3          READ TWA3 INTO TIA                           
         BAS   RE,READTWA                                                       
         L     R4,UPACUROB         R4=A(TIA)                                    
         SPACE                                                                  
         PRINT NOGEN                                                            
*                                                                               
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
*******  MVC   TWWSSVR,CWSSVR-COMFACSD(RF)                                      
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB                                                       
*                                                                               
         CLC   12(4,R4),=CL4'HDR*'                                              
         BNE   MAIN10                                                           
*                                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   TWERROR,INVERR      ERROR IN FILE HEADER EXIT                    
         BNE   MAIN10                                                           
         BAS   RE,WRTTWA           WRITE TWA BLOCK BACK                         
         B     MAIN20                                                           
*                                                                               
*****    BAS   RE,RESWSSVR         RESTORE STORAGE USING WSSVR                  
MAIN10   BAS   RE,PROCESS          READ TAPE WRITE RECORDS                      
         SPACE 2                                                                
***********************************************************************         
* END OF PROCESSING                                                   *         
***********************************************************************         
MAIN20   DS    0H                                                               
*  SAVE LOCAL STORAGE IN WSSVR BLOCK                                            
*****    LA    R5,WORKAREA                                                      
*****    USING FAWSSVRD,R5                                                      
*****    XC    WORKAREA,WORKAREA                                                
*****    MVC   FAWSTOKN,=CL4'EDIW'                                              
*****    MVI   FAWSACTN,FAWSUSVE                                                
*****    LA    RE,WSSVRLEN                                                      
*****    STH   RE,FAWSLEN                                                       
*****    LA    RE,TWTYPE                                                        
*****    ST    RE,FAWSADR                                                       
*****    GOTO1 TWWSSVR,FAWSSVRD                                                 
*****    CLI   TWDUMP,C'Y'                                                      
*****    BNE   *+6                                                              
*****    DC    H'0'                                                             
*****    CLI   FAWSRTN,0                                                        
*****    BE    *+6                                                              
*****    DC    H'0'                                                             
*****    DROP  R5                                                               
*                                                                               
         XC    WORKAREA,WORKAREA                                                
         LA    R1,WORKAREA         CALL GLOBBER WITH TRANSFER CONTROL           
         USING GLVXFRSY,R1         ELEMENT                                      
         MVC   GLVXFRSY,=C'NET'    FROM SPOT  BUY                               
         MVC   GLVXFRPR,=C'NBU'                                                 
         MVC   GLVXTOSY,=C'CON'    TO CONTROL $MAD                              
         MVC   GLVXTOPR,=C'MAD'                                                 
         OI    GLVXFLG1,GLV1RETN+GLV1RETG   RETURN                              
         DROP  R1                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORKAREA,24,GLVXCTL                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'INIT' INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                        
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         LA    RE,SCRATCH          A(SCRATCH WORK SPACE)                        
         LA    RF,LSCRATCH         L(SCRATCH WORK SPACE)                        
         XCEF  (RE),(RF)           INITIALIZE THE AREA                          
*                                                                               
*****    L     RF,ACOMFACS                                                      
*****    MVC   TWWSSVR,CWSSVR-COMFACSD(RF)                                      
*                                                                               
*                                                                               
*  DELETE WSSVR BLOCK                                                           
*****    LA    R5,WORKAREA                                                      
*****    USING FAWSSVRD,R5                                                      
*****    XC    WORKAREA,WORKAREA                                                
*****    MVC   FAWSTOKN,=CL4'EDIW'                                              
*****    MVI   FAWSACTN,FAWSUDEL                                                
*****    GOTO1 TWWSSVR,FAWSSVRD                                                 
*****    DROP  R5                                                               
*                                                                               
         L     R4,UPACUROB         SET A(CURRENT OBJECT)                        
         USING NHDRD,R4                                                         
         CLC   NHDRLEN,=X'FFFF'                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R5,R5                                                            
         ICM   R5,3,NHDRLEN                                                     
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   NHDRTYPE,=C'HDR*'   FIRST OBJECT MUST BE HDR                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAGY,NHDRAGID                                                   
*                                                                               
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),(R9),RR=MYRELO   VALAGY            
         CLI   TWERROR,0                                                        
         BNE   INIT100                                                          
         GOTO1 =A(OVFLRTN),DMCB,(5,DUB),(RC),(R9),RR=MYRELO   BILDOV            
         B     INX                                                              
INIT100  MVC   NHDRERNO+1(1),TWERROR                                            
         MVI   NHDRERF,3                                                        
         B     INX                                                              
*                                                                               
INX      B     XIT                                                              
         DROP  R4                                                               
*                                                                               
* 'INIT' INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                        
* INITIALIZATION.                                                               
*                                                                               
RESWSSVR NTR1                                                                   
*****    CLC   12(4,R4),=CL4'HDR*'  DON'T RESTORE ON HEADER                     
*****    BE    RESWSSEX                                                         
*                                                                               
*  RESTORE WSSVR BLOCK                                                          
*****    LA    R5,WORKAREA                                                      
*****    USING FAWSSVRD,R5                                                      
*****    XC    WORKAREA,WORKAREA                                                
*****    MVC   FAWSTOKN,=CL4'EDIW'                                              
*****    MVI   FAWSACTN,FAWSURST                                                
*****    LA    RE,TWTYPE                                                        
*****    ST    RE,FAWSADR                                                       
*****    GOTO1 TWWSSVR,FAWSSVRD                                                 
*****    CLI   FAWSRTN,0                                                        
*****    BE    *+6                                                              
*****    DC    H'0'                                                             
***** RESWSSEX B     XIT                                                        
*****    DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET NEXT OBJECT                                                     *         
***********************************************************************         
NEXT     NTR1                                                                   
NEXT1    SR    R1,R1                                                            
         L     R4,UPACUROB                                                      
         ICM   R1,3,0(R4)          ADVANCE TO NEXT OBJECT                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RE,2(R1,R4)                                                      
         CLC   14(4,RE),=CL4'PROG'                                              
         BE    NEXT2                                                            
         CLC   14(4,RE),=CL4'UNIT'                                              
         BE    NEXT2                                                            
         CLC   14(4,RE),=CL4'UNUP'                                              
         BNE   *+8                                                              
NEXT2    LA    RE,2(RE)                                                         
         ST    RE,UPACUROB                                                      
         LR    R4,RE                                                            
*                                                                               
         CLC   0(2,R4),=X'FFFF'    TEST END OF TWA                              
         BNE   NEXT5                                                            
         BAS   RE,WRTTWA           YES-WRITE CURRENT TWA                        
         ZIC   R1,UPCURTWA                                                      
         LA    R1,1(R1)                                                         
         STC   R1,UPCURTWA                                                      
         BAS   RE,READTWA          GET NEXT TEMPSTR PAGE                        
         L     R4,UPACUROB                                                      
*                                                                               
NEXT5    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET TEMPSTOR PAGE                                                   *         
***********************************************************************         
         SPACE 1                                                                
READTWA  NTR1  ,                                                                
         CLI   UPCURTWA,10         DON'T READ BEYOND TWA10                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         IC    R4,UPCURTWA         READ TWA3 INTO TIA                           
         SLL   R4,24                                                            
         ICM   R4,3,TERM           TERMINAL NUMBER                              
         MVC   DMCB+20(2),=C'L='   LARGE TEMPSTR SIZE                           
         MVC   DMCB+22(2),LENTWA                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,(R4),ATIA                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,ATIA                                                          
         CLC   14(4,RE),=CL4'PROG'                                              
         BE    READTW80                                                         
         CLC   14(4,RE),=CL4'UNUP'                                              
         BE    READTW80                                                         
         CLC   14(4,RE),=CL4'UNIT'                                              
         BNE   READTWX                                                          
READTW80 LA    RE,2(RE)                                                         
*                                                                               
READTWX  ST    RE,UPACUROB                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET TEMPSTOR FOR LOACL STORAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
READTW2  NTR1  ,                                                                
         CLI   UPCURTWA,10         DON'T READ BEYOND TWA10                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         IC    R4,UPCURTWA         READ TWA3 INTO TIA                           
         SLL   R4,24                                                            
         ICM   R4,3,TERM           TERMINAL NUMBER                              
         MVC   DMCB+20(2),=C'L='   LARGE TEMPSTR SIZE                           
         MVC   DMCB+22(2),=XL2'7D0'                                             
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,(R4),AOVWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* WRITE TIA BACK TO TEMPSTOR PAGE                                     *         
***********************************************************************         
         SPACE 1                                                                
WRTTWA   NTR1  ,                                                                
         IC    R4,UPCURTWA                                                      
         SLL   R4,24                                                            
         ICM   R4,3,TERM           TERMINAL NUMBER                              
         GOTO1 VDATAMGR,DMCB,DMWRITE,TEMPSTR,(R4),ATIA                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* WRITE LOCAL BACK TO TWA                                             *         
***********************************************************************         
         SPACE 1                                                                
WRTTW2   NTR1  ,                                                                
         IC    R4,UPCURTWA                                                      
         SLL   R4,24                                                            
         ICM   R4,3,TERM           TERMINAL NUMBER                              
         GOTO1 VDATAMGR,DMCB,DMWRITE,TEMPSTR,(R4),AOVWORK                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
* 'PROCSTRT' PROCESSES THE START MODE.  IT PROCESSES THE REQUEST FOR            
* DEMOGRAPHIC INFORMATION FROM THE PC AND RETURNS THE FIRST SCREENFUL           
* OF INFORMATION.  IT ALSO SETS THE INTERNAL CONTROL TO BEGIN THE               
* SEND OF SUBSEQUENT DATA.                                                      
*                                                                               
PROCESS  NTR1                                                                   
*                                                                               
         BAS   RE,CHKSTORE          CHECK BUYVALS STILL EXIST                   
         CLC   12(4,R4),=CL4'HDR*'                                              
         BNE   PROC050                                                          
         MVC   TWTYPE,17(R4)                                                    
*                                                                               
PROC020  BAS   RE,NEXT                                                          
         L     R4,UPACUROB                                                      
*                                                                               
PROC050  MVI   TWERROR,0                                                        
         CLC   12(4,R4),=CL4'DEAL'                                              
         BE    PROC100                                                          
         CLC   12(4,R4),=CL4'PKG*'                                              
         BE    PROC120                                                          
         CLC   12(4,R4),=CL4'PROG'                                              
         BE    PROC140                                                          
         CLC   12(4,R4),=CL4'PDRE'                                              
         BE    PROC160                                                          
         CLC   12(4,R4),=CL4'PDRA'                                              
         BE    PROC180                                                          
         CLC   12(4,R4),=CL4'EPGM'                                              
         BE    PROC020                                                          
         CLC   12(4,R4),=CL4'UNIT'                                              
         BE    PROC200                                                          
         CLC   12(4,R4),=CL4'UDRE'                                              
         BE    PROC220                                                          
         CLC   12(4,R4),=CL4'UDRA'                                              
         BE    PROC240                                                          
         CLC   12(4,R4),=CL4'UPSC'                                              
         BE    PROC260                                                          
         CLC   12(4,R4),=CL4'PROD'                                              
         BE    PROC280                                                          
         CLC   12(4,R4),=CL4'CMMT'                                              
         BE    PROC300                                                          
         CLC   12(4,R4),=CL4'COMM'                                              
         BE    PROC320                                                          
         CLC   12(4,R4),=CL4'DEL*'                                              
         BE    PROC340                                                          
         CLC   12(4,R4),=CL4'UNUP'                                              
         BE    PROC360                                                          
         CLC   12(4,R4),=CL4'EUNT'                                              
         BE    PROC380                                                          
         CLC   12(4,R4),=CL4'EPKG'                                              
         BE    PROC400                                                          
         CLC   12(4,R4),=CL4'EOD*'                                              
         BE    PROC400                                                          
         CLC   12(4,R4),=CL4'EOF*'                                              
         BE    PROC420                                                          
         DC    H'0'                                                             
*                                                                               
PROC100  BAS   RE,DODEAL           HANDLE DEAL RECORD                           
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC120  BAS   RE,DOPKG            HANDLE PACKAGE RECORD                        
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC140  BAS   RE,DOPROG           HANDLE PROGRAM RECORD                        
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC160  BAS   RE,DOPDRE           HANDLE PROGRAM EST. DEMOS                    
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC180  BAS   RE,DOPDRA           HANDLE PROGRAM ACT. DEMOS                    
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC200  OC    TWMKT,TWMKT         CHECK FOR BAD UPLOAD DATA                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DOUNIT           HANDLE UNIT RECORD                           
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC220  BAS   RE,DOUDRE           HANDLE DEMO OVERRIDES ESTIMATED              
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC240  BAS   RE,DOUDRA           HANDLE DEMO OVERRIDES ACTUAL                 
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC260  BAS   RE,DOSPCH           HANDLE SPECIAL CHARGES                       
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC280  BAS   RE,DOPROD           HANDLE PRODUCT                               
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC300  BAS   RE,DOCMMT           HANDLE COMMENTS                              
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC320  BAS   RE,DOCOMM           HANDLE COMMERCIALS                           
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC340  BAS   RE,DODEL            HANDLE DELETE OF UNIT                        
         L     R4,UPACUROB                                                      
         B     PROC020                                                          
*                                                                               
PROC360  GOTO1 =A(OVFLRTN2),DMCB,(0,DUB),(RC),(R9),RR=MYRELO   DOUPUNIT         
         L     R4,UPACUROB                                                      
         USING NCHUD,R4                                                         
*  CHECK RETURN ERRORS                                                          
         CLI   NCHUERF,0                                                        
         BE    PROC020                                                          
         DROP  R4                                                               
*                                                                               
* ERROR CONDITION BYPASS UNIT RECORD UNTIL HIGHER RECORED                       
*                                                                               
         MVI   TWERROR,INVERR                                                   
PROC365  BAS   RE,NEXT                                                          
         L     R4,UPACUROB                                                      
         CLC   12(4,R4),=CL4'UNIT'                                              
         BE    PROC370                                                          
         CLC   12(4,R4),=CL4'UNUP'                                              
         BE    PROC370                                                          
         CLC   12(4,R4),=CL4'DEAL'                                              
         BE    PROC370                                                          
         CLC   12(4,R4),=CL4'PKG*'                                              
         BE    PROC370                                                          
         CLC   12(4,R4),=CL4'EPKG'                                              
         BE    PROC370                                                          
         CLC   12(4,R4),=CL4'EOD*'                                              
         BE    PROC370                                                          
         CLC   12(4,R4),=CL4'EOF*'                                              
         BE    PROC370                                                          
         B     PROC365                                                          
PROC370  B     PROC050                                                          
*                                                                               
PROC380  BAS   RE,DOEUNT           HANDLE END OF UNIT                           
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC400  BAS   RE,DOEPKG           HANDLE END OF PACKAGE                        
         L     R4,UPACUROB                                                      
         B     PROC020                                                          
*                                                                               
PROC420  BAS   RE,DOEOF            HANDLE EOF RECORD                            
         L     R4,UPACUROB                                                      
*                                                                               
PROCEX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK BUYVALS STILL EXIST                                           *         
***********************************************************************         
CHKSTORE NTR1                                                                   
         L     R5,AIOAREA4                                                      
         USING BUYVALD,R5                                                       
         ST    R5,ABUYVALS                                                      
*                                                                               
         OC    AGYMED,AGYMED                                                    
         BNZ   CHKST50                                                          
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),(R9),RR=MYRELO   VALAGY            
*                                                                               
CHKST50  OC    TWCLI,TWCLI                                                      
         BZ    CHKSTEX                                                          
*                                                                               
         GOTO1 VCLPACK,DMCB,TWCLI,FULL                                          
*                                                                               
         CLC   CLIPK,FULL                                                       
         BE    CHKSTEX              BUYVAL STILL VALID EXIST                    
*                                                                               
         GOTO1 =A(OVFLRTN),DMCB,(1,DUB),(RC),(R9),RR=MYRELO   VALCLI            
         GOTO1 =A(OVFLRTN),DMCB,(4,DUB),(RC),(R9),RR=MYRELO   GETSTA            
         GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(RC),(R9),RR=MYRELO   VALEST            
CHKSTEX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* HANDLE DEAL RECORD                                                  *         
* R4=ADDRESS OF DEAL RECORD                                                     
***********************************************************************         
DODEAL   NTR1                                                                   
         USING NDLD,R4                                                          
         PRINT GEN                                                              
*                                                                               
DEAL020  MVI   TWERROR,NETERR                                                   
         OC    NDLNET,NDLNET       CHECK NETWORK INPUTTED                       
         BZ    DEAL030                                                          
         MVC   TWCLI,NDLCLT        CLIENT                                       
         MVC   TWNET,NDLNET        NETWORK                                      
         MVC   TWEDF,NDLEDF        ESTIMATE DEMO FORM                           
         MVC   TWADF,NDLADF        ACTUAL DEMO FORM                             
         MVC   TWACT,NDLDEL        ACTION                                       
         MVC   TWDEAL,NDLDEAL      DEAL #                                       
*                                                                               
         GOTO1 =A(OVFLRTN),DMCB,(1,DUB),(RC),(R9),RR=MYRELO   VALCLI            
         CLI   TWERROR,0                                                        
         BE    *+18                                                             
DEAL030  MVC   NDLERNO+1(1),TWERROR                                             
         MVI   NDLERF,4                                                         
         B     DEAL500                                                          
*                                                                               
         GOTO1 =A(OVFLRTN),DMCB,(4,DUB),(RC),(R9),RR=MYRELO   GETSTA            
         CLI   TWERROR,0                                                        
         BE    *+18                                                             
         MVC   NDLERNO+1(1),TWERROR                                             
         MVI   NDLERF,3                                                         
         B     DEAL500                                                          
*                                                                               
         MVI   NDLERF,6                                                         
         MVI   NDLERNO+1,INVERR                                                 
         CLI   TWEDF,C'R'                                                       
         BE    DEAL050                                                          
         CLI   TWEDF,C'I'                                                       
         BE    DEAL050                                                          
         CLI   TWEDF,C'V'                                                       
         BNE   DEAL500                                                          
*                                                                               
DEAL050  MVI   NDLERF,7                                                         
         CLI   TWEDF,C'R'                                                       
         BE    DEAL100                                                          
         CLI   TWEDF,C'I'                                                       
         BE    DEAL100                                                          
         CLI   TWEDF,C'V'                                                       
         BNE   DEAL500                                                          
*                                                                               
******** BAS   RE,SETDEMO                                                       
DEAL100  GOTO1 =A(OVFLRTN),DMCB,(8,DUB),(RC),(R9),RR=MYRELO   SETDEMO           
         CLI   TWERROR,0                                                        
         BE    *+18                                                             
         MVC   NDLERNO+1(1),TWERROR                                             
         MVI   NDLERF,9                                                         
         B     DEAL500                                                          
*                                                                               
         B     DEALX                                                            
*                                                                               
*--IF ERROR IN DEAL RECORD BYPASS ALL RECORDS UNDER                             
*--THIS DEAL. READ TEMPSTOR UNTIL NEXT DEAL RECORD                              
*--IS FOUND.IF EOF RECORD IS FOUND FIRST THEN EXIT.                             
DEAL500  MVI   TWERROR,INVERR                                                   
DEAL520  BAS   RE,NEXT                                                          
         L     R4,UPACUROB                                                      
         CLC   12(4,R4),=CL4'DEAL'                                              
         BE    DEAL020                                                          
         CLC   12(4,R4),=CL4'EOD*'                                              
         BE    XIT                                                              
         CLC   12(4,R4),=CL4'EOF*'                                              
         BE    XIT                                                              
         B     DEAL520                                                          
*                                                                               
DEALX    MVI   TWERROR,0                                                        
         MVC   NDLERNO(3),=XL3'000000'                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*--CHECK THAT ALL DEMOS RESIDE IN THE ESTIMATE HEADER                           
****DEMEST   NTR1                                                               
*                                                                               
****         MVI   TWERROR,INVERR                                               
****         L     R5,AIOAREA4                                                  
****         USING BUYVALD,R5                                                   
****         LA    RE,TWDEMOS                                                   
****DMEST050 LA    RF,TWESDEMS                                                  
****         ZIC   R1,TWESNDEM                                                  
*                                                                               
****DMEST100 OC    0(3,RF),0(RF)       END OF EST LIST                          
****         BZ    XIT                                                          
****         CLC   0(1,RE),0(RF)                                                
****         BNE   DMEST150                                                     
****         CLC   2(1,RE),2(RF)                                                
****         BE    DMEST200                                                     
****DMEST150 LA    RF,3(RF)                                                     
****         BCT   R1,DMEST100                                                  
*                                                                               
****DMEST200 LA    RE,3(RE)                                                     
****         CLI   0(RE),X'FF'                                                  
****         BNE   DMEST050                                                     
*                                                                               
****         MVI   TWERROR,0                                                    
****         B     XIT                                                          
****         DROP  R4,R5                                                        
         EJECT                                                                  
*                                                                               
* BUILD PACKAGE RECORD                                                          
*                                                                               
DOPKG    NTR1                                                                   
         USING NPKGD,R4            PACKAGE DSECT                                
*                                                                               
         MVI   TWRITESW,C'N'                                                    
*                                                                               
         XC    WORKAREA+100(100),WORKAREA+100                                   
         LA    R5,WORKAREA+100                                                  
         USING UNBLOCKD,R5                                                      
*                                                                               
*-VALIDATE AND BUILD PACKAGE RECORD                                             
*                                                                               
         L     R3,AIOAREA2                                                      
         USING NPRECD,R3                                                        
*                                                                               
DOPKG010 MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,TWAGYMED                                                   
         MVC   NPKRLEN,=XL2'0058'                                               
*--SET 01 ELEMENT                                                               
         MVC   NPAKEL(2),=XL2'013C'                                             
*--CALCULATE CLIENT                                                             
         MVC   NPKCLT,TWCLIENT                                                  
*--CALCULATE NETWORK                                                            
         MVC   NPKNET,TWNET                                                     
*--CALCULATE ESTIMATE                                                           
         PACK  DUB,NPKGEST(3)                                                   
         CVB   RE,DUB                                                           
         STCM  RE,1,TWEST                                                       
         MVC   NPKEST,TWEST                                                     
*                                                                               
         GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(RC),(R9),RR=MYRELO   VALEST            
         CLI   TWERROR,0                                                        
         BE    DOPKG050                                                         
         MVC   NPKGERNO+1(1),TWERROR                                            
         MVI   NPKGERF,1                                                        
         B     DOPKG500                                                         
*--CALCULATE PACKAGE CODE                                                       
DOPKG050 MVI   TWERROR,INVERR                                                   
         PACK  DUB,NPKGPKG(3)                                                   
         CVB   RE,DUB                                                           
         STCM  RE,1,TWPKG                                                       
         CH    RE,=H'0'                                                         
         BE    *+12                                                             
         CH    RE,=H'255'                                                       
         BNH   DOPKG070                                                         
         MVC   NPKGERNO+1(1),TWERROR                                            
         MVI   NPKGERF,2                                                        
         B     DOPKG500                                                         
DOPKG070 MVC   NPKPACK,TWPKG                                                    
*--CALCULATE PACKAGE NAME                                                       
         MVC   NPAKNAME,NPKGDSC    PACKAGE NAME                                 
*--CALCULATE DAYPART                                                            
         CLI   NPKGDPT,X'40'                                                    
         BNH   DOPKG080                                                         
         EXPDP WORKAREA,NPKGDPT                                                 
         CLC   WORKAREA(7),=CL7'UNKNOWN'                                        
         BNE   *+18                                                             
         MVC   NPKGERNO+1(1),TWERROR                                            
         MVI   NPKGERF,4                                                        
         B     DOPKG500                                                         
         MVC   TWDYPT,NPKGDPT                                                   
         MVC   NPAKDP,TWDYPT                                                    
*--CALCULATE PACKAGE COST                                                       
DOPKG080 CLI   NPKGCOST,X'40'                                                   
         BE    DOPKG090                                                         
         PACK  DUB,NPKGCOST(8)                                                  
         CVB   RE,DUB                                                           
         STCM  RE,15,NPAKCOST                                                   
*--CALCULATE UNIVERSE                                                           
DOPKG090 MVC   TWUNIVL,NPKGUNV                                                  
         CLI   NPKGUNV,X'40'                                                    
         BE    DOPKG100                                                         
         XC    WORKAREA(20),WORKAREA                                            
         MVI   WORKAREA,12                                                      
         MVI   WORKAREA+4,X'08'                                                 
         MVC   WORKAREA+8(4),NPKGUNV                                            
         LA    R2,WORKAREA                                                      
         GOTO1 VGETFLD                                                          
*                                                                               
         MVI   UNEDATA,UUNCD                                                    
         MVC   UNALOCAL,AIOAREA3                                                
         ST    R9,UNAGLOB                                                       
         GOTO1 VEDIT,DMCB,(C'S',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    *+18                                                             
         MVC   NPKGERNO+1(1),UNERROR                                            
         MVI   NPKGERF,6                                                        
         B     DOPKG500                                                         
*                                                                               
         MVC   TWUNIV,DUB+5        CONVERTED UNIVERSE NUMBER                    
         MVC   NPAKUNCD,TWUNIV                                                  
*--CALCULATE SPECIAL REP                                                        
DOPKG100 MVC   TWSREPL,NPKGREP                                                  
         CLI   NPKGREP,X'40'                                                    
         BE    DOPKG120                                                         
         XC    WORKAREA(20),WORKAREA                                            
         MVI   WORKAREA,11                                                      
         MVI   WORKAREA+4,X'08'                                                 
         MVC   WORKAREA+8(3),NPKGREP                                            
         LA    R2,WORKAREA                                                      
         GOTO1 VGETFLD                                                          
*                                                                               
         STCM  R0,3,TWSREP                                                      
         MVC   NPAKSREP,TWSREP                                                  
*                                                                               
         MVI   UNEDATA,USREP                                                    
         MVC   UNALOCAL,AIOAREA3                                                
         ST    R9,UNAGLOB                                                       
         GOTO1 VEDIT,DMCB,(C'S',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    *+18                                                             
         MVC   NPKGERNO+1(1),UNERROR                                            
         MVI   NPKGERF,7                                                        
         B     DOPKG500                                                         
*--CALCULATE HUT TYPE                                                           
DOPKG120 MVI   NPAKHTYP,C'A'                                                    
*--CALCULATE POST DATA TYPE                                                     
         MVI   NPAKPDT,C'D'                                                     
*--CALCULATE HUT SCHEME                                                         
         CLI   BUYPROF,STAR                                                     
         BE    *+16                                                             
         MVC   NPAKHUTS,BUYPROF                                                 
         MVC   TWHUTSCH,BUYPROF                                                 
*--CALCULATE HUT STATUS                                                         
         MVI   NPAKHUTL,0          INIT THE FIELD                               
         OI    NPAKHUTL,X'80'      GET HUTS FROM DEMO FILE                      
         CLI   BUYPROF+3,C'Y'                                                   
         BNE   *+8                                                              
         OI    NPAKHUTL,X'40'      USE 52 WEEK HUT SCHEDULE                     
         MVC   TWHUTST,NPAKHUTL                                                 
*--SET CABLE UPLOAD INDICATOR                                                   
         OI    NPAKCNTL,X'10'                                                   
*--SET NO UNITS INDICATOR                                                       
         OI    NPAKCNTL,X'20'                                                   
*--CALCULATE DEMO TYPE                                                          
         CLI   TWPSTTYP,C'C'       PROFILE ONLY FOR CABLE                       
         BNE   DOPKG200                                                         
         CLI   BUYPROF+9,C'X'      NO DEFAULT ALLOWED                           
         BE    DOPKG200                                                         
         MVC   FLD(1),BUYPROF2+9   IF NO INPUT USE PROFILE                      
*                                                                               
         CLI   FLD,C'V'                                                         
         BE    DOPKG200                                                         
         CLI   FLD,C'I'                                                         
         BNE   DOPKG200                                                         
         OI    NPAKCNTL,X'40'                                                   
         MVC   TWPCNTRL,NPAKCNTL                                                
*--CALCULATE ACTIVITY AND DATE                                                  
DOPKG200 MVI   NPAKACTA,C'A'                                                    
         GOTO1 VDATCON,DMCB,(5,0),(2,NPAKACTD)                                  
*--CALCULATE LOCK OF PACKAGE                                                    
         CLC   TWTYPE,=CL3'MVU'                                                 
         BE    *+8                                                              
         OI    NPAKSTAT,X'20'                                                   
*--CHECK TO SEE IF RECORD EXISTS                                                
         GOTO1 =A(OVFLRTN),DMCB,(3,DUB),(RC),(R9),RR=MYRELO   VALPKG            
         CLI   TWERROR,0                                                        
         BE    DOPKGX                                                           
         B     DOPKG300                                                         
*        CLI   TWERROR,NOTFOUND                                                 
*        BNE   DOPKG300                                                         
*--ADD NEW PACKAGE RECORD (ONLY IF DAYPART AND NAME ARE PRESENT)                
*  DOPKG250 CLI   NPKGDPT,X'40'       DAYPART                                   
*        BNH   DOPKG300                                                         
*        CLC   NPKGDSC,SPACES      PACKAGE NAME                                 
*        BE    DOPKG300                                                         
*        GOTO1 AIO,DMCB,UNT+FILE+ADDREC,(R3)                                    
*        B     DOPKGX                                                           
*-SET PACKAGE VALIDATION ERROR                                                  
DOPKG300 MVC   NPKGERNO+1(1),TWERROR                                            
         MVI   NPKGERF,2                                                        
         B     DOPKG500                                                         
*                                                                               
*--IF ERROR IN PKG RECORD BYPASS ALL RECORDS UNDER                              
*--THIS PKG. READ TEMPSTOR UNTIL NEXT PKG RECORD                                
*--IS FOUND.IF EOF OR DEAL RECORD IS FOUND FIRST                                
*--EXIT.                                                                        
DOPKG500 MVI   TWERROR,INVERR                                                   
DOPKG520 BAS   RE,NEXT                                                          
         L     R4,UPACUROB                                                      
         CLC   12(4,R4),=CL4'XXXX'                                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   12(4,R4),=CL4'PKG*'                                              
         BE    DOPKG010                                                         
         CLC   12(4,R4),=CL4'DEAL'                                              
         BE    XIT                                                              
         CLC   12(4,R4),=CL4'EOD*'                                              
         BE    XIT                                                              
         CLC   12(4,R4),=CL4'EOF*'                                              
         BE    XIT                                                              
         B     DOPKG520                                                         
*                                                                               
DOPKGX   MVI   TWERROR,0                                                        
         XIT                                                                    
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
*                                                                               
*--BUILD A PROGRAM RECORD                                                       
*                                                                               
DOPROG NTR1                                                                     
*                                                                               
         USING NPGMD,R4            PROGRAM RECORD DSECT                         
*--INIT DEMO TABLES                                                             
         MVI   TWLSTLIN,200        FORCE PROGRAM LOOKUP                         
         MVI   TWPRINPC,C'N'                                                    
         MVI   TWDEMVE,X'FF'                                                    
         MVI   TWDEMVA,X'FF'                                                    
         MVI   TWSTAT,0                                                         
*--CHECK IF PROGRAM CODE INPUTTED                                               
         CLI   NPGMPGCD,X'40'                                                   
         BNH   *+14                                                             
         MVI   TWPRINPC,C'Y'                                                    
         MVC   TWPRCODE,NPGMPGCD                                                
*--SAVE PROGRAM NAME                                                            
         MVC   TWPRNAME,NPGMPROG                                                
         MVC   TWPRCODE,NPGMPGCD                                                
*--SAVE CONTRACT #                                                              
         MVC   TWCON,NPGMCON                                                    
*--CALCULATE PROGRAM LENGTH                                                     
         PACK  DUB,NPGMULN                                                      
         CVB   RE,DUB                                                           
         STCM  RE,1,TWPLEN                                                      
*--CALCULATE LENGTH TYPE                                                        
         MVC   TWPRLNTP,NPGMULNT                                                
         CLI   TWPRLNTP,C'S'                                                    
         BE    DOPRG020                                                         
         CLI   TWPRLNTP,C'M'                                                    
         BE    DOPRG020                                                         
         MVI   NPGMERNO+1,INVERR                                                
         MVI   NPGMERF,8                                                        
         B     DOPRG900                                                         
*--CALCULATE ACTUAL COST                                                        
DOPRG020 PACK  DUB,NPGMTRT(9)                                                   
         CVB   RE,DUB                                                           
         STCM  RE,15,TWPRACT                                                    
         OI    TWSTAT,X'20'                                                     
*--CALCULATE INTEGRATION COST                                                   
         XC    TWPRINT,TWPRINT                                                  
         CLI   NPGMIRT,X'40'                                                    
         BE    *+18                                                             
         PACK  DUB,NPGMIRT(8)                                                   
         CVB   RE,DUB                                                           
         STCM  RE,15,TWPRINT                                                    
*--CALCULATE ASSIGNED COST                                                      
         XC    TWPRASS,TWPRASS                                                  
         CLI   NPGMACST,X'40'                                                   
         BE    *+22                                                             
         PACK  DUB,NPGMACST(9)                                                  
         CVB   RE,DUB                                                           
         STCM  RE,15,TWPRASS                                                    
         OI    TWSTAT,X'08'                                                     
*--CALCULATE ESTIMATE                                                           
*        PACK  DUB,NPGMEST(3)                                                   
*        CVB   RE,DUB                                                           
*        STCM  RE,1,BYTE                                                        
*        MVC   TWEST,BYTE                                                       
*                                                                               
*        GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(RC),(R9),RR=MYRELO   VALEST            
*        CLI   TWERROR,0                                                        
*        BE    DOPRG040                                                         
*        MVC   NPGMERNO+1(1),TWERROR                                            
*        MVI   NPGMERF,1                                                        
*        B     DOPRG900                                                         
*--CALCULATE PACKAGE                                                            
* DOPRG040 PACK  DUB,NPGMPKG(3)                                                 
*        CVB   RE,DUB                                                           
*        STCM  RE,1,BYTE                                                        
*        MVC   TWPKG,BYTE                                                       
*                                                                               
*        GOTO1 =A(OVFLRTN),DMCB,(3,DUB),(RC),(R9),RR=MYRELO   VALPKG            
*        CLI   TWERROR,0                                                        
*        BE    DOPRG050                                                         
*        MVC   NPGMERNO+1(1),TWERROR                                            
*        MVI   NPGMERF,2                                                        
*        B     DOPRG900                                                         
*--CALCULATE RATE TYPES                                                         
DOPRG050 MVC   TWPRTRAT,NPGMTRQT                                                
         MVC   TWPRIRAT,NPGMIRQ                                                 
*                                                                               
*--CALCULATE PROGRAM CODE                                                       
         CLI   TWPRINPC,C'Y'       NAME ALREADY INPUTTED                        
         BE    DOPRG300                                                         
         MVI   TWPRCODE+3,X'FF'                                                 
         XC    TWPRCODE(3),TWPRCODE                                             
         LA    RE,TWPRNAME                                                      
         LA    RF,TWPRCODE                                                      
         LA    R1,16                                                            
*                                                                               
*--FIRST AREA CHECKS FOR BYPASS NAMES                                           
DOPRG060 LA    R2,NAMETAB                                                       
DOPRG070 CLI   0(R2),X'FF'         END OF NAME TABLE                            
         BE    DOPRG100            YES CHECK FOR VOWELS                         
         ZIC   R6,8(R2)            MOVE IN LENGTH OF COMPARE                    
         EX    R6,NAMECOMP                                                      
         BNE   DOPRG080                                                         
         LA    R6,1(R6)                                                         
         AR    RE,R6               BUMP PROGRAM NAME TO NEXT WORD               
         SR    R1,R6               SUBTRACT LOOP COUNT BY WORD BUMP             
         LTR   R1,R1                                                            
         BZ    DOPRG260            END OF LITERAL EXIT ROUTINE                  
         BP    DOPRG060            CHECK NEXT WORD                              
         DC    H'0'                WENT PAST LITERAL                            
DOPRG080 LA    R2,9(R2)                                                         
         B     DOPRG070                                                         
*                                                                               
*--THIS AREA CHECKS FOR TO BYPASS VOWELS AND SPECIAL VALUES                     
DOPRG100 LA    R2,VOWELLST                                                      
DOPRG120 CLI   0(RE),X'40'         CHECK FOR WORD BREAK                         
         BNE   DOPRG140                                                         
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         BCTR  R1,0                SUBTRACT FROM COUNT                          
         LTR   R1,R1                                                            
         BZ    DOPRG260            END OF LITERAL EXIT ROUTINE                  
         B     DOPRG060            CHECK AGAINST WORD TABLE                     
*                                                                               
DOPRG140 CLI   0(RE),X'C1'         CHECK CHARACTER FOR ALPHA/NUM                
         BL    DOPRG180                                                         
         CLI   0(RE),X'F9'                                                      
         BH    DOPRG180                                                         
*                                                                               
DOPRG160 CLI   0(R2),X'FF'         END OF VOWEL TABLE                           
         BE    DOPRG200            MOVE LETTER TO CODE                          
         CLC   0(1,RE),0(R2)                                                    
         BE    DOPRG180            LETTER IS A VOWEL, BYPASS                    
         LA    R2,1(R2)                                                         
         B     DOPRG160                                                         
*                                                                               
DOPRG180 LA    RE,1(RE)                                                         
         BCT   R1,DOPRG100                                                      
         B     DOPRG260                                                         
*                                                                               
DOPRG200 CLI   0(RF),X'FF'                                                      
         BE    DOPRG260            CODE FIELD IS FILLED                         
         MVC   0(1,RF),0(RE)       MOVE NEXT LETTER OF CODE IN                  
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,DOPRG100                                                      
*                                                                               
*--THE LITERAL PORTION OF THE PROGRAM CODE MUST BE                              
*--1 POSITIONS LONG. IF IT IS NOT AN ERROR CODE IS                              
*--AND THE PROGRAM BYPASSES THE UNITS UNDER THAT CODE.                          
DOPRG260 MVC   TWPRCODE+3(3),=XL3'404040'                                       
         CLI   TWPRCODE,X'00'                                                   
         BNE   DOPRG280                                                         
         MVI   NPGMERNO+1,INVERR   INVALID PROGRAM NAME                         
         MVI   NPGMERF,6                                                        
         B     DOPRG900                                                         
*                                                                               
DOPRG280 MVC   TWPRCODE+3(3),=XL3'F0F0F0'                                       
         CLI   TWPRCODE+2,0                                                     
         BNE   DOPRG285                                                         
         MVI   TWPRCODE+2,X'F0'                                                 
         CLI   TWPRCODE+1,0                                                     
         BNE   DOPRG285                                                         
         MVI   TWPRCODE+1,X'F0'                                                 
         SPACE 3                                                                
*--NAMES THAT HAVE BEEN FILLED CONVERSION TABLE                                 
*--IF PROGRAM COUNT ALREADY UP TO 999 A CONVERSION IS NEEDED                    
DOPRG285 LA    RE,CONVTAB                                                       
DOPRG290 CLI   0(RE),X'FF'                                                      
         BE    DOPRG300                                                         
         CLC   TWPRCODE(3),0(RE)                                                
         BE    DOPRG295                                                         
         LA    RE,6(RE)                                                         
         B     DOPRG290                                                         
DOPRG295 MVC   TWPRCODE(3),3(RE)                                                
*--CALCULATE ROTATION                                                           
DOPRG300 XC    TWPRROT(4),TWPRROT  CLEAR THE DAY FIELDS                         
         MVC   TWPROTE,NPGMROT                                                  
         LA    R1,ROTTABLE                                                      
         LA    RE,NPGMROT                                                       
DOPRG320 CLI   0(R1),X'FF'                                                      
         BE    DOPRG350                                                         
         CLI   0(RE),C'Y'                                                       
         BNE   DOPRG340                                                         
         OC    TWPRROT,2(R1)                                                    
         NI    TWPRROTN,X'F0'      CLEAR END DAT NUMBER                         
         OC    TWPRROTN,5(R1)      END DAY NUMBER                               
         TM    TWPRROTN,X'F0'                                                   
         BNZ   DOPRG340                                                         
         NI    TWPRROTN,X'0F'      CLEAR START DAY NUMBER                       
         OC    TWPRROTN,4(R1)      START DAY NUMBER                             
         MVC   TWDAYNO,5(R1)      START DAY NUMBER (NUMERIC)                    
         MVC   TWDAYHEX,3(R1)     START DAY NUMBER (HEX)                        
DOPRG340 LA    R1,6(R1)                                                         
         LA    RE,1(RE)                                                         
         B     DOPRG320                                                         
*                                                                               
DOPRG350 CLI   TWDAYNO,0                                                        
         BNE   DOPRG360                                                         
         MVI   NPGMERNO+1,INVERR                                                
         MVI   NPGMERF,3                                                        
         B     DOPRG900                                                         
         SPACE 2                                                                
*--CALCULATE MIRROR                                                             
DOPRG360 XC    TWPRMIR,TWPRMIR                                                  
         CLI   NPGMMST,X'40'                                                    
         BE    *+18                                                             
         PACK  DUB(8),NPGMMST(4)                                                
         CVB   RE,DUB                                                           
         STCM  RE,3,TWPRMIR                                                     
*-CHECK MIRROR TIME AGAINST 2400                                                
         MVI  NPGMERNO+1,INVERR                                                 
         MVI  NPGMERF,11                                                        
         CLC  TWPRMIR,=XL2'0960'                                                
         BH   DOPRG900                                                          
*                                                                               
*--CALCULATE START AND END TIMES                                                
         PACK  DUB(8),NPGMSTM(4) START TIME                                     
         CVB   RE,DUB                                                           
         STCM  RE,3,TWPRSTIM                                                    
*                                                                               
         PACK  DUB(8),NPGMETM(4) END TIME                                       
         CVB   RE,DUB                                                           
         STCM  RE,3,TWPRETIM                                                    
*-CHECK START AND END FOR GREATER THEN 2400                                     
         MVI  NPGMERNO+1,INVERR                                                 
         MVI  NPGMERF,4                                                         
         CLC  TWPRETIM,=XL2'0960'                                               
         BH   DOPRG900                                                          
*                                                                               
         MVI  NPGMERF,5                                                         
         MVI  NPGMERNO+1,INVERR                                                 
         CLC  TWPRSTIM,=XL2'0960'                                               
         BH   DOPRG900                                                          
*-CHECK START AND END TIME FOR MINUTES LESS THEN 60                             
         MVI  NPGMERF,5                                                         
         SR   R0,R0                                                             
         SR   R1,R1                                                             
         SR   RE,RE                                                             
         LH   R1,TWPRETIM                                                       
         LA   RE,100                                                            
         DR   R0,RE                                                             
         CH   R0,=H'59'                                                         
         BH   DOPRG900                                                          
*                                                                               
         MVI  NPGMERF,4                                                         
         SR   R0,R0                                                             
         SR   R1,R1                                                             
         SR   RE,RE                                                             
         LH   R1,TWPRSTIM                                                       
         LA   RE,100                                                            
         DR   R0,RE                                                             
         CH   R0,=H'59'                                                         
         BH   DOPRG900                                                          
*-CHECK START AND END FOR ZERO                                                  
         MVI  NPGMERF,4                                                         
         OC   TWPRETIM,TWPRETIM                                                 
         BZ   DOPRG900                                                          
*                                                                               
         MVI  NPGMERF,5                                                         
         OC   TWPRSTIM,TWPRSTIM                                                 
         BZ   DOPRG900                                                          
*-GET MIRROR CODE                                                               
         MVI  TWPRMRCD,0           INITIALISE FIELD                             
         OC   TWPRMIR,TWPRMIR      WAS MIRROR INPUTTED                          
         BZ   DOPRG800             NOPE BYPASS CHECK                            
*                                                                               
         MVI  NPGMERF,11                                                        
         SR   RE,RE                                                             
         LH   RE,TWPRMIR           MIRROR START TIME                            
         CH   RE,=H'600'           CONVERT 12M-6AM                              
         BH   *+8                                                               
         LA   RE,2400(RE)                                                       
*                                                                               
         SR   RF,RF                                                             
         LH   RF,TWPRSTIM          PROGRAM START TIME                           
         CH   RF,=H'600'           CONVERT 12M-6AM                              
         BH   *+8                                                               
         LA   RE,2400(RE)                                                       
         SR   RE,RF                (MIRROR TIME - START TIME)                   
*                                                                               
         LA   RF,MIRRTBLE                                                       
DOPRG400 CLI  0(RF),X'FF'                                                       
         BE   DOPRG900                                                          
         CH   RE,0(RF)                                                          
         BE   DOPRG420                                                          
         LA   RF,4(RF)                                                          
         B    DOPRG400                                                          
DOPRG420 MVC  TWPRMRCD,2(RF)       MOVE OUT MIRROR CODE                         
*                                                                               
* CLEAR TIME ERRORS                                                             
DOPRG800 MVI  NPGMERNO+1,0                                                      
         MVI  NPGMERF,0                                                         
         B    DOPRGX                                                            
*                                                                               
*        CLI   TWPRINPC,C'Y'                                                    
*        BNE   DOPRGX                                                           
*                                                                               
*        BAS   RE,CHKPROG          CODE EXISTS CHECK AGAINST REC                
*        CLI   TWERROR,0                                                        
*        BE    DOPRGX                                                           
*        B     DOPRG900                                                         
*                                                                               
*--IF ERROR IN PRG RECORD BYPASS ALL RECORDS UNDER                              
*--THIS PRG. READ TEMPSTOR UNTIL NEXT PRG RECORD                                
*--IS FOUND.IF EOF OR DEAL OR PACKAGE RECORD                                    
*--IS FOUND FIRST EXIT                                                          
*                                                                               
DOPRG900 BAS   RE,BYPROG           ERROR BYPASS THIS RECORD                     
         B     XIT                                                              
*-EXIT                                                                          
DOPRGX   MVI   TWERROR,0                                                        
         B     XIT                                                              
*                                                                               
NAMECOMP CLC   0(0,RE),0(R2)       FOR TEST ON NAME TABLE                       
         DROP  R4                                                               
*                                                                               
CONVTAB  DC    CL6'NCBNCX'                                                      
         DC    CL6'CNNCNX'                                                      
         DC    CL6'SPRSPV'                                                      
         DC    CL6'CLSCLX'                                                      
         DC    CL6'PRMPRX'                                                      
         DC    CL6'RSTRSY'                                                      
         DC    CL6'XGMXGX'                                                      
         DC    CL6'SPNSPM'                                                      
******   DC    CL6'SPZSPM'                                                      
         DC    CL6'NFLNFQ'                                                      
         DC    CL6'MLBMLX'                                                      
         DC    CL6'SMVSMZ'                                                      
         DC    CL6'DSCDSX'                                                      
         DC    CL6'FXMFXZ'                                                      
         DC    CL6'NSCNSX'                                                      
         DC    X'FF'                                                            
*                                                                               
NAMETAB  DC    CL8'THE     ',XL1'03'                                            
         DC    X'FF'                                                            
VOWELLST DC    CL5'AEIOU'                                                       
         DC    X'FF'                                                            
*                                                                               
ROTTABLE DC    CL2'MO',XL4'40401001'         MON                                
         DC    CL2'TU',XL4'20202002'         TUE                                
         DC    CL2'WE',XL4'10103003'         WED                                
         DC    CL2'TH',XL4'08084004'         THU                                
         DC    CL2'FR',XL4'04045005'         FRI                                
         DC    CL2'SA',XL4'02026006'         SAT                                
         DC    CL2'SU',XL4'01017007'         SUN                                
         DC    CL2'MF',XL4'7C401001'         MON-FRI                            
         DC    CL2'WK',XL4'7F401001'         MON-SUN                            
         DC    X'FF'                                                            
*                                                                               
MIRRTBLE DC    H'00400',C'A '      CODE A                                       
         DC    H'-0500',C'B '      CODE B                                       
         DC    H'-1100',C'C '      CODE C                                       
         DC    H'-0600',C'D '      CODE D                                       
         DC    H'-0900',C'E '      CODE E                                       
         DC    H'00300',C'F '      CODE F                                       
         DC    H'00700',C'G '      CODE G                                       
         DC    H'-0400',C'H '      CODE H                                       
         DC    H'00630',C'I '      CODE I                                       
         DC    H'00730',C'J '      CODE J                                       
         DC    H'00500',C'K '      CODE K                                       
         DC    H'00430',C'L '      CODE L                                       
         DC    H'00200',C'Q '      CODE Q                                       
         DC    H'00600',C'R '      CODE R                                       
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*--READ A PROGRAM RECORD THAT FITS THE UNITS TO BE ADDED.                       
*--IF FOUND FILL FIELD TWPRCODE WITH THE PROGRAM CODE.                          
*--IF NONE FOUND PASS A X'FF' IN TWLSTLIN.                                      
GETPROG  NTR1                                                                   
*                                                                               
         CLI   TWPRINPC,C'Y'       WAS PROGRAM CODE INPUTTED                    
         BNE   *+8                                                              
         MVI   TWERROR,INVERR      PRESET ERROR CONDITION                       
*                                                                               
*        MVI   TWLSTLIN,0                                                       
*                                                                               
*                                                                               
*--READ A PROGRAM RECORD SEE IF IT ALREADY EXISTS                               
*                                                                               
         BAS   RE,READPROG         READ HIGH CALL FOR PROGRAM                   
         CLI   TWLSTLIN,191        PRIOR PROGRAM CODE MAXED OUT                 
         BH    GETPR020            READ NEXT RECORD                             
         B     GETPR040                                                         
*                                                                               
GETPR020 GOTO1 AIO,DMCB,SPT+DIR+SEQ                                             
*                                                                               
GETPR040 CLC   KEY(8),KEYSAVE                                                   
         BNE   GETPR090                                                         
*                                                                               
*--FOR KEY TO QUALIFY LAST THEREE POSITIONS                                     
*--OF PROGRAM CODE MUST BE CHAR. NUMERIC                                        
         TM    KEY+8,X'F0'                                                      
         BNO   GETPR080                                                         
         TM    KEY+9,X'F0'                                                      
         BNO   GETPR080                                                         
         TM    KEY+10,X'F0'                                                     
         BNO   GETPR080                                                         
         MVC   TWPRCODE+3(3),KEY+8  LINE COUNT NUMBER                           
*                                                                               
*--IF RECORD DOES EXIST CHECK NAME, DAY,ROTATION,TO SEE IF                      
*--THIS RECORD MATCHES THE UNITS TO BE ADDED.                                   
         L     R3,AIOAREA2                                                      
         USING NPGRECD,R3                                                       
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA2                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE '),(X'92',AIOAREA),0                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NPGEL92,R6                                                       
*                                                                               
         CLC   TWPRROT,NPGROT                                                   
         BNE   GETPR080                                                         
*                                                                               
         OC    NPGNAME,=16X'40'                                                 
         CLC   NPGNAME,TWPRNAME                                                 
         BNE   GETPR080                                                         
*                                                                               
         CLC   TWPRSTIM(4),NPGTIME                                              
         BNE   GETPR080                                                         
         B     GETPR100            YES VALID PROGRAM RECORD FOUND               
*                                                                               
GETPR080 CLI   TWPRINPC,C'Y'       WAS PROGRAM CODE INPUTTED                    
         BE    GETPR200            ERROR NO MATCH WITH PROGRAM RECORD           
         CLC   NPGKPROG+3(3),=XL3'F9F9F9'                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     GETPR020                                                         
*                                                                               
GETPR090 MVI   TWLSTLIN,X'FF'                                                   
GETPR100 MVI   TWERROR,0                                                        
         B     XIT                                                              
*--SET ERROR CODE OF PROGRAM NOT FOUND ON THE UNIT RECORD                       
*--NOTE: THIS CODE WILL ONLY BE SET IF A PROGRAM COD WAS SPECIFIED              
*--ON THE PROGRAM RECORD.                                                       
GETPR200 L     RE,UPACURUN         ADDRESS OF UNIT                              
         USING NUNTD,RE                                                         
         MVI   NUNTERNO+1,PROGERR                                               
         MVI   NUNTERF,2                                                        
         B     XIT                                                              
*                                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
*                                                                               
*--ROUTINE WILL ADD A NEW PROGRAM RECORD.                                       
*--FIELD TWPRCODE WILL HAVE THE NEW PROGRAM CODE.                               
*                                                                               
ADDPROG  NTR1                                                                   
         L     R3,AIOAREA2                                                      
         XCEF  (R3),2000                                                        
         USING NPGRECD,R3                                                       
*                                                                               
         PACK  DUB(2),TWPRCODE+3(3) CREATE A NEW PROGRAM LINE COUNT             
         AP    DUB(2),=PL1'1'                                                   
         UNPK  TWPRCODE+3(3),DUB(2)                                             
         OI    TWPRCODE+5,X'F0'                                                 
*                                                                               
*--BUILD NEW PROGRAM CODE                                                       
*                                                                               
         MVC   NPGKTYP,=XL2'0D20'                                               
         MVC   NPGKAM,TWAGYMED                                                  
         MVC   NPGKNET,TWMKT                                                    
         MVC   NPGKPROG,TWPRCODE                                                
         MVC   NPGKEND,=XL2'F19F'  DEC31/20                                     
*                                                                               
         MVC   NPGRLEN,=XL2'0021'                                               
         MVC   NPGCNTL+5(2),TWAGY                                               
*                                                                               
         MVC   NPGMAINL(2),=XL2'0108'                                           
         GOTO1 VDATCON,DMCB,(5,0),(3,NPGACTD)                                   
*                                                                               
*--BUILD 5D ELEMENT (FOR DEMOS)                                                 
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUBKD,RE                                                         
*                                                                               
         MVC   NUBKEL(2),=XL2'5D07'                                             
         MVC   NUBKFMS(3),=CL3'EVN'                                             
         MVC   NUBKBOOK(2),=XL2'580E'                                           
         DROP  RE                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'SPTFILE '),AIOAREA2,WORKAREA,0              
*                                                                               
*--BUILD 92 ELEMENT                                                             
         XC    WORKAREA(100),WORKAREA                                           
         LA    RE,WORKAREA                                                      
         USING NPGEL92,RE                                                       
*                                                                               
         MVC   NPGELEM(2),=XL2'9250'                                            
         MVI   NPGDAY,X'7F'                                                     
         MVC   NPGTIME,TWPRSTIM                                                 
         MVC   NPGNAME,TWPRNAME                                                 
         MVC   NPGROT,TWPRROT                                                   
         MVC   NPGROTNO,TWPRROTN                                                
         MVI   NPGDAYNO,X'17'                                                   
         MVI   NPGUPLD,C'Y'                                                     
         DROP  RE                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'SPTFILE '),AIOAREA2,WORKAREA,0              
*                                                                               
*--BUILD 03 ELEMENT                                                             
         XC    WORKAREA(100),WORKAREA                                           
         LA    RE,WORKAREA                                                      
         USING NPGEL03,RE                                                       
*                                                                               
         MVC   NPGSPEL(2),=XL2'0328'                                            
         MVC   NPGMIRCD,TWPRMRCD                                                
         DROP  RE                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'SPTFILE '),AIOAREA2,WORKAREA,0              
*--WRITE THE RECORD OUT                                                         
         GOTO1 AIO,DMCB,SPT+FILE+ADDREC,(R3)                                    
         B     ADDPRX                                                           
*--EXIT                                                                         
ADDPRX   MVI   TWERROR,0                                                        
         MVI   TWLSTLIN,1                                                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*--PROGRAM CODE INPUTTED READ RECORD                                            
*--CHECK TO SEE IF RECORD EXISTS. CHECK                                         
*--THAT THE INPUTTED ROTATION IS A SUBSET                                       
*--OF THE PROGRAM RECORDS ROTATION.                                             
CHKPROG  NTR1                                                                   
*                                                                               
*                                                                               
*        USING NPGMD,R4            PROGRAM RECORD DSECT                         
*                                                                               
*--READ A PROGRAM RECORD SEE IF IT ALREADY EXISTS                               
*                                                                               
*        MVI   TWERROR,INVERR                                                   
*        BAS   RE,READPROG         READ HIGH CALL FOR PROGRAM                   
*                                                                               
*        CLC   KEY(8),KEYSAVE                                                   
*        BNE   CHKPR500                                                         
*                                                                               
*--IF RECORD DOES EXIST CHECK NAME, DAY,ROTATION TO SEE IF                      
*--THIS RECORD MATCHES THE UNITS TO BE ADDED.                                   
*        GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA2                                   
*                                                                               
*        GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE '),(X'92',AIOAREA2),0               
*        CLI   12(R1),0                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        L     R6,12(R1)                                                        
*        USING NPGEL92,R6                                                       
*                                                                               
*        MVC   BYTE,NPGROT                                                      
*        OC    BYTE(1),TWPRROT                                                  
*        CLC   BYTE,NPGROT                                                      
*        BNE   CHKPR500                                                         
*        B     CHKPRX                                                           
*                                                                               
*   CHKPR500 MVI  NPGMERF,7                                                     
*        MVI  NPGMERNO+1,INVERR                                                 
*        B    XIT                                                               
*                                                                               
*   CHKPRX   MVI   TWERROR,0                                                    
*        B     XIT                                                              
*        DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
*--BUILD A PROGRAM ESTIMATE RECORD                                              
*                                                                               
DOPDRE NTR1                                                                     
*                                                                               
         USING NPDED,R4            ESTIMATED PROGRAM DEMO DSECT                 
*                                                                               
         CLI   TWPRINPC,C'Y'       IF CODE INPUTTED BYPASS RECORD               
         BE    DOPDEX                                                           
*                                                                               
         LA    RF,NPDEEHI                                                       
         LA    R1,TWDEMVE                                                       
         LA    R5,19                                                            
*                                                                               
DOPDE020 MVC   0(4,R1),=XL4'FFFFFFFF'   DEFAULT VALUE                           
         CLI   0(RF),X'40'         IF BLANK BYPASS                              
         BE    *+18                                                             
*                                                                               
         PACK  DUB,0(6,RF)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,15,0(R1)                                                      
         LA    RF,6(RF)                                                         
         LA    R1,4(R1)                                                         
         BCT   R5,DOPDE020                                                      
*                                                                               
         CLI   TWEDF,C'V'          CHECK DEMO FORM FOR VPH                      
         BNE   DOPDEX                                                           
         CLI   TWDEMVE,X'FF'                                                    
         BNE   DOPDEX                                                           
*--IF DEMO TYPE=VPH HOMES IMPS MUST EXIST                                       
         MVI   NPDEERNO+1,INVERR                                                
         MVI   NPDEERF,1                                                        
         BAS   RE,BYPROG                                                        
         B     XIT                                                              
*                                                                               
DOPDEX   MVI   TWERROR,0                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--BUILD A PROGRAM ACTUAL RECORD                                                
*                                                                               
DOPDRA NTR1                                                                     
*                                                                               
         USING NPDAD,R4            ACTUAL PROGRAM DEMO DSECT                    
*                                                                               
         LA    RF,NPDAEHI                                                       
         LA    R1,TWDEMVA                                                       
         LA    R5,19                                                            
*                                                                               
DOPDA020 MVC   0(4,R1),=XL4'FFFFFFFF'   DEFAULT VALUE                           
         CLI   0(RF),X'40'         IF BLANK BYPASS                              
         BE    *+18                                                             
*                                                                               
         PACK  DUB,0(6,RF)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,15,0(R1)                                                      
         LA    RF,6(RF)                                                         
         LA    R1,4(R1)                                                         
         BCT   R5,DOPDA020                                                      
*                                                                               
         CLI   TWADF,C'V'          CHECK DEMO FORM FOR VPH                      
         BNE   DOPDAX                                                           
         CLI   TWDEMVA,X'FF'                                                    
         BNE   DOPDAX                                                           
*--IF DEMO TYPE=VPH HOMES IMPS MUST EXIST                                       
         MVI   NPDAERNO+1,INVERR                                                
         MVI   NPDAERF,1                                                        
         BAS   RE,BYPROG                                                        
         B     XIT                                                              
*                                                                               
DOPDAX   MVI   TWERROR,0                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--IF ERROR IN PRG,PDRE,PDRA RECORD BYPASS ALL RECORDS UNDER                    
*--THIS PRG. READ TEMPSTOR UNTIL NEXT PRG RECORD                                
*--IS FOUND.IF EOF OR DEAL OR PACKAGE RECORD                                    
*--IS FOUND FIRST EXIT                                                          
*                                                                               
BYPROG   NTR1                                                                   
*                                                                               
BYPRG020 MVI   TWERROR,INVERR                                                   
BYPRG100 BAS   RE,NEXT                                                          
         L     R4,UPACUROB                                                      
         CLC   12(4,R4),=CL4'PROG'                                              
         BE    BYPRGX                                                           
         CLC   12(4,R4),=CL4'DEAL'                                              
         BE    BYPRGX                                                           
         CLC   12(4,R4),=CL4'PKG*'                                              
         BE    BYPRGX                                                           
         CLC   12(4,R4),=CL4'EOD*'                                              
         BE    BYPRGX                                                           
         CLC   12(4,R4),=CL4'EOF*'                                              
         BE    BYPRGX                                                           
         B     BYPRG100                                                         
*-EXIT                                                                          
BYPRGX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*--BUILD A UNIT RECORD                                                          
*                                                                               
* THIS ROUTINE:                                                                 
*    BUILDS THE BASIC INFORMATION NEEDED TO BUILD                               
*    A UNIT RECORD.                                                             
*                                                                               
*    NOTE:      IF PROGRAM CODE INPUTTED PROGRAM RECORD                         
*               RESIDES IN AIOAREA2                                             
*                                                                               
DOUNIT   NTR1                                                                   
*                                                                               
*INITIALIZE SETTINGS                                                            
         MVI   TWSPSEQ,1           SET SPECIAL CHARGE SEQ NUMBER                
         MVI   TWUNST3,0                                                        
         OI    TWUNST3,X'04'      SET UNIT STATUS 3 FOR CABLE UPLOAD            
         MVI   TWSPDEL,C'N'                                                     
*                                                                               
         USING NUNTD,R4            UNIT RECORD DSECT                            
         ST    R4,UPACURUN         ADDRESS OF UNIT RECORD                       
*                                                                               
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
         XCEF  (R3),2000                                                        
*--04 KEY                                                                       
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,TWAGYMED                                                   
         MVC   NUKCLT,TWCLIENT                                                  
         MVC   NUKNET,TWNET                                                     
*        MVC   NUKPROG,TWPRCODE                                                 
         MVC   NUKEST,TWEST                                                     
         MVC   NUKDP,TWDYPT                                                     
         MVC   NUKTIME,TWQTRHR                                                  
*                                                                               
*--SET AIR DATE                                                                 
*                                                                               
*-CHECK DAY FIELD IS VALID                                                      
         BAS   RE,AUTCALDR          CHECK FOR AUTO CALANDER                     
*                                                                               
         MVI   NUNTERF,16                                                       
*                                                                               
         PACK  DUB,NUNTDAYN(1)                                                  
         CVB   RE,DUB                                                           
         STCM  RE,1,BYTE                                                        
*                                                                               
         MVC   TWDAYNO,BYTE        DAY NUMBER FOR 94 KEY                        
*                                                                               
         CLI   BYTE,1                                                           
         BL    DOUNT500                                                         
         CLI   BYTE,7                                                           
         BH    DOUNT500                                                         
*-CHECK DAY FIELD IS IN ROTATION                                                
DOUNT010 MVI   NUNTERNO+1,COVERR                                                
         ZIC   R6,BYTE                                                          
         BCTR  R6,0                                                             
         LA    RE,TWPROTE                                                       
         AR    RE,R6                                                            
         CLI   0(RE),C'Y'          IS DAY COVERED IN ROTATION                   
         BNE   DOUNT500            NO ERROR                                     
*-CHECK DATE FOR MONDAY                                                         
         MVI   NUNTERNO+1,INVERR                                                
         MVI   NUNTERF,2                                                        
         MVC   DUB(6),NUNTWOD                                                   
         GOTO1 VGETDAY,DMCB,DUB,FULL                                            
         CLI   0(R1),1                                                          
         BNE   DOUNT500                                                         
*-SET DATE TO DAY NUMBER                                                        
         GOTO1 VADDAY,DMCB,DUB,DUB,(R6)                                         
*-CHECK DATE WITHIN ESTIMATE RANGE                                              
         MVI   NUNTERNO+1,STERR                                                 
         MVI   NUNTERF,2                                                        
         L     R5,AIOAREA4                                                      
         USING BUYVALD,R5                                                       
         CLC   DUB(6),TWESSTRT     IS DATE > OR = TO EST START                  
         BNL   DOUNT020            YES CHECK END DATE                           
         BAS   RE,DOUFSTD          BUMP TO NEXT ROTATION DAY                    
         BZ    DOUNT010            CHECK NEXT ROTATION DAY                      
         B     DOUNT500            NO MORE ROTATION DAYS ERROR                  
DOUNT020 MVI   NUNTERNO+1,ENDERR                                                
         CLC   DUB(6),TWESEND                                                   
         BH    DOUNT500                                                         
         DROP  R5                                                               
*                                                                               
         MVC   TWUDATE,DUB                                                      
         GOTO1 VDATCON,DMCB,(0,DUB),(2,NUKDATE)                                 
         MVC   TWPDATE,NUKDATE                                                  
         MVI   NUNTERNO+1,0                                                     
         MVI   NUNTERF,0                                                        
         OC    NURSTAT,TWPSTBIT                                                 
         MVC   NUUNCODE,TWUNIV                                                  
         MVI   NUPOSTDT,C'D'                                                    
         MVI   NUHUTTYP,C'A'                                                    
         MVC   NUHUTSCM,TWHUTSCH                                                
*                                                                               
         MVI   NURLEN+1,X'6C'      RECORD LENGTH 108                            
*--SET 01 ELEMENT                                                               
         MVC   NUMAINEL(2),=XL2'0150'                                           
         MVC   NUPACK,TWPKG                                                     
         MVC   NUPACKST,TWPKSTAT                                                
*                                                                               
         MVC   NUUNITST,TWSTAT                                                  
         MVC   NUPROGNM,TWPRNAME                                                
         MVC   NULEN,TWPLEN                                                     
         MVC   NUPRD,TWMSTPRD                                                   
         LA    RE,DAYTAB                                                        
         AR    RE,R6               R6 CONTAINS DAY NUMBER                       
         MVC   NUDAY,0(RE)                                                      
         MVC   NUTIME,TWPRSTIM                                                  
*-CHECK ACTUAL                                                                  
         CLC   NUACTUAL,TWPRACT                                                 
         BE    DOUNT040                                                         
         GOTO1 CKPAID,DMCB,(C'T',DUB)                                           
         BNZ   DOUNT040                                                         
         MVI   NUNTERF,1                                                        
         MVI   NUNTRNUM+1,189                                                   
         B     DOUNT500                                                         
DOUNT040 MVC   NUACTUAL,TWPRACT                                                 
*-CHECK INTEGRATION                                                             
         CLC   NUINTEG,TWPRINT                                                  
         BE    DOUNT060                                                         
         GOTO1 CKPAID,DMCB,(C'I',DUB)                                           
         BNZ   DOUNT060                                                         
         MVI   NUNTERF,1                                                        
         MVI   NUNTRNUM+1,189                                                   
         B     DOUNT500                                                         
DOUNT060 MVC   NUINTEG,TWPRINT                                                  
*                                                                               
         MVC   NUASSIGN,TWPRASS                                                 
         BAS   RE,CALCOST2                                                      
         OC    NUUNST2,TWHUTST                                                  
         CLI   NUNTPM,C'P'                                                      
         BNE   *+8                                                              
         OI    NUUNITST,X'40'                                                   
         OI    NUACTWHY,X'80'       NEW BUY                                     
         MVC   NUMARKET,TWMKT                                                   
         MVC   NUALPHA,TWAAGY                                                   
*                                                                               
*--BUILD 02 ELEMENT                                                             
         XC    WORKAREA(50),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUSDRD,RE                                                        
*                                                                               
         MVC   NUSDREL(2),=XL2'0214'                                            
         MVC   NUSTATYP,TWMEDTYP                                                
         MVC   NUSDROT,TWPRROT                                                  
         MVC   NUPOSTYP,TWPSTTYP                                                
         MVC   NUSDSRT,TWRATE                                                   
         MVC   NUSDRTCV,TWCVRGE                                                 
         MVC   NUMIRTYP,TWPRMRCD                                                
         MVC   NUBKTYP,TWBKTYP                                                  
         CLI   NUNTADU,C'Y'                                                     
         BNE   *+8                                                              
         OI    NUSDST3,X'02'                                                    
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'02' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
*******  GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RE                                                               
*                                                                               
*--BUILD 18 ELEMENT                                                             
         XC    WORKAREA(150),WORKAREA                                           
         LA    RE,WORKAREA                                                      
         USING NUDTAD,RE                                                        
*                                                                               
         MVI   NUDTAEL,X'18'                                                    
         MVI   NUDTALEN,NUDTELEN                                                
         MVC   NUDTTRAD,TWTRADP                                                 
         MVC   NUDVTYPE,TWVTYPE                                                 
         MVC   NUDTCASH,TWCASHP                                                 
         MVC   AIOAREA,AIOAREA1    ADD X'18' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
         DROP  RE                                                               
* CHECK IF CASH CREDIT NEEDS TO BEE ADDED                                       
******   GOTO1 =A(OVFLRTN2),DMCB,(2,DUB),(RC),(R9),RR=MYRELO   CALCASHP         
         L     R5,AIOAREA4         R5 COVERS BUY VALUES                         
         USING BUYVALD,R5                                                       
         GOTO1 VCALCASH,DMCB,UPACURUN,ACOMFACS,(X'01',CLICPRD)                  
         DROP  R5                                                               
*                                                                               
*--BUILD 19 ELEMENT ALPHA PRODUCT                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'UNTFIL  '),(X'19',AIOAREA1),0               
         OC    TWMSTPRA,TWMSTPRA                                                
         BZ    DOUNT100                                                         
         XC    WORKAREA(100),WORKAREA                                           
         LA    RE,WORKAREA                                                      
         USING NUPDED,RE                                                        
*                                                                               
         MVC   NUPDEEL(2),=XL2'190A'                                            
         MVC   NUPDEPR,TWMSTPRA                                                 
         MVC   AIOAREA,AIOAREA1    ADD X'19' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
*******  GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RE                                                               
*                                                                               
*--BUILD 21 ELEMENT (FOR TRAFFIC)                                               
DOUNT100 XC    WORKAREA(100),WORKAREA                                           
         LA    RE,WORKAREA                                                      
         USING NUCMLEL,RE                                                       
*                                                                               
         MVC   NUCMLEID(2),=XL2'2150'                                           
*                                                                               
         CLI   NUNTBLAT,C'Y'                                                    
         BNE   *+8                                                              
         OI    NUCMLFLG,X'04'      SET FOR BILLBOARD                            
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'21' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
*******  GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RE                                                               
*                                                                               
*--BUILD 60 ELEMENT FOR BUY TYPE                                                
         CLI   TWBUYTYP,X'40'       IS THERE A BUY TYPE                         
         BNH   DOUNT120                                                         
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUOTH,RE                                                         
*                                                                               
         MVC   NUOTEL(2),=XL2'6004'                                             
         MVI   NUOTTYP,C'F'                                                     
         MVC   NUOTHER(1),TWBUYTYP                                              
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'60' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
*                                                                               
*--BUILD 60 ELEMENT FOR HP PERCENT                                              
DOUNT120 CLI   TWHPPCT,X'40'       IS THERE AN HP PERCENT                       
         BNH   DOUNT150                                                         
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
*                                                                               
         MVC   NUOTEL(2),=XL2'6008'                                             
         MVI   NUOTTYP,C'0'                                                     
         MVC   NUOTHER(5),TWHPPCT                                               
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'60' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
*******  GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RE                                                               
*--BUILD 35 ELEMENT (FOR VPH)                                                   
DOUNT150 XC    WORKAREA(70),WORKAREA                                            
         LA    RF,WORKAREA                                                      
         USING NUEHD,RF                                                         
*                                                                               
         MVC   NUEHEL(3),=XL3'350902'                                           
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'35' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
******   GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RF                                                               
*--BUILD 75 ELEMENT (SERIAL NUMBER)                                             
         XC    WORKAREA(70),WORKAREA                                            
         LA    RF,WORKAREA                                                      
         USING NUSQD,RF                                                         
*                                                                               
*!!!!    MVC   NUSQEL(2),=XL2'7514'                                             
         MVI   NUSQEL,X'75'                                                     
         MVI   NUSQLEN,NUSQDQ       ELEM LN                                     
         MVC   NUSQSER,NUNTSERN     SERIAL #                                    
         MVC   NUSQDEAL,TWDEAL      DEAL #                                      
         MVC   NUSQCON,TWCON        CONTRACT #                                  
*                                                                               
         BAS   RE,PUTUNEL                                                       
******   GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RF                                                               
*--BUILD 45 ELEMENT                                                             
         XC    WORKAREA(70),WORKAREA                                            
         LA    RF,WORKAREA                                                      
         USING NUAHD,RF                                                         
*                                                                               
         MVC   NUAHEL(2),=XL2'4509'                                             
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'45' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
******   GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RF                                                               
*                                                                               
*--BUILD 5D ELEMENT (FOR DEMOS)                                                 
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUBKD,RE                                                         
*                                                                               
         MVC   NUBKEL(2),=XL2'5D07'                                             
         MVC   NUBKBOOK(2),=XL2'580E'                                           
         MVC   NUBKFMS(3),=CL3'EIN'                                             
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'5D' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
******   GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RE                                                               
*--CREATE UNIVERSE ELEMENT                                                      
*        BAS   RE,BLDUNIV                                                       
         GOTO1 =A(OVFLRTN),DMCB,(7,DUB),(RC),(R9),RR=MYRELO   BLDUNIV           
         MVC   AIOAREA,AIOAREA1                                                 
         BAS   RE,PUTUNEL                                                       
******   GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
*                                                                               
*--BUILD 99 ELEMENT                                                             
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUACTD,RE                                                        
*                                                                               
         MVC   NUACTEL(2),=XL2'9917'                                            
         MVC   NUACTOVL,OVLAYNUM                                                
         GOTO1 VDATCON,DMCB,(5,0),(3,NUACTADT)                                  
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'99' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
******   GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RE                                                               
*                                                                               
*--BUILD DD ELEMENT                                                             
         PRINT GEN                                                              
         GOTO1 BUILDDEM,DMCB,(X'DD',TWDEMOS),(TWEDF,TWDEMVE)                    
         PRINT NOGEN                                                            
*                                                                               
*--BUILD DE ELEMENT                                                             
         PRINT GEN                                                              
         GOTO1 BUILDDEM,DMCB,(X'DE',TWDEMOS),(TWADF,TWDEMVA)                    
         PRINT NOGEN                                                            
         B     DOUNT600                                                         
*                                                                               
DOUNT500 BAS   RE,BYUNIT                                                        
         B     XIT                                                              
*                                                                               
DOUNT600 B     XIT                                                              
         DROP  R3,R4                                                            
*                                                                               
DAYTAB   DC    XL7'40201008040201'                                              
         EJECT                                                                  
*                                                                               
*--SUPPRESS AUTO ROTATE IF PROFILE REQUIRES                                     
*--IF SUPPRESS AUTO CALANDER IS SET, KEEP ONLY FIRST DAY                        
*--FIRST DAY OF ROTATION.                                                       
*                                                                               
AUTCALDR NTR1                                                                   
         CLI   BUYPROF+12,C'Y'                                                  
         BE    AUTCLEX                                                          
*                                                                               
         LA    RE,7                                                             
         LA    RF,TABNUM                                                        
         LA    R1,TWPROTE                                                       
*                                                                               
AUTCL050 CLI   0(R1),C'Y'                                                       
         BE    AUTCL100                                                         
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,AUTCL050                                                      
         B     AUTCLEX                                                          
*                                                                               
AUTCL100 L     RE,UPACURUN         ADDRESS OF UNIT RECORD                       
         USING NUNTD,RE            UNIT RECORD DSECT                            
         MVC   NUNTDAYN,0(RF)      SET DAY TO FIRST DAY OF ROTATION             
         DROP  RE                                                               
*                                                                               
AUTCLEX  B     XIT                                                              
TABNUM   DC    CL7'1234567'                                                     
*                                                                               
*--GET NEXT DAY OF ROTATION                                                     
*                                                                               
DOUFSTD  NTR1                                                                   
*                                                                               
DOUFS010 ZIC   RE,TWDAYNO                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,1,TWDAYNO                                                     
         CLI   TWDAYNO,7                                                        
         BH    DOUFSNO                                                          
*                                                                               
*-CHECK DAY FIELD IS IN ROTATION                                                
         ZIC   R6,TWDAYNO                                                       
         BCTR  R6,0                                                             
         LA    RE,TWPROTE                                                       
         AR    RE,R6                                                            
         CLI   0(RE),C'Y'          IS DAY COVERED IN ROTATION                   
         BNE   DOUFS010            NO ERROR                                     
         MVC   BYTE,TWDAYNO                                                     
*                                                                               
DOUFSYES SR    RE,RE                                                            
DOUFSNO  LTR   RE,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  PUT ELEMENT TO THE UNIT RECORD                                               
*                                                                               
PUTUNEL  NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         B     XIT                                                              
*                                                                               
*--BUILD UNIT ESTIMATED DEMOS                                                   
*                                                                               
DOUDRE   NTR1                                                                   
*                                                                               
         USING NUDED,R4            UNIT ESTIMATED DEMO DSECT                    
*                                                                               
         XC    WORKAREA+100(100),WORKAREA+100                                   
         LA    RF,NUDEEHI                                                       
         LA    R1,WORKAREA+100                                                  
         LA    R5,19                                                            
*                                                                               
DOUDE020 MVC   0(4,R1),=XL4'FFFFFFFF'   DEFAULT VALUE                           
         CLI   0(RF),X'40'         IF BLANK BYPASS                              
         BE    *+18                                                             
*                                                                               
         PACK  DUB,0(6,RF)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,15,0(R1)                                                      
         LA    RF,6(RF)                                                         
         LA    R1,4(R1)                                                         
         BCT   R5,DOUDE020                                                      
*                                                                               
         CLI   TWEDF,C'V'          CHECK DEMO FORM FOR VPH                      
         BNE   DOUDEX                                                           
         CLI   WORKAREA+100,X'FF'                                               
         BNE   DOUDEX                                                           
*--IF DEMO TYPE=VPH HOMES IMPS MUST EXIST                                       
         MVI   NUDEERNO+1,INVERR                                                
         MVI   NUDEERF,1                                                        
         BAS   RE,BYUNIT                                                        
         B     XIT                                                              
*                                                                               
*                                                                               
*--BUILD DD ELEMENT                                                             
DOUDEX   LA    R5,WORKAREA+100                                                  
         PRINT GEN                                                              
         GOTO1 BUILDDEM,DMCB,(X'DD',TWDEMOS),(TWEDF,(R5))                       
         PRINT NOGEN                                                            
         MVI   TWERROR,0                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--BUILD UNIT ACTUAL DEMOS                                                      
*                                                                               
DOUDRA NTR1                                                                     
*                                                                               
         USING NUDAD,R4            UNIT ACTUAL DEMO DSECT                       
*                                                                               
         XC    WORKAREA+100(100),WORKAREA+100                                   
         LA    RF,NUDAEHI                                                       
         LA    R1,WORKAREA+100                                                  
         LA    R5,19                                                            
*                                                                               
DOUDA020 MVC   0(4,R1),=XL4'FFFFFFFF'   DEFAULT VALUE                           
         CLI   0(RF),X'40'         IF BLANK BYPASS                              
         BE    *+18                                                             
*                                                                               
         PACK  DUB,0(6,RF)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,15,0(R1)                                                      
         LA    RF,6(RF)                                                         
         LA    R1,4(R1)                                                         
         BCT   R5,DOUDA020                                                      
*                                                                               
         CLI   TWADF,C'V'          CHECK DEMO FORM FOR VPH                      
         BNE   DOUDAX                                                           
         CLI   WORKAREA+100,X'FF'                                               
         BNE   DOUDAX                                                           
*--IF DEMO TYPE=VPH HOMES IMPS MUST EXIST                                       
         MVI   NUDAERNO+1,INVERR                                                
         MVI   NUDAERF,1                                                        
         BAS   RE,BYUNIT                                                        
         B     XIT                                                              
*                                                                               
*--BUILD DD ELEMENT                                                             
DOUDAX   LA    R5,WORKAREA+100                                                  
         PRINT GEN                                                              
         GOTO1 BUILDDEM,DMCB,(X'DE',TWDEMOS),(TWADF,(R5))                       
         PRINT NOGEN                                                            
         MVI   TWERROR,0                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--BUILD SPECIAL CHARGE ELEMENT                                                 
*                                                                               
DOSPCH NTR1                                                                     
*                                                                               
         USING NSPCD,R4            SPECIAL CHARGES DSECT                        
*DELETE OLD ELEMENTS                                                            
         CLI   TWSPDEL,C'N'                                                     
         BNE   DOSPC050                                                         
         GOTO1 VHELLO,DMCB,(C'D',=C'UNTFIL  '),(X'03',AIOAREA1),0               
         MVI   TWSPDEL,C'Y'                                                     
*                                                                               
DOSPC050 XC    WORKAREA(50),WORKAREA                                            
         LA    R5,WORKAREA                                                      
         USING  NUSPRD,R5                                                       
*                                                                               
*******  MVC   NUSPREL(2),=X'030C'                                              
         MVI   NUSPREL,X'03'                                                    
         MVI   NUSPRLEN,NUSPRLN1                                                
*--SEQUENCE NUMBER                                                              
         MVC   NUSPRSEQ,TWSPSEQ                                                 
         ZIC   RE,TWSPSEQ                                                       
         LA    RE,1(RE)                                                         
         STC   RE,TWSPSEQ                                                       
*--AMOUNT                                                                       
         PACK  DUB,NSPCAMT(9)                                                   
         CVB   RE,DUB                                                           
         STCM  RE,15,NUSPRAMT                                                   
*--CODE                                                                         
         LA    RE,RATCODE                                                       
*                                                                               
DOSPC100 CLI   0(RE),X'FF'                                                      
         BE    DOSPC500                                                         
         CLC   NSPCSPC,0(RE)                                                    
         BE    DOSPC150                                                         
         LA    RE,3(RE)                                                         
         B     DOSPC100                                                         
*                                                                               
DOSPC150 MVC   NUSPRTYP,2(RE)                                                   
         CLI   NUSPRTYP,C'U'                                                    
         BNE   *+8                                                              
         OI    TWUNST3,X'40'                                                    
*-CHECK SPECIALS                                                                
         GOTO1 CKPAID,DMCB,(NUSPRTYP,DUB)                                       
         BNZ   DOSPC180                                                         
         MVI   NSPCERF,1                                                        
         MVI   NSPCRNUM+1,189                                                   
         B     DOSPC500                                                         
*--WRITE THE ELEMENT OUT                                                        
DOSPC180 BAS   RE,PUTUNEL                                                       
****     GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         B     DOSPCX                                                           
*--ERROR CODE                                                                   
DOSPC500 MVI   NSPCERNO+1,INVERR                                                
         MVI   NSPCERF,1                                                        
         BAS   RE,BYUNIT                                                        
         B     XIT                                                              
*                                                                               
DOSPCX   MVI   TWERROR,0                                                        
         B     XIT                                                              
*                                                                               
RATCODE  DC    CL3'CIU'                                                         
         DC    CL3'BOB'                                                         
         DC    CL3'CSS'                                                         
         DC    CL3'TXX'                                                         
         DC    CL3'SEE'                                                         
         DC    CL3'ADA'                                                         
         DC    CL3'OTO'                                                         
         DC    XL1'FF'                                                          
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    SEE IF A SECOND COST PERCENTAGE WAS SET UP ON THE                          
*    CLIENT OR ESTIMATE RECORD AND IF IT WAS CALCULATES THE                     
*    SECOND COST AND INSERTS IT IN THE ASSIGNED COST FIELD                      
*                                                                               
*                                                                               
CALCOST2 NTR1                                                                   
         L     RE,AIOAREA4                                                      
         USING BUYVALD,RE                                                       
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
         MVC   FULL,CLICOST2       MOVE CLIENT PCT.                             
         OC    ESTCOST2,ESTCOST2   WAS ESTIMATE LEVEL COST INPUTTED             
         BZ    *+10                                                             
         MVC   FULL,ESTCOST2                                                    
         OC    PKGCOST2,PKGCOST2   WAS PACKAGE LEVEL COST INPUTTED              
         BZ    *+10                                                             
         MVC   FULL,PKGCOST2                                                    
         OC    FULL,FULL           WAS ANY COST PCT INPUTTED                    
         BZ    XIT                 NO, EXIT                                     
*                                                                               
         NI    NUUNITST,X'F7'                                                   
         XC    NUASSIGN,NUASSIGN                                                
         TM    FULL,X'80'           WAS COST PCT SET TO ZERO                    
         BZ    CL2CS50              NO CALCULATE                                
         OI    NUUNITST,X'08'                                                   
         B     XIT                                                              
*                                                                               
CL2CS50  ZAP   WORK(16),=PL1'0'                                                 
         ICM   R1,15,NUACTUAL       COST                                        
         CVD   R1,WORK+8                                                        
         ICM   R1,15,FULL           PERCENTAGE X.XXXXXX                         
         CVD   R1,DUB                                                           
         MP    WORK(16),DUB+4(4)    MULT COST BY PERCENTAGE                     
         AP    WORK(16),=PL4'500000'  ROUND                                     
         DP    WORK(16),=PL4'1000000'                                           
         CVB   R1,WORK+4                                                        
         STCM  R1,15,NUASSIGN                                                   
         OC    NUASSIGN,NUASSIGN                                                
         BNZ   XIT                                                              
         OI    NUUNITST,X'08'                                                   
         B     XIT                                                              
         DROP  R3,RE                                                            
         EJECT                                                                  
*                                                                               
*--VALIDATE PRODUCT RECORD                                                      
*                                                                               
DOPROD NTR1                                                                     
*                                                                               
         USING NPRDD,R4            PRODUCT DSECT                                
*                                                                               
         L     R5,AIOAREA1                                                      
         USING NURECD,R5                                                        
*                                                                               
         L     R6,AIOAREA4                                                      
         USING BUYVALD,R6                                                       
*                                                                               
*--CHECK IF POL INPUTTED                                                        
         CLC   NPRDPRD,=CL3'POL'    CHECK 1ST PRODUCT                           
         BE    DOPRD080                                                         
         CLC   NPRDPPRD,=CL3'POL'   CHECK SECOND PRODUCT                        
         BE    DOPRD080                                                         
*                                                                               
*--PRODUCT 1                                                                    
*                                                                               
*--CHECK FOR VALID PRODUCT                                                      
         XC    KEY(20),KEY                                                      
         LA    R3,KEY                                                           
         USING PRDHDR,R3                                                        
         MVC   PLSTTYPE(2),=XL2'0DF1'                                           
         MVC   PLSTAM,TWAGYMED                                                  
         MVC   PLSTCLT,TWCLIENT                                                 
         MVC   PLSTPRD,NPRDPRD                                                  
         CLC   NPRDPRD(3),=CL3'POL'                                             
         BNE   *+8                                                              
         MVI   PLSTXFF,X'FF'                                                    
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(9),KEYSAVE                                                   
         BE    DOPRD100                                                         
DOPRD080 MVI   NPRDERNO+1,INVERR                                                
         MVI   NPRDERF,1                                                        
         B     DOPRD500                                                         
*                                                                               
*-CHECK PROD1                                                                   
DOPRD100 CLC   NUPRD,PLSTBPRD+1                                                 
         BE    DOPRD120                                                         
         GOTO1 CKPAID,DMCB,(X'FF',DUB)                                          
         BNZ   DOPRD120                                                         
         MVI   NPRDERF,1                                                        
         MVI   NPRDERNO+1,107                                                   
         B     DOPRD500                                                         
DOPRD120 MVC   NUPRD,PLSTBPRD+1    ONE BYTE PRODUCT CODE                        
         DROP  R3                                                               
*                                                                               
*--CHECK ESTIMATE AGAINST PRODUCT                                               
         XC    KEY(20),KEY                                                      
         MVC   KEY+1(1),TWAGYMED                                                
         MVC   KEY+2(2),TWCLIENT                                                
         MVC   KEY+4(3),NPRDPRD                                                 
         MVC   KEY+7(1),TWEST                                                   
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+16                                                             
         MVI   NPRDERF,1                                                        
         MVI   NPRDERNO+1,106                                                   
         B     DOPRD500                                                         
*--PRODUCT 2                                                                    
         CLI   NPRDPPRD,X'40'                                                   
         BNH   DOPRD400                                                         
*                                                                               
*--CHECK FOR VALID PRODUCT                                                      
         XC    KEY(20),KEY                                                      
         LA    R3,KEY                                                           
         USING PRDHDR,R3                                                        
         MVC   PLSTTYPE(2),=XL2'0DF1'                                           
         MVC   PLSTAM,TWAGYMED                                                  
         MVC   PLSTCLT,TWCLIENT                                                 
         MVC   PLSTPRD,NPRDPPRD                                                 
         CLC   NPRDPPRD(3),=CL3'POL'                                            
         BNE   *+8                                                              
         MVI   PLSTXFF,X'FF'                                                    
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(9),KEYSAVE                                                   
         BE    DOPRD200                                                         
         MVI   NPRDERNO+1,INVERR                                                
         MVI   NPRDERF,2                                                        
         B     DOPRD500                                                         
*                                                                               
*-CHECK PROD2                                                                   
DOPRD200 CLC   NUPRD2,PLSTBPRD+1                                                
         BE    DOPRD220                                                         
         GOTO1 CKPAID,DMCB,(X'FF',DUB)                                          
         BNZ   DOPRD220                                                         
         B     DOPRD500                                                         
DOPRD220 MVC   NUPRD2,PLSTBPRD+1   ONE BYTE PRODUCT CODE                        
         DROP  R3                                                               
*--CHECK ESTIMATE AGAINST PRODUCT                                               
         XC    KEY(20),KEY                                                      
         MVC   KEY+1(1),TWAGYMED                                                
         MVC   KEY+2(2),TWCLIENT                                                
         MVC   KEY+4(3),NPRDPPRD                                                
         MVC   KEY+7(1),TWEST                                                   
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+16                                                             
         MVI   NPRDERF,2                                                        
         MVI   NPRDERNO+1,106                                                   
         B     DOPRD500                                                         
*--LENGTH                                                                       
         PACK  DUB,NPRDPLN1(3)                                                  
         CVB   RE,DUB                                                           
         STCM  RE,1,NULEN1                                                      
         CLC   NULEN1,NULEN                                                     
         BNH   DOPRD300                                                         
         MVI   NPRDERNO+1,INVERR                                                
         MVI   NPRDERF,NPRDPL1Q                                                 
         B     DOPRD500                                                         
*--PERCENT                                                                      
DOPRD300 PACK  DUB,NPRDPCS1(3)                                                  
         CVB   RE,DUB                                                           
         STCM  RE,1,NUP1SHR                                                     
         CLC   NUP1SHR,=H'10000'                                                
         BNH   DOPRD400                                                         
         MVI   NPRDERNO+1,INVERR                                                
         MVI   NPRDERF,NPRDPCSQ                                                 
         B     DOPRD500                                                         
*                                                                               
*--BUILD ALPHA PRODUCT ELEMENT                                                  
DOPRD400 XC    WORKAREA(100),WORKAREA                                           
         LA    R3,WORKAREA                                                      
         USING NUPDED,R3                                                        
*--REMOVE EXISTING 19 ELEMENT                                                   
         GOTO1 VHELLO,DMCB,(C'D',=C'UNTFIL  '),(X'19',AIOAREA1),0               
*                                                                               
*  BUILD NEW ELEMENT                                                            
         MVC   NUPDEEL(2),=XL2'190A'                                            
         MVC   NUPDEPR,NPRDPRD                                                  
         CLI   NPRDPPRD,X'40'                                                   
         BNH   *+14                                                             
         MVI   NUPDELEN,X'11'                                                   
         MVC   NUPDEPR+7(3),NPRDPPRD                                            
         MVC   AIOAREA,AIOAREA1    ADD X'19' ELEMENT                            
         BAS   RE,PUTUNEL                                                       
         B     DOPRDX                                                           
*                                                                               
*--ERROR CODE                                                                   
DOPRD500 BAS   RE,BYUNIT                                                        
         B     XIT                                                              
*                                                                               
DOPRDX   MVI   TWERROR,0                                                        
         B     XIT                                                              
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
*                                                                               
*--BUILD COMMENT ELEMENT                                                        
*                                                                               
DOCMMT NTR1                                                                     
*                                                                               
         USING NCOMD,R4            COMMENT DSECT                                
*                                                                               
         XC    WORKAREA(50),WORKAREA                                            
         LA    R5,WORKAREA                                                      
         USING NUCOMD,R5                                                        
*                                                                               
         MVC   NUCOMEL(2),=XL2'044A'                                            
*--TYPE                                                                         
         MVI   NUCOMTYP,C'I'       INTERNAL                                     
*--LINE                                                                         
         MVI   NUCOMLIN,1                                                       
*--COMMENT                                                                      
         MVC   NUCOMMNT(70),NCOMDATA                                            
*--WRITE THE ELEMENT OUT                                                        
         BAS   RE,PUTUNEL                                                       
******   GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         B     DOCOMX                                                           
*                                                                               
DOCOMX   MVI   TWERROR,0                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--DELETE UNITS                                                                 
*                                                                               
DODEL  NTR1                                                                     
*                                                                               
         USING NDELD,R4            COMMENT DSECT                                
         USING NURECD,R3                                                        
*-SET LINE NUMBER                                                               
         LA    R5,NDELSLN1                                                      
*-SET ERROR                                                                     
         MVI   NDELERF,1                                                        
DODEL100 MVI   NDELERNO+1,X'53'                                                 
*-BUILD 84 KEY                                                                  
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,TWAGYMED                                                  
         MVC   NUKPCLT(15),NDELCLT                                              
         MVC   NUKPSUB,0(R5)                                                    
         MVC   NUKPDP,NDELDPT                                                   
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BNE   DODEL500                                                         
*--GET THE RECORD                                                               
         L     R3,AIOAREA1                                                      
         GOTO1 AIO,DMCB,UNT+FILE+GET+UPDATE,AIOAREA1                            
*--PRE-EMPT THE RECORD                                                          
         OI    NUUNITST,X'40'                                                   
         GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA1                                   
*-BUMP TO NEXT LINE                                                             
         CLC   NDELSLN2,0(R5)                                                   
         BE    DODELEX                                                          
         LA    R5,1(R5)                                                         
         CLI   0(R5),0             WAS SECOND LINE INPUTTED                     
         BE    DODELEX             NO EXIT                                      
         MVI   NDELERF,2                                                        
         B     DODEL100                                                         
*                                                                               
DODELEX  MVI   NDELERF,0                                                        
         MVI   NDELERNO+1,0                                                     
DODEL500 B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
*--HANDLE END OF UNIT                                                           
*                                                                               
DOEUNT NTR1                                                                     
         CLC   TWTYPE,=CL3'MVU'                                                 
         BE    DOEUNX                                                           
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
*SET UNIT STATUS 3                                                              
         GOTO1 VHELLO,DMCB,(C'G',=C'UNTFIL  '),(X'02',AIOAREA1),0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NUSDRD,R6                                                        
         OC    NUSDST3,TWUNST3                                                  
         DROP  R6                                                               
*CHECK FOR CHANGE ACTION                                                        
         L     R4,UPACURUN                                                      
         USING NUNTD,R4                                                         
*                                                                               
         CLI   NUNTACTN,C'C'                                                    
         BNE   DOEUN040                                                         
         BAS   RE,DOCHNG                                                        
         B     DOEUNX                                                           
*                                                                               
DOEUN040 MVC   TWSVDATE,NUNTWOD                                                 
*                                                                               
*        OC    TWPRMIR,TWPRMIR     COMMENT THIS MIRROR LOGIC OUT                
*        BZ    DOEUN200            FOR NOW                                      
         B     DOEUN200                                                         
*--HANDLE MIRROR ADD                                                            
         MVI   TWLSTLIN,0           PROGRAM NOT FOUND, ADD IT NOW               
DOEUN060 BAS   RE,GETPROG                                                       
         CLI   TWERROR,0            INPUTTED CODE NOT FOUND                     
         BNE   DOEUN500                                                         
         CLI   TWLSTLIN,X'FF'       PROGRAM NOT FOUND, ADD IT NOW               
         BE    DOEUN080                                                         
         BAS   RE,GETLAST                                                       
         CLI   TWLSTLIN,191         SEE IF ROOM UNDER PROGRAM                   
         BNH   DOEUN100                                                         
         B     DOEUN060             GET NEXT VALID PROGRAM RECORD               
DOEUN080 BAS   RE,ADDPROG                                                       
*                                                                               
*                                                                               
*--IF PROGRAM CODE INPUTTED MOVE VPH'S TO THE UNIT                              
DOEUN100 CLI   TWPRINPC,C'Y'                                                    
         BNE   DOEUN120                                                         
*        BAS   RE,TRAPPROG                                                      
         GOTO1 =A(OVFLRTN),DMCB,(6,DUB),(RC),(R9),RR=MYRELO   TRAPPROG          
DOEUN120 MVC   DUB(2),TWPRSTIM                                                  
         BAS   RE,GETSQH           GET QUARTER HOUR                             
         MVC   NUKTIME,TWQTRHR                                                  
         MVC   NUKPROG,TWPRCODE                                                 
         MVC   NUNTPROG,TWPRCODE                                                
         MVC   NUNTADAT,TWPDATE                                                 
         MVC   NUNTLINE,TWLSTLIN                                                
         MVC   NUKSUB,TWLSTLIN                                                  
         MVC   KEY(20),NUKEY                                                    
         GOTO1 =A(OVFLRTN2),DMCB,(1,DUB),(RC),(R9),RR=MYRELO   WRTREC           
******   BAS   RE,WRTREC                                                        
*--CREATE SECOND RECORD                                                         
         LA    R5,1(R5)                                                         
         STC   R5,NUKSUB                                                        
         STC   R5,TWLSTLIN                                                      
         STC   R5,NUNTLIN2                                                      
*--CALCULATE SECOND MIRROR TIME                                                 
*                                                                               
         MVC   DUB(2),TWPRMIR                                                   
         BAS   RE,GETSQH           GET MIRROR QUARTER HOUR                      
         MVC   NUKTIME,TWQTRHR                                                  
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,3,TWPRMIR                                                     
         ICM   RF,3,NUTIME                                                      
         SR    RE,RF               MIR ST - ST TIME = TIME DIFF                 
         MVC   NUTIME(2),TWPRMIR                                                
         SR    RF,RF                                                            
         ICM   RF,3,NUTIME+2                                                    
         AR    RF,RE               END TIME + TIME DIFF = MIRROR END            
         CH    RF,=H'2400'                                                      
         BNH   *+8                                                              
         SH    RF,=H'2400'                                                      
         STCM  RF,3,NUTIME+2                                                    
         MVC   KEY(20),NUKEY                                                    
         GOTO1 =A(OVFLRTN2),DMCB,(1,DUB),(RC),(R9),RR=MYRELO   WRTREC           
***      BAS   RE,WRTREC                                                        
         LA    R5,1(R5)                                                         
         STC   R5,TWLSTLIN                                                      
         B     DOEUNX                                                           
*--NON MIRROR ADD                                                               
DOEUN200 MVI   TWLSTLIN,0                                                       
DOEUN210 BAS   RE,GETPROG                                                       
         CLI   TWERROR,0            INPUTTED PROGRAM CODE NOT FOUND             
         BNE   DOEUN500                                                         
         CLI   TWLSTLIN,X'FF'       MAX REAC0ED FOR THIS CODE                   
         BE    DOEUN260                                                         
         BAS   RE,GETLAST                                                       
         CLI   TWLSTLIN,191         SEE IF ROOM UNDER PROGRAM                   
         BNH   DOEUN300                                                         
         B     DOEUN210             GET NEXT VALID PROGRAM RECORD               
DOEUN260 BAS   RE,ADDPROG                                                       
*                                                                               
*                                                                               
*--IF PROGRAM CODE INPUTTED MOVE VPH'S TO THE UNIT                              
DOEUN300 CLI   TWPRINPC,C'Y'                                                    
         BNE   DOEUN320                                                         
*        BAS   RE,TRAPPROG                                                      
         GOTO1 =A(OVFLRTN),DMCB,(6,DUB),(RC),(R9),RR=MYRELO   TRAPPROG          
DOEUN320 MVC   DUB(2),TWPRSTIM                                                  
         BAS   RE,GETSQH           GET QUARTER HOUR                             
         MVC   NUKTIME,TWQTRHR                                                  
         MVC   NUKPROG,TWPRCODE                                                 
         MVC   NUNTPROG,TWPRCODE                                                
         MVC   NUNTADAT,TWPDATE                                                 
         MVC   NUNTLINE,TWLSTLIN                                                
         MVC   NUKSUB,TWLSTLIN                                                  
         MVC   KEY(20),NUKEY                                                    
         GOTO1 =A(OVFLRTN2),DMCB,(1,DUB),(RC),(R9),RR=MYRELO   WRTREC           
***      BAS   RE,WRTREC                                                        
         B     DOEUNX                                                           
*--ERROR CONDITION                                                              
DOEUN500 BAS   RE,BYUNIT                                                        
         B     XIT                                                              
*                                                                               
DOEUNX   B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
*--GET LAST LINE NUMBER FOR 84 KEY                                              
*                                                                               
GETLAST  NTR1                                                                   
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
         L     R4,UPACURUN                                                      
         USING NUNTD,R4                                                         
         MVI   TWLSTLIN,0                                                       
         BAS   RE,GETKEY84                                                      
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         B     GTLS150                                                          
GTLS100  GOTO1 AIO,DMCB,UNT+DIR+SEQ                                             
GTLS150  CLC   KEY(17),KEYSAVE                                                  
         BNE   GTLS200                                                          
         MVC   TWLSTLIN,KEY+17                                                  
         B     GTLS100                                                          
GTLS200  ZIC   R5,TWLSTLIN                                                      
         LA    R5,1(R5)                                                         
         STC   R5,TWLSTLIN                                                      
         STC   R5,NUNTLINE                                                      
         STC   R5,NUKSUB                                                        
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
*--HANDLE END OF UNIT                                                           
*                                                                               
DOCHNG   NTR1                                                                   
         MVI   TWTIMCH,C'N'                                                     
*                                                                               
         L     R4,UPACURUN                                                      
         USING NUNTD,R4                                                         
*                                                                               
         USING NURECD,R5                                                        
*                                                                               
         LA    R3,NUNTLINE                                                      
*                                                                               
         MVC   DUB(2),TWPRSTIM                                                  
         BAS   RE,GETSQH                                                        
*                                                                               
*-READ 84 KEY CHECK FOR TIME CHANGE                                             
DOCHN050 LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TWLINHLD,TWLSTLIN   SAVE LINE                                    
         MVC   TWPRGHLD,TWPRCODE   AND PROGRAM CODE                             
         MVC   TWLSTLIN,0(R3)                                                   
         MVC   TWPRCODE,NUNTPROG                                                
*                                                                               
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,TWAGYMED                                                  
         MVC   NUKPCLT,TWCLIENT                                                 
         MVC   NUKPNET,TWNET                                                    
         MVC   NUKPPROG,NUNTPROG                                                
         MVC   NUKPDATE,TWPDATE                                                 
         MVC   NUKPEST,TWEST                                                    
         MVC   NUKPSUB,0(R3)                                                    
         MVC   NUKPDP,TWDYPT                                                    
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+16                                                             
         MVI   NUNTERNO+1,53                                                    
         MVI   NUNTERF,1                                                        
         B     DOCHN500                                                         
*-GET THE RECORD                                                                
         L     R5,AIOAREA3                                                      
         GOTO1 AIO,DMCB,UNT+FILE+GET+UPDATE,AIOAREA3                            
*--CHECK FOR TIME CHANGE                                                        
         CLC   NUKTIME,TWQTRHR                                                  
         BE    *+8                                                              
         B     DOCHN200                                                         
*-NO TIME CHANGE WRITE NEW RECORD BACK                                          
         L     R5,AIOAREA1                                                      
         MVC   NUKTIME,TWQTRHR                                                  
         MVC   NUKPROG,NUNTPROG                                                 
         MVC   NUKSUB,0(R3)                                                     
         GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA1                                   
*-CHECK NEXT LINE                                                               
         CLC   0(1,R3),NUNTLINE    ARE WE UP TO 2ND LINE                        
         BNE   DOCHNX              YES EXIT                                     
         LA    R3,1(R3)                                                         
         CLI   0(R3),0             ARE 2 LINES INPUTTED                         
         BE    DOCHNX                                                           
         MVC   DUB(2),TWPRMIR                                                   
         BAS   RE,GETSQH           GET QUARTER HOUR FOR MIRROR                  
         B     DOCHN050            YES PROCESS 2ND LINE                         
*-TIME CHANGE KEYS MUST BE DELETED AND REBUILT                                  
*                                                                               
DOCHN200 MVC   FULL(1),NUDAY                                                    
         MVC   FULL+1(1),NUKTIME                                                
         MVI   TWTIMCH,C'Y'                                                     
*--DELETE THE RECORD                                                            
         OI    NURSTAT,X'80'                                                    
         GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA3                                   
*--DELETE THE 84 KEY                                                            
         LA    R5,KEY                                                           
*        OI    NUKSTAT,X'80'                                                    
*        GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
*-BUILD 04 KEY                                                                  
         XC    KEY,KEY                                                          
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,TWAGYMED                                                   
         MVC   NUKCLT,TWCLIENT                                                  
         MVC   NUKDATE,TWPDATE                                                  
         MVC   NUKTIME,FULL+1                                                   
         MVC   NUKNET,TWNET                                                     
         MVC   NUKPROG,NUNTPROG                                                 
         MVC   NUKEST,TWEST                                                     
         MVC   NUKSUB,0(R3)                                                     
         MVC   NUKDP,TWDYPT                                                     
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE   *+6                                                               
         DC   H'0'                                                              
*--DELETE THE 04 KEY                                                            
         LA    R5,KEY                                                           
         OI    NUKSTAT,X'80'                                                    
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
*-BUILD 94 KEY                                                                  
         XC    KEY,KEY                                                          
         MVI   NUKDTYPE,X'94'                                                   
         MVC   NUKDAM,TWAGYMED                                                  
         MVC   NUKDCLT,TWCLIENT                                                 
         MVC   NUKDEST,TWEST                                                    
         MVC   NUKDNET,TWNET                                                    
         MVC   NUKDTIME,FULL+1                                                  
         MVC   NUKDPROG,NUNTPROG                                                
         MVC   NUKDDATE,TWPDATE                                                 
         MVC   NUKDSUB,0(R3)                                                    
*-GET DAY                                                                       
         LA    RE,ROTTABLE                                                      
         LA    RF,9                                                             
DOCHN300 CLC   FULL(1),3(RE)                                                    
         BE    DOCHN320                                                         
         LA    RE,6(RE)                                                         
         BCT   R9,DOCHN300                                                      
         DC    H'0'                                                             
*                                                                               
DOCHN320 MVC   NUKDDAY,5(RE)                                                    
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE   *+6                                                               
         DC   H'0'                                                              
*--DELETE THE 94 KEY                                                            
         LA    R5,KEY                                                           
         OI    NUKSTAT,X'80'                                                    
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
*-WRITE THE RECORD BACK                                                         
*                                                                               
         L     R5,AIOAREA1                                                      
         MVC   NUKTIME,TWQTRHR     SET QUARTER HOUR TIME IN KEY                 
         MVC   NUKPROG,NUNTPROG                                                 
         MVC   NUKSUB,0(R3)                                                     
         MVC   KEY(20),NUKEY                                                    
         GOTO1 =A(OVFLRTN2),DMCB,(1,DUB),(RC),(R9),RR=MYRELO   WRTREC           
***      BAS   RE,WRTREC           ADD NEW RECORD                               
*                                                                               
         MVI   TWTIMCH,C'N'                                                     
         CLC   0(1,R3),NUNTLINE    ARE WE ON FIRST LINE                         
         BNE   DOCHNX              NO SET FOR MIRROR LINE                       
         LA    R3,1(R3)                                                         
         CLI   0(R3),0             IS THERE TWO LINES                           
         BNH   DOCHNX              NO EXIT                                      
         MVC   DUB(2),TWPRMIR                                                   
         BAS   RE,GETSQH           GET QUARTER HOUR FOR MIRROR                  
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,3,TWPRMIR                                                     
         ICM   RF,3,NUTIME                                                      
         SR    RE,RF               MIR ST - ST TIME = TIME DIFF                 
         MVC   NUTIME(2),TWPRMIR                                                
         SR    RF,RF                                                            
         ICM   RF,3,NUTIME+2                                                    
         AR    RF,RE               END TIME + TIME DIFF = MIRROR END            
         CH    RF,=H'2400'                                                      
         BNH   *+8                                                              
         SH    RF,=H'2400'                                                      
         STCM  RF,3,NUTIME+2                                                    
*                                                                               
         B     DOCHN050            YES DO MIRROR                                
*                                                                               
DOCHNX   MVI   NUNTERNO+1,0                                                     
         MVI   NUNTERF,0                                                        
DOCHN500 MVC   TWLSTLIN,TWLINHLD   RESTORE LINE                                 
         MVC   TWPRCODE,TWPRGHLD   RESTORE PROGRAM CODE                         
         XIT                                                                    
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
*--BUILD TRAFFIC ELEMENT                                                        
*                                                                               
DOCOMM NTR1                                                                     
*                                                                               
         USING NCOMD,R4            COMMENT DSECT                                
*                                                                               
DOCMMX   MVI   TWERROR,0                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* THIS ROUTINE:                                                                 
*    BUILDS A CORRESPONDING IMPRESSION OVERRIDE ELEMENT                         
*    FOR EVERY VPH ELEMENT.                                                     
*                                                                               
*   INPUT P1 BYTE 1 ELEMENT CODE                                                
*         P1 BYTE 2-4 A(DEMO LIST)                                              
*         P2 BYTE 1 DEMO MODIFIER                                               
*         P2 BYTE 2-4 A(DEMO VALUES)                                            
*                                                                               
*         AIOAREA POINTS TO THE UNIT RECORD                                     
BUILDDEM NTR1                                                                   
*                                                                               
         MVC   FULL(1),0(R1)                                                    
         MVC   FULL+1(1),4(R1)                                                  
         MVC   FULL+2(1),4(R1)                                                  
         CLI   FULL+2,C'I'                                                      
         BNE   *+8                                                              
         MVI   FULL+2,C'T'         HOLD NUOVMOD                                 
*                                                                               
         L     R4,0(R1)            DEMO CATEGORIES                              
         L     R2,4(R1)            DEMO VALUES                                  
         XC    WORKAREA(50),WORKAREA                                            
         LA    R5,WORKAREA                                                      
         USING NUOVD,R5                                                         
         MVC   NUOVEL,0(R1)                                                     
         MVI   NUOVLEN,X'0C'                                                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'UNTFIL  '),(FULL,AIOAREA1),0                
*                                                                               
*--BUILD HOME IMPRESSION ELEMENT IF INPUTTED                                    
*                                                                               
         MVC   NUOVCAT(3),=XL3'00C801'    HOMES IMPS C'H'                       
         MVI   NUOVPRE,X'42'       CABLE PREFIX                                 
         CLI   TWPSTTYP,C'N'                                                    
         BE    *+12                                                             
         CLI   TWPSTTYP,C'S'                                                    
         BNE   *+14                                                             
         MVC   NUOVCAT(3),=XL3'00E301'    HOMES IMPS C'T'                       
         MVI   NUOVPRE,X'43'       NETWORK PREFIX                               
         MVC   TWHOMES,0(R2)       USED TO BUILD IMPS                           
         CLI   0(R2),X'FF'                                                      
         BE    BLDD0200                                                         
         MVC   NUOVVAL,0(R2)                                                    
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD OVERRIDE ELEMENT                         
         BAS   RE,PUTUNEL                                                       
******   GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
*                                                                               
BLDD0200 LA    R2,4(R2)                                                         
         CLI   FULL+1,C'I'                                                      
         BE    BLDD0250                                                         
         MVC   NUOVMOD,FULL+1                                                   
         MVC   NUOVPRE,DEMPREC+1   RATING PRECISSION                            
         CLI   FULL+1,C'R'                                                      
         BE    BLDD0250                                                         
         MVC   NUOVPRE,DEMPREC+13  VPH PRECISSION                               
*                                                                               
BLDD0250 LA    R6,18                                                            
*                                                                               
BLDD0270 CLI   0(R2),X'FF'         IF ZERO VALUE BYPASS                         
         BE    BLDD0360                                                         
         OC    0(4,R2),0(R2)       IF ZERO VALUE BYPASS                         
         BZ    BLDD0360                                                         
         CLI   0(R4),X'FF'         END OF DEMO LIST                             
         BZ    BLDDX                                                            
         MVC   ENCDEMO,0(R4)                                                    
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)  DECODE DEMO                            
         MVC   DECDEMO+1(1),FULL+2       DEFAULT NUOVMOD                        
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)  ENCODE DEMO                            
         MVC   NUOVCAT(3),ENCDEMO                                               
         CLI   NUOVCAT,0           CHECK FOR NAD                                
         BE    *+8                                                              
         OI    NUOVFLG,X'80'                                                    
         MVC   NUOVVAL,0(R2)                                                    
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD OVERRIDE ELEMENT                         
         BAS   RE,PUTUNEL                                                       
*******  GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         BAS   RE,BLDIMP                                                        
*                                                                               
BLDD0360 LA    R4,3(R4)                                                         
         LA    R2,4(R2)                                                         
         BCT   R6,BLDD0270                                                      
*                                                                               
BLDDX    B     XIT                                                              
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    BUILDS A CORRESPONDING IMPRESSION OVERRIDE ELEMENT                         
*    FOR EVERY VPH ELEMENT.                                                     
*                                                                               
*   INPUT WORKAREA = VPH OVERIDE ELEMENT                                        
*                                                                               
BLDIMP   NTR1                                                                   
         LA    R6,WORKAREA+20                                                   
         USING NUOVD,R6                                                         
         MVC   WORKAREA+20(12),WORKAREA   COPY OVERRIDE ELEM                    
*                                                                               
         CLI   DECDEMO+1,C'V'            CHECK FOR VPH                          
         BNE   BLDIMPEX                                                         
*                                                                               
         CLI   TWHOMES,X'FF'       CHECK FOR INPUTTED HOMES                     
         BE    BLDIMPEX                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R1,15,TWHOMES                                                    
         ICM   RF,15,NUOVVAL                                                    
*                                                                               
         MR    R0,RF               HOMES*VPH                                    
         M     R0,=F'10'                                                        
         D     R0,=F'1000'                                                      
         A     R1,=F'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
*                                                                               
         STCM  R1,15,NUOVVAL                                                    
*                                                                               
         MVI   DECDEMO+1,C'T'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)  ENCODE DEMO                            
         MVC   NUOVCAT(3),ENCDEMO                                               
         MVI   NUOVPRE,X'42'                                                    
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'DD' ELEMENT                            
         PRINT GEN                                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,(R6),0                  
         PRINT NOGEN                                                            
*                                                                               
BLDIMPEX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* THIS ROUTINE:                                                                 
*    CHECKS TO SEE IF UNIT IS PAID OR BILLED                                    
*                                                                               
*   INPUT P1 BYTE 1 PAY TYPE C'T', C'I', C'S' X'FF'=ANY PAY                     
*                                                                               
*         AIOAREA POINTS TO THE UNIT RECORD                                     
CKPAID   NTR1                                                                   
*                                                                               
         MVC   BYTE,0(R1)                                                       
*--SET UP OPTIONAL SEARCH                                                       
         XC    DMCB+8(4),DMCB+8                                                 
         CLI   BYTE,X'FF'                                                       
         BE    CKPD020                                                          
         LA    RE,BYTE                                                          
         ST    RE,DMCB+8                                                        
         MVI   DMCB+8,1                                                         
*                                                                               
CKPD020  GOTO1 VHELLO,DMCB,(C'G',=C'UNTFIL  '),(X'12',AIOAREA1)                 
         CLI   12(R1),0                                                         
*                                                                               
CKPDEX   B     XIT                                                              
*                                                                               
*                                                                               
*--IF ERROR IN UNIT,UDRE,UDRA,SPCH,PROD,CMMT,COMM.                              
*--BYPASS ALL RECORDS UNDER THIS UNIT. READ TEMPTSTR                            
*--UNTIL THE NEXT UNIT RECORD IS FOUND. IF EOF, DEAL                            
*--PACKAGE OR PROGRAM RECORD IS FOUND FIRST EXIT.                               
*                                                                               
BYUNIT   NTR1                                                                   
*                                                                               
BYUNT020 MVI   TWERROR,INVERR                                                   
BYUNT100 BAS   RE,NEXT                                                          
         L     R4,UPACUROB                                                      
         CLC   12(4,R4),=CL4'PROG'                                              
         BE    BYUNTX                                                           
         CLC   12(4,R4),=CL4'UNIT'                                              
         BE    BYUNTX                                                           
         CLC   12(4,R4),=CL4'UNUP'                                              
         BE    BYUNTX                                                           
         CLC   12(4,R4),=CL4'DEAL'                                              
         BE    BYUNTX                                                           
         CLC   12(4,R4),=CL4'PKG*'                                              
         BE    BYUNTX                                                           
         CLC   12(4,R4),=CL4'EPKG'                                              
         BE    BYUNTX                                                           
         CLC   12(4,R4),=CL4'EOD*'                                              
         BE    BYUNTX                                                           
         CLC   12(4,R4),=CL4'EOF*'                                              
         BE    BYUNTX                                                           
         B     BYUNT100                                                         
*-EXIT                                                                          
BYUNTX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
*-THIS ROUTINE SETS UP THE SPOTFILE                                             
*-SETS UP THE PROGRAM KEY DOES A READ                                           
*-HIGH FOR THE PROGRAM RECORD.                                                  
READPROG NTR1                                                                   
         LA    R3,KEY                                                           
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,TWAGYMED                                                  
         MVC   NPGKNET,TWMKT                                                    
         MVC   NPGKPROG(6),TWPRCODE                                             
*                                                                               
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*--THIS WRITES LAST TEMPSTR PAGE AND EXITS THE PROGRAM                          
*                                                                               
DOEPKG   NTR1                                                                   
         BAS   RE,PACKUPDT                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*--THIS WRITES LAST TEMPSTR PAGE AND EXITS THE PROGRAM                          
*                                                                               
DOEOF    NTR1                                                                   
         BAS   RE,WRTTWA                                                        
*                                                                               
         MVI   UPCURTWA,2                                                       
         BAS   RE,WRTTW2                                                        
         B     XIT                                                              
         EJECT                                                                  
*          DATA SET CTMAD0E    AT LEVEL 174 AS OF 09/30/93                      
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    CHECKS TO SEE IF ANY UNITS WRITTEN IF YES CHANGES THE CONTROL              
*    BIT TO SAY PACKAGE HAS UNITS UNDER IT.                                     
*                                                                               
PACKUPDT NTR1                                                                   
         CLI   TWRITESW,C'Y'                                                    
         BNE   PACKUPEX                                                         
*                                                                               
         MVI   TWRITESW,C'N'                                                    
*                                                                               
*--GET A PACKAGE RECORD                                                         
*                                                                               
         MVC   KEY(20),TWPKEY                                                   
         GOTO1 AIO,DMCB,UNT+DIR+HIGH+UPDATE                                     
*        CLC   KEY(20),KEYSAVE                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
         GOTO1 AIO,DMCB,UNT+FILE+GET+UPDATE,AIOAREA1                            
*                                                                               
         L     R3,AIOAREA1                                                      
         USING NPRECD,R3                                                        
*                                                                               
         NI    NPAKCNTL,X'DF'      UNITS EXIST UNDER PACKAGE                    
         NI    NPAKCNTL,X'F7'      TAKE CABLE LOCK OFF                          
         OI    NPAKCNTL,X'10'      CABLE UPLOAD PACKAGE                         
         CLC   TWTYPE,=CL3'MVU'                                                 
         BE    *+8                                                              
         OI    NPAKSTAT,X'20'      PACKAGE IS LOCKED                            
         GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA1                                   
*                                                                               
PACKUPEX B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*-SUB-ROUTINE TO CHECK CHARACTERS FOR NUMERIC                                   
*                                                                               
CHKNUM   NTR1                                                                   
         ZIC   R4,0(R1)            NUMBER OF BYTES                              
         L     R6,0(R1)            ADDRESS OF LOCATION                          
         LTR   R4,R4                                                            
         BZ    CHKNERR                                                          
*                                                                               
CHKN020  CLI   0(R6),C'0'                                                       
         BL    CHKNERR                                                          
         CLI   0(R6),C'9'                                                       
         BH    CHKNERR                                                          
         LA    R6,1(R6)                                                         
         BCT   R4,CHKN020                                                       
*                                                                               
         SR    R6,R6               SET ZERO RETURN CODE                         
*                                                                               
CHKNERR  LTR   R6,R6                                                            
         B     XIT                                                              
         EJECT                                                                  
*-SUB-ROUTINE TO CONVERT MILITARY TIME TO START QUARTER HOUR                    
*                                                                               
GETSQH   NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,3,DUB            START TIME                                   
         SR    R0,R0                                                            
         D     R0,=F'100'          R1=HOURS, R0=REMAINDER MINUTES               
         MH    R1,=H'60'           CONVERT HOURS TO MINUTES                     
         AR    R1,R0               SUM TOTAL MINUTES                            
         CH    R1,=H'360'          TEST FOR LESS THAN 6 AM                      
         BNL   *+8                                                              
         AH    R1,=Y(60*24)        ADD MINUTES OF 24 HOURS                      
         SH    R1,=H'360'          SUBTRACT 6 HOURS TO BASE OFF 6AM             
         SR    R0,R0                                                            
         D     R0,=F'15'           DIVIDE BY MINUTES IN A QUARTER HOUR          
         STC   R1,TWQTRHR                                                       
         B     XIT                                                              
         EJECT                                                                  
*-SUB-ROUTINE TO BUILD THE 84 KEY                                               
*                                                                               
GETKEY84 NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING NURECD,RE                                                        
*        L     RF,AIOAREA1                                                      
*        USING NURECD,RF                                                        
*                                                                               
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,TWAGYMED                                                  
         MVC   NUKPCLT,TWCLIENT                                                 
         MVC   NUKPNET,TWNET                                                    
         MVC   NUKPPROG,TWPRCODE                                                
         MVC   NUKPDATE,TWPDATE                                                 
         MVC   NUKPEST,TWEST                                                    
         MVC   NUKPSUB,TWLSTLIN                                                 
         MVC   NUKPDP,TWDYPT                                                    
*                                                                               
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOCAL WORKAREA FOR TABLES, ETC                                                
LENTWA   DC    H'18432'                                                         
TEMPSTR  DC    C'TEMPSTR'                                                       
DMREAD   DC    C'DMREAD '                                                       
DMWRITE  DC    C'DMWRT  '                                                       
         DS    0D                                                               
*                                                                               
*--OVERFLOW ROUTINES                                                            
*                                                                               
         DS    0F                                                               
         DROP  RA,RB,RC                                                         
OVFLRTN  NMOD1 0,**40OV**                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING OVFLRTN+4096,RA                                                  
         L     RC,4(R1)                                                         
         L     R9,8(R1)                                                         
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         L     R5,AIOAREA4                                                      
         USING BUYVALD,R5                                                       
         MVC   NBAIO,AIOAREA3                                                   
                                                                                
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRANCH(RF)                                                     
*                                                                               
OVBRANCH B     VALAGY                                                           
         B     VALCLI                                                           
         B     VALEST                                                           
         B     VALPKG                                                           
         B     GETSTA                                                           
         B     BILDOV                                                           
         B     TRAPPROG                                                         
         B     BLDUNIV                                                          
         B     SETDEMO                                                          
         B     RESETPKG                                                         
         EJECT                                                                  
* START BY GETTING AGENCY VALUES                                                
*                                                                               
VALAGY   MVC   NBSELAGY,TWAGY                                                   
         MVI   NBSELMED,C'N'                                                    
         MVI   NBSELMOD,NBVALAGY   READ AGENCY RECORD                           
         MVC   NBAIO,AIOAREA1                                                   
         MVC   NBACOM,ACOMFACS                                                  
         MVC   NBAUTH,TWAAUTH                                                   
         PRINT GEN                                                              
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         PRINT NOGEN                                                            
         CLI   NBERROR,NBGOOD      TEST FOR ERROR                               
         BE    *+12                                                             
         MVI   TWERROR,INVERR                                                   
         B     OVXIT                                                            
*                                                                               
         MVC   AGENCY,AGYALPH                                                   
         MVC   AGYMED,NBACTAM                                                   
         MVC   TWAGYMED,AGYMED                                                  
         L     R4,NBAIO                                                         
         USING AGYHDRD,R4                                                       
         MVC   AGYPRO,AGYPROF      EXTRACT PROFILE                              
         MVI   TWERROR,0                                                        
         B     OVXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE CLIENT                                                               
*                                                                               
VALCLI   MVI   TWERROR,INVERR                                                   
         GOTO1 VCLPACK,DMCB,TWCLI,CLIPK                                         
         CLI   DMCB,X'FF'          TEST FOR ERROR                               
         BE    OVXIT                                                            
*--READ CLIENT RECORD                                                           
         XC    KEY(20),KEY                                                      
         MVC   KEY+1(1),TWAGYMED                                                
         MVC   KEY+2(2),CLIPK                                                   
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OVXIT                                                            
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA1                                   
         SPACE 1                                                                
VALCLI1  MVC   CLI,TWCLI           ALPHA CLIENT                                 
         MVC   CLIENT,CLIPK                                                     
         MVC   TWCLIENT,CLIPK                                                   
*                                                                               
         L     R4,AIOAREA1                                                      
         USING CLTHDRD,R4                                                       
*                                                                               
         OC    TWAACCS(2),TWAACCS  TEST FOR ANY SECURITY LIMITS                 
         BZ    VALCLI4                                                          
         CLI   TWAACCS,C'+'        TEST FOR MARKET LOCKOUT                      
         BE    VALCLI4             NO CHECK                                     
         MVI   TWERROR,SCTYERR                                                  
         CLI   TWAACCS,C'$'        TEST FOR OFFICE LIMIT                        
         BE    VALCLI2                                                          
         CLI   TWAACCS,C'*'        TEST FOR OFFICE LIMIT                        
         BE    VALCLI3                                                          
         CLC   TWAACCS(2),CLIPK    TEST FOR FILTERED CLIENT                     
         BE    VALCLI4             OK                                           
         B     OVXIT                                                            
         SPACE 1                                                                
*                                                                               
VALCLI2  DS    0H               * TEST OFFICE LIST SECURITY *                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         L     RF,VCALLOV                                                       
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,TWAACCS     ID AUTH VALUE                                
         MVC   OFCLMT,TWAACCS                                                   
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,ACOMFACS,0                                         
         CLI   0(R1),0                                                          
         BNE   OVXIT                                                            
         B     VALCLI4                                                          
*                                                                               
VALCLI3  CLC   TWAACCS+1(1),COFFICE TEST FOR FILTERED OFFICE                    
         BNE   OVXIT                                                            
         SPACE 1                                                                
VALCLI4  MVI   TWERROR,CLIFRERR     CHECK FROZEN CLIENT                         
         TM    COPT2,X'08'                                                      
         BO    OVXIT                                                            
*                                                                               
         MVC   CLIPRO,CPROF                                                     
         MVC   CLIEXTRA,CEXTRA                                                  
         MVC   CLIOPT2,COPT2                                                    
         MVC   CLICOST2,CCOST2                                                  
         MVC   CLICPRD,CPRPRD                                                   
         LA    RE,CLILIST                                                       
         LA    RF,880                                                           
         LR    R1,RF                                                            
         LA    R0,CLIST            PRODUCT LIST                                 
         MVCL  RE,R0               MOVE IT TO BUY VALUES                        
*                                                                               
         LA    RE,CLILIST                                                       
         AH    RE,=H'880'                                                       
         LA    RF,140                                                           
         LR    R1,RF                                                            
         LA    R0,CLIST2           EXTENDED PRODUCT LIST                        
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,TWCLIPRD                                                      
         LA    RF,880                                                           
         LR    R1,RF                                                            
         LA    R0,CLIST            PRODUCT LIST                                 
         MVCL  RE,R0               MOVE IT TO BUY VALUES                        
*                                                                               
         LA    RE,TWCLIPRD                                                      
         AH    RE,=H'880'                                                       
         LA    RF,140                                                           
         LR    R1,RF                                                            
         LA    R0,CLIST2           EXTENDED PRODUCT LIST                        
         MVCL  RE,R0                                                            
*                                                                               
         LA    RF,RATETAB                                                       
VALCLI6  CLI   0(RF),X'40'                                                      
         BE    VALCLI10                                                         
         CLC   CPROF+14(1),0(RF)                                                
         BE    VALCLI7                                                          
         LA    RF,2(RF)                                                         
         B     VALCLI6                                                          
*                                                                               
VALCLI7  MVC   TWRATE,1(RF)        GET RATE TYPE                                
         CLI   CEXTRA+14,C'0'                                                   
         BE    *+10                                                             
         MVC   TWCVRGE,CEXTRA+14   GET COVERAGE FACTOR                          
*                                                                               
VALCLI10 XC    KEY,KEY                                                          
         MVC   KEY(4),=C'S0N1'     GET BUY PROGRAM PROFILE                      
         MVC   KEY+4(2),AGYALPH                                                 
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),CLI                                                     
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),COFFICE                                                
         GOTO1 VGETPROF,DMCB,KEY,BUYPROF,VDATAMGR                               
         MVC   KEY(4),=C'S0N2'     GET BUY PROGRAM PROFILE EXTENSION            
         GOTO1 VGETPROF,DMCB,KEY,BUYPROF2,VDATAMGR                              
         MVC   KEY(4),=C'S0N0'     GET NETWORK PROFILE                          
         GOTO1 VGETPROF,DMCB,KEY,NETPROF,VDATAMGR                               
         SPACE 1                                                                
VALCLIX  MVI   TWERROR,0                                                        
         B     OVXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE ESTIMATE                                                             
*                                                                               
VALEST   MVI   TWERROR,INVERR                                                   
         CLI   TWEST,0                                                          
         BE    OVXIT                                                            
         CLI   TWEST,255                                                        
         BH    OVXIT                                                            
         BAS   RE,ESTNUM                                                        
         STC   R0,EST                                                           
         STC   R0,EST+1                                                         
         MVI   ESTTYP,ISINGLE                                                   
         SPACE                                                                  
VALEST10 MVC   ESTIMATE,EST                                                     
         SPACE 1                                                                
*--READ ESTIMATE RECORD                                                         
         XC    KEY(20),KEY                                                      
         MVC   KEY+1(1),TWAGYMED                                                
         MVC   KEY+2(2),TWCLIENT                                                
         MVC   KEY+4(3),=CL3'POL'                                               
         MVC   KEY+7(1),TWEST                                                   
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OVXIT                                                            
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA3                                   
         SPACE 1                                                                
*                                  OUTPUT ESTIMATE VALUES ON SCREEN             
VALEST13 L     R4,AIOAREA3                                                      
         USING ESTHDRD,R4                                                       
         MVI   TWERROR,ELOCKERR    TEST FOR LOCKED ESTIMATE                     
         TM    ECNTRL,X'08'                                                     
         BO    OVXIT                                                            
         MVC   ESTSTART,ESTART     ESTIMATE START (YYMMDD)                      
         MVC   TWESSTRT,ESTART     ESTIMATE START (YYMMDD)                      
         MVC   ESTEND,EEND         ESTIMATE END (YYMMDD)                        
         MVC   TWESEND,EEND        ESTIMATE END (YYMMDD)                        
         PRINT GEN                                                              
         GOTO1 VDATCON,DMCB,(0,ESTART),(2,ESTS)                                 
         GOTO1 VDATCON,DMCB,(0,EEND),(2,ESTE)                                   
         PRINT NOGEN                                                            
         MVC   ESTBOOK,EBOOK       HUT BOOK                                     
*                                                                               
VALEST14 MVC   ESTDEMSE,EDEMLST                                                 
         MVC   ESTDEMSE+60(3),EDEM21                                            
         MVC   ESTWLST,EWGTLST                                                  
         MVC   ESTWLST+20(1),EDEM21WT                                           
         MVC   ESTUSNS,EUSRNMS                                                  
         MVC   ESTWNAM,EWGTNM                                                   
         MVC   ESTFILT,EPROF                                                    
         MVC   ESTSREP,EREP                                                     
         MVC   ESTRATE,ERATE                                                    
         MVC   ESTRATEC,ERATECST                                                
         MVC   ESTCOST2,ECOST2                                                  
*                                                                               
         LA    RE,DBLOCKA                                                       
         L     RF,=F'256'                                                       
         XCEF                                                                   
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         GOTO1 VDEMOCON,DMCB,(50,ESTDEMSE),('DEMOCON_17',ESTDEMS),     X        
               (C'S',DBLOCK),ESTDEMPL                                           
         DROP  RF                                                               
*                                                                               
         LA    R0,21               MAXIMUM DEMOS                                
         SR    R1,R1               COUNTER                                      
         LA    R2,ESTDEMSE                                                      
VALEST15 OC    0(3,R2),0(R2)                                                    
         BZ    *+16                                                             
         LA    R1,1(R1)            INCREMENT DEMO COUNT                         
         LA    R2,3(R2)            NEXT DEMO                                    
         BCT   R0,VALEST15                                                      
         STC   R1,ESTNDEMS                                                      
         STC   R1,TWESNDEM                                                      
         MVC   TWESDEMS,ESTDEMSE                                                
*                                                                               
         LA    RF,RATETAB                                                       
VALEST20 CLI   0(RF),X'40'                                                      
         BE    VALESTX                                                          
         CLC   ERATE,0(RF)                                                      
         BE    VALEST25                                                         
         LA    RF,2(RF)                                                         
         B     VALEST20                                                         
*                                                                               
VALEST25 MVC   TWRATE,1(RF)        GET RATE TYPE                                
         MVC   TWCVRGE,ERATECST    GET COVERAGE                                 
*                                                                               
         SPACE 1                                                                
VALESTX  XC    TWERROR,TWERROR                                                  
         B     OVXIT                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO PROCESS ESTIMATE NUMBER (AT ENTRY TWEST HAS NUMBER)            
         SPACE 1                                                                
ESTNUM   ZIC   R0,TWEST                                                         
         LTR   R0,R0                                                            
         BZ    OVXIT                                                            
         CH    R0,=H'255'                                                       
         BH    OVXIT                                                            
         BR    RE                                                               
*                                                                               
RATETAB  DC    CL9'2F8C9WYY '                                                   
         EJECT                                                                  
* VALIDATE PACKAGE                                                              
*                                                                               
VALPKG   MVI   TWERROR,NOTFOUND                                                 
         XC    TWMSTPRD,TWMSTPRD                                                
         CLC   TWTYPE,=CL3'MVU'                                                 
         BE    *+8                                                              
         MVI   TWPKSTAT,X'20'      SET STATUS TO LOCKED                         
         CLI   TWPKG,0             CHECK FOR ZERO INPUT                         
         BE    OVXIT                                                            
         CLI   TWPKG,255                                                        
         BH    OVXIT                                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY              TEST FOR ERROR                               
         USING NPRECD,R4                                                        
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,TWAGYMED                                                   
         MVC   NPKCLT,TWCLIENT                                                  
         MVC   NPKNET,TWNET                                                     
         MVC   NPKEST,TWEST                                                     
         MVC   NPKPACK,TWPKG                                                    
RESETPKG MVI   TWERROR,NOTFOUND                                                 
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE     TEST FOR ERROR                               
         BNE   OVXIT                                                            
         MVC   TWPKEY,KEY                                                       
         SPACE 1                                                                
         GOTO1 AIO,DMCB,UNT+FILE+GET,PACKREC                                    
         LA    R4,PACKREC                                                       
         ST    R4,APACKREC                                                      
         SPACE                                                                  
         CLI   TWACT,C'C'          CHECK FOR CONTINUATION                       
*****    BE    VALPKG50            BYPASS ERROR CHECKS                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TWERROR,PAKLERR                                                  
         TM    NPAKSTAT,X'20'      CHECK IF LOCKED                              
         BNZ   OVXIT                                                            
         CLC   TWTYPE,=CL3'MVU'                                                 
         BE    VALPKG50                                                         
         MVI   TWERROR,INVERR                                                   
         TM    NPAKCNTL,X'20'      CHECK IF UNITS UNDER PACKAGE                 
         BZ    OVXIT               YES ERROR                                    
VALPKG50 MVC   TWPKSTAT,NPAKSTAT                                                
         CLC   TWTYPE,=CL3'MVU'                                                 
         BE    *+8                                                              
         OI    TWPKSTAT,X'20'      SAVE PACKAGE STATUS                          
         MVC   TWDYPT,NPAKDP       DAYPART                                      
         MVC   TWSREP,NPAKSREP     SPECIAL REP                                  
         MVC   TWUNIV,NPAKUNCD     UNIVERSE                                     
         MVC   TWPCNTRL,NPAKCNTL   CONTROL                                      
         MVC   TWMSTPRD,NPAKMAST   MASTER PRODUCT                               
         MVC   TWPINTG,NPAKINT     INTEGRATION COST                             
*                                                                               
         XC    TWHPPCT,TWHPPCT                                                  
         XC    TWBUYTYP,TWBUYTYP                                                
         GOTO1 VHELLO,DMCB,(C'G',=C'UNTFIL  '),(X'02',APACKREC),0               
         CLI   12(R1),0                                                         
         BNE   VALPKG60                                                         
         L     RE,12(R1)                                                        
         USING NPK2D,RE                                                         
         MVC   TWBUYTYP,NPK2BTYP   BUY TYPE                                     
         MVC   TWMSTPRA,NPAKMPRD   MASTER PRODUCT ALPHA                         
         MVC   TWHPPCT(5),NPKHPNAM HP PERCENTAGE                                
         MVC   TWVTYPE(2),NPK2VTYP V TYPE                                       
         MVC   TWTRADP(2),NPK2TRAD TRADE PCTG                                   
         MVC   TWCASHP(2),NPK2CASH CASH PCTG                                    
         DROP  RE                                                               
*                                                                               
VALPKG60 CLC   TWAGY,=CL2'DU'       NO IMP CHECK FOR MEDIAVEST                  
         BE    VALPKGX                                                          
         MVI   TWERROR,UPLIERR                                                  
         TM    NPAKCNTL,X'40'       PACKAGE MUST BE IMP BASED                   
         BZ    VALPKG80                                                         
         CLI   TWEDF,C'I'           CHECK FOR IMP BASED DEMOS                   
         BNE   OVXIT                                                            
         B     VALPKGX                                                          
VALPKG80 CLI   TWEDF,C'I'           MUST BE A VPH,OR RTG PACKAGE                
         BE    OVXIT                                                            
*                                                                               
         SPACE 1                                                                
VALPKGX  MVI   TWERROR,0                                                        
         B     OVXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* READ STATION RECORD                                                           
*                                                                               
GETSTA   MVI   TWERROR,INVERR                                                   
*                                                                               
         L     R5,AIOAREA1                                                      
         USING STAREC,R5                                                        
         L     R6,AIOAREA4                                                      
         USING BUYVALD,R6                                                       
         MVI   0(R5),C'0'                                                       
         MVC   1(14,R5),0(R5)                                                   
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),TWNET                                                
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,TWAGY  '                                                 
         MVC   KEY(15),0(R5)                                                    
         PRINT GEN                                                              
         GOTO1 AIO,DMCB,STA+HIGH+FILE,AIOAREA1                                  
         PRINT NOGEN                                                            
         CLC   KEYSAVE(15),0(R5)                                                
         BNE   OVXIT                                                            
*                                                                               
         L     R5,AIOAREA1                                                      
         MVC   TWPSTTYP,SPTYPE                                                  
         MVC   TWTRTYP,STRTYPE                                                  
         MVC   TWMEDTYP,STYPE                                                   
         MVC   TWBKTYP,SOVBKTYP                                                 
         MVC   TWSUBMED,SUBMEDIA                                                
         XC    TWNTISTA,TWNTISTA                                                
         CLI   SNTISTA,X'40'                                                    
         BNH   *+10                                                             
         MVC   TWNTISTA,SNTISTA                                                 
         PACK  DUB,SMKT(4)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,TWMKT                                                       
         MVC   NETMARK,TWMKT                                                    
         MVC   NETTRTYP,TWTRTYP                                                 
*SET POSTING TYPE BITS                                                          
         LA    RE,POSTAB                                                        
         NI    TWPSTBIT,X'FC'       INIT THE BITS                               
*                                                                               
GETST40  CLI   0(RE),C'O'           CHECK FOR DEFAULT SETTING                   
         BE    GETST50                                                          
         CLC   TWTRTYP,0(RE)                                                    
         BE    GETST50                                                          
         LA    RE,2(RE)                                                         
         B     GETST40                                                          
GETST50  OC    TWPSTBIT,1(RE)                                                   
*                                                                               
         CLI   SPTYPE,C'C'                                                      
         BE    *+12                                                             
         CLI   SPTYPE,C'O'                                                      
         BNE   *+14                                                             
         MVC   DEMPREC(14),COPREC                                               
         B     GETSTX                                                           
         MVC   DEMPREC(14),NSPREC                                               
*                                                                               
GETSTX   MVI   TWERROR,0                                                        
         B     OVXIT                                                            
*                                                                               
*--DEMO OVERRIDE PRECISION TABLES                                               
COPREC   DC    C'R',X'82',C'S',X'81',C'P',X'82',C'T',X'42',C'H',X'42'           
         DC    C'U',X'43',C'V',X'40'                                            
NSPREC   DC    C'R',X'81',C'S',X'81',C'P',X'81',C'T',X'43',C'H',X'43'           
         DC    C'U',X'43',C'V',X'40'                                            
*                                                                               
POSTAB   DC    C'N',X'00',C'H',X'00',C'C',X'01',C'S',X'02',C'O',X'03'           
         DROP  R5,R6                                                            
         EJECT                                                                  
BILDOV   CLI   TWAGY,X'40'                                                      
         BNH   OVXIT                                                            
*                                                                               
         XC    REQHDR,REQHDR                                                    
         MVI   REQHDR+15,X'01'     1 CARD, LINKED                               
*                                                                               
         MVC   REQUEST,SPACES                                                   
         MVC   REQUEST(2),=C'W4'                                                
         MVC   REQUEST+2(2),TWAGY                                               
         SPACE                                                                  
         L     R1,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
******   LA    R3,FASIN-FACTSD(,RE)                                             
******   LA    R3,100                                                           
******                                                                          
******   LA    R4,REQUEST+5                                                     
******   SPACE                                                                  
******   EDIT  (B4,0(R3)),(6,(R4)),FILL=0                                       
         MVC   REQUEST+5(6),=CL6'000100'                                        
         MVC   REQUEST+12(31),=CL31'0207CBRECAP 0303REP 0506OV,T/A '            
         MVC   REQUEST+43(10),=CL10'0606DIRECT'                                 
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(8,DUB)                                       
         OI    DUB+7,X'40'                                                      
         CLI   DUB+7,X'40'                                                      
         BE    BLDOV100                                                         
         MVC   REQUEST+54(4),=CL4'1208'                                         
         MVC   REQUEST+58(8),DUB                                                
         MVI   REQUEST+66,C'*'                                                  
         B     BLDOV300                                                         
*                                                                               
BLDOV100 MVC   REQUEST+54(4),=CL4'1207'                                         
         MVC   REQUEST+58(7),DUB                                                
         MVI   REQUEST+65,C'*'                                                  
         B     BLDOV300                                                         
*                                                                               
BLDOV300 L     R1,ACOMFACS                                                      
         L     RF,CREQTWA-COMFACSD(R1)                                          
         GOTO1 (RF),DMCB,(5,ATWA),REQHDR,VDATAMGR,ACOMFACS                      
         CLI   DMCB+8,0                                                         
         BE    OVXIT                                                            
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*--IF PROGRAM RECORD EXISTS MOVE VPH'S INTO UNIT RECORD                         
*UNIT RECORD = AIOAREA1   PROGRAM RECORD = AIOAREA2                             
*                                                                               
TRAPPROG DS    0H                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'UNTFIL  '),(X'33',AIOAREA1),0               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE '),(X'92',AIOAREA2),0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NPGEL92,R6                                                       
*--SHARE/RTG VALUES                                                             
         GOTO1 VHELLO,DMCB,(C'G',=C'UNTFIL  '),(X'35',AIOAREA1),0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,12(R1)                                                        
         USING NUEHD,R5                                                         
*                                                                               
         TM    NPGSTAT,X'80'                                                    
         BZ    *+14                                                             
         MVC   NUEHRTG,NPGSHARE                                                 
         B     *+10                                                             
         MVC   NPGSHARE,NPGSHARE                                                
*--REGULAR ESTIMATED VPHS (92 ELEMENT)                                          
         LA    R3,WORKAREA                                                      
         USING NUEVD,R3                                                         
*                                                                               
         MVC   NUEVEL(3),=XL3'332501'                                           
         MVC   NUEVPHS(34),NPGVPHS                                              
         DROP  R5,R6                                                            
*--REGULAR ESTIMATED VPHS (93 ELEMENT)                                          
         GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE '),(X'93',AIOAREA2),0               
         CLI   12(R1),0                                                         
         BNE   TRPPR100                                                         
         L     R6,12(R1)                                                        
         USING NPGEL93,R6                                                       
         MVC   NUEVEL(3),=XL3'337702'                                           
         MVC   NUEVPHS,NPG2VPHS                                                 
TRPPR100 GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         B     TRPPR200                                                         
         DROP  R6,R3                                                            
*--NAD DEMOS                                                                    
TRPPR200 GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE '),(X'DD',AIOAREA2),0               
         CLI   12(R1),0                                                         
         BNE   TRPPR300                                                         
         L     R6,12(R1)                                                        
         B     TRPPR250                                                         
*                                                                               
TRPPR220 ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'DD'                                                      
         BNE   TRPPR300                                                         
*                                                                               
TRPPR250 GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,(R6),0                  
         B     TRPPR220                                                         
*--USER DEMOS                                                                   
TRPPR300 GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE '),(X'C3',AIOAREA2),0               
         CLI   12(R1),0                                                         
         BNE   TRPPRX                                                           
         L     R6,12(R1)                                                        
         B     TRPPR350                                                         
*                                                                               
TRPPR320 ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'C3'                                                      
         BNE   TRPPRX                                                           
*                                                                               
TRPPR350 GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,(R6),0                  
         B     TRPPR320                                                         
*                                                                               
TRPPRX   B     OVXIT                                                            
         EJECT                                                                  
*--THIS ROUTINE BUILDS THE UNIVERSE ELEMENT                                     
*--ELEMENT RETURNED IN WORKAREA                                                 
*                                                                               
BLDUNIV  L     R3,AIOAREA3                                                      
         USING NURECD,R3                                                        
*                                                                               
         LA    R2,WORKAREA+120                                                  
         USING GUVD,R2                                                          
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVAGY,TWAGY                                                     
         MVC   GUVCODE,TWUNIV                                                   
         OC    GUVCODE,GUVCODE     TEST FOR UNIVERSE CODE                       
         BNZ   *+10                HAVE ONE                                     
         MVC   GUVDATE,TWPDATE     ELSE USE AIR DATE                            
         XC    WORKAREA(100),WORKAREA                                           
         LA    R3,WORKAREA                                                      
         ST    R3,GUVAOUT          OUTPUT ELEMENT ADDRESS                       
         L     R0,AIOAREA2                                                      
         ST    R0,GUVAREC          SET AND CLEAR AREA FOR UNIV. RECORD          
         LA    R1,2000                                                          
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   GUVCMFCS,ACOMFACS                                                
         MVC   GUVNETWK,VNETWEEK                                                
         NI    NUUNST2,X'FF'-X'08' TURN OFF CABLE UNIV                          
         GOTO1 VGETNUN,DMCB,(R2)                                                
*        CLI   GUVERROR,0          TEST FOR ERROR                               
*        BE    *+6                                                              
*        DC    H'0'                TAKE A HIT FOR NOW                           
         CLI   GUVUTYPE,C'C'                                                    
         BNE   *+8                                                              
         OI    NUUNST2,X'08'                                                    
*                                                                               
         B     OVXIT                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    VALIDATES AND CONVERTS THE DEMOS INTO 3 BYTE                               
*    FORMAT.  THEN COMPARES THE PACKAGE DEMOS TO THE                            
*    ESTIMATE RECORD DEMOS, AND REMOVES THE DEMOS THAT                          
*    ARE NOT ON BOTH LISTS.                                                     
*                                                                               
*****************************************SETDEMO  NTR1                          
SETDEMO  DS    0H                                                               
         USING NDLD,R4                                                          
         MVI   TWERROR,INVERR                                                   
*                                                                               
         XC    WORKAREA(200),WORKAREA                                           
*                                                                               
         LA    RE,WORKAREA+8                                                    
         LA    RF,NDLDEMO                                                       
         LA    R1,18                                                            
*                                                                               
STDM020  CLI   0(RF),X'40'                                                      
         BNH   STDM100                                                          
         MVC   0(6,RE),0(RF)                                                    
         OC    0(6,RE),SPACES                                                   
         CLI   0(RE),C'P'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C'V'          CHANGE PEOPLE TO VIEWERS                     
*                                                                               
         LA    R5,DEMCONV                                                       
STDM030  CLI   0(R5),X'FF'                                                      
         BE    STDM035                                                          
         CLC   0(6,RE),0(R5)                                                    
         BE    STDM033                                                          
         LA    R5,12(R5)                                                        
         B     STDM030                                                          
STDM033  MVC   0(6,RE),6(R5)                                                    
*                                                                               
STDM035  LA    R6,6                                                             
STDM040  CLI   0(RE),X'40'                                                      
         BNH   STDM060                                                          
         LA    RE,1(RE)                                                         
         BCT   R6,STDM040                                                       
*                                                                               
STDM060  LA    RF,7(RF)                                                         
         CLI   0(RF),X'40'                                                      
         BNH   STDM080                                                          
         MVI   0(RE),C','                                                       
         B     *+8                                                              
STDM080  MVI   0(RE),X'FF'                                                      
         LA    RE,1(RE)                                                         
         BCT   R1,STDM020                                                       
*--CALCULATE NUMBER OF DEMO FIELDS                                              
*                                                                               
STDM100  LA    RE,18                                                            
         SR    RE,R1                                                            
         LTR   RE,RE                                                            
         BZ    STDMX               NO DEMOS PROCESSED EXIT                      
*                                                                               
         STCM  RE,1,NFLDS                                                       
*                                                                               
*--CALCULATE THE LENGTH OF THE DEMOS                                            
         LA    RE,WORKAREA+8                                                    
         SR    RF,RF                                                            
STDM120  CLI   0(RE),X'FF'                                                      
         BE    STDM140                                                          
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         B     STDM120                                                          
*                                                                               
STDM140  STCM  RF,1,WORKAREA+5     FIELD LENGTH                                 
         LA    RF,8(RF)                                                         
         STCM  RF,1,WORKAREA                                                    
         MVI   0(RE),0                                                          
*                                                                               
*--CALL DEMOVAL CONVERT TO 3 BYTE FORMAT                                        
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
*                                                                               
         XC    TWDEMOS,TWDEMOS                                                  
         LA    R6,DBLOCKA                                                       
         USING DBLOCKD,R6                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVC   DBFILE,=C'NTI'                                                   
         GOTO1 CDEMOVAL,DMCB,(0,WORKAREA),(18,TWDEMOS),(0,(R6))                 
*                                                                               
         CLI   TWDEMOS,X'FF'       ANY DEMOS                                    
         BE    OVXIT               NO ERROR                                     
         B     STDMX                                                            
*                                                                               
STDMX    MVI   TWERROR,0                                                        
         B     OVXIT                                                            
         DROP  R3,R4,R6                                                         
DEMCONV  DC    CL6'M2-5  ',CL6'B2-5  '                                          
         DC    CL6'M0205 ',CL6'B2-5  '                                          
         DC    CL6'M6-8  ',CL6'B6-8  '                                          
         DC    CL6'M0608 ',CL6'B6-8  '                                          
         DC    CL6'M6-11 ',CL6'B6-11 '                                          
         DC    CL6'M0611 ',CL6'B6-11 '                                          
         DC    CL6'M2-11 ',CL6'B2-11 '                                          
         DC    CL6'M0211 ',CL6'B2-11 '                                          
         DC    CL6'M9-11 ',CL6'B9-11 '                                          
         DC    CL6'M0911 ',CL6'B9-11 '                                          
         DC    CL6'F2-5  ',CL6'G2-5  '                                          
         DC    CL6'F0205 ',CL6'G2-5  '                                          
         DC    CL6'F6-8  ',CL6'G6-8  '                                          
         DC    CL6'F0608 ',CL6'G6-8  '                                          
         DC    CL6'F6-11 ',CL6'G6-11 '                                          
         DC    CL6'F0611 ',CL6'G6-11 '                                          
         DC    CL6'F2-11 ',CL6'G2-11 '                                          
         DC    CL6'F0211 ',CL6'G2-11 '                                          
         DC    CL6'F9-11 ',CL6'G9-11 '                                          
         DC    CL6'F0911 ',CL6'G9-11 '                                          
         DC    X'FF'                                                            
         EJECT                                                                  
OVXIT    XMOD1 1                                                                
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
*--OVERFLOW ROUTINES                                                            
*                                                                               
         DS    0F                                                               
         DROP  RA,RB                                                            
OVFLRTN2 NMOD1 0,**40OV2*                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING OVFLRTN2+4096,RA                                                 
         L     RC,4(R1)                                                         
         L     R9,8(R1)                                                         
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         L     R5,AIOAREA4                                                      
         USING BUYVALD,R5                                                       
         ST    R5,ABUYVALS                                                      
         L     R7,AOVWORK                                                       
         USING WORKD,R7                                                         
         MVC   NBAIO,AIOAREA3                                                   
                                                                                
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRANC2(RF)                                                     
*                                                                               
OVBRANC2 B     DOUPUNIT                                                         
         B     WRTREC                                                           
         B     CALCASHP                                                         
         EJECT                                                                  
*--CHANGE A UNIT RECORD                                                         
*                                                                               
* THIS ROUTINE:                                                                 
*    UPDATE THE UNITS WITH INFORMATION PASSED                                   
*    A UNIT RECORD.                                                             
*                                                                               
*    NOTE:      IF PROGRAM CODE INPUTTED PROGRAM RECORD                         
*               RESIDES IN AIOAREA2                                             
*                                                                               
DOUPUNIT DS    0H                                                               
*                                                                               
*  CHECK IF PACKAGE RECORD STILL SET                                            
*                                                                               
         L     RE,APACKREC                                                      
         CLC   TWPKEY,0(RE)                                                     
         BE    DOUP05                                                           
         MVC   KEY(20),TWPKEY                                                   
         GOTO1 =A(OVFLRTN),DMCB,(9,DUB),(RC),(R9),RR=MYRELO   RESETPKG          
*                                                                               
*INITIALIZE SETTINGS                                                            
DOUP05   MVI   TWSPSEQ,1           SET SPECIAL CHARGE SEQ NUMBER                
         MVI   TWUNST3,0                                                        
         OI    TWUNST3,X'04'      SET UNIT STATUS 3 FOR CABLE UPLOAD            
         MVI   TWSPDEL,C'N'                                                     
*                                                                               
         USING NCHUD,R4            UNIT RECORD DSECT                            
         ST    R4,UPACURUN         ADDRESS OF UNIT RECORD                       
* CHECK FIELD INPUTTED                                                          
         CLC   NCHUSERN,SPACES                                                  
         BNE   DOUP10                                                           
         CLC   NCHUCNT(NCHTFLDS),SPACES                                         
         BNE   DOUP10                                                           
         MVI   NCHUERNO+1,INVERR                                                
         MVI   NCHUERF,NCHUACTQ                                                 
         B     DOUP900                                                          
*                                                                               
DOUP10   L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
*                                                                               
         BAS   RE,DOUPVAL          SET THE VALUES                               
         CLI   NCHUERF,0                                                        
         BNE   DOUP900                                                          
*                                                                               
         BAS   RE,CKREASN          VALIDATE THE REASON CODE                     
         CLI   NCHUERF,0                                                        
         BNE   DOUP900                                                          
*                                                                               
         XC    NBMAINEL(NBUNTLEN),NBMAINEL  CLEAR OUT STALE DATA                
         CLI   NCHUACT,C'A'                                                     
         BNE   DOUP50                                                           
*                                                                               
*  SET DEFAULT INFORMATION FOR ADD                                              
         XCEF  (R3),2000                                                        
*--04 KEY                                                                       
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,TWAGYMED                                                   
         MVC   NUKCLT,TWCLIENT                                                  
         MVC   NUKDATE,TWPDATE                                                  
         MVC   NUKTIME,TWQTRHR                                                  
         MVC   NUKNET,TWNET                                                     
         MVC   NUKPROG,TWPRCODE                                                 
         MVC   NUKEST,TWEST                                                     
         MVC   NUKDP,TWDYPT                                                     
         OC    NURSTAT,TWPSTBIT                                                 
*                                                                               
*  SET FIELD FOR INTEGRATION TABLE LOOKUP                                       
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         MVC   NBACTAM,TWAGYMED                                                 
         MVC   NBACTNET,TWNET                                                   
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   NBDM,CDATAMGR                                                    
         MVC   NBDATCON,CDATCON                                                 
         B     DOUP70                                                           
         DROP  R6,RE                                                            
*                                                                               
*  ACTION CHANGE                                                                
DOUP50   XC    KEY,KEY                                                          
         MVI   KEY,X'04'                                                        
         MVC   KEY+1(L'TWAGYMED),TWAGYMED                                       
         MVC   KEY+2(18),TWKEY                                                  
         CLC   KEY,KEYSAVE                                                      
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+16                                                             
         MVI   NCHUERNO+1,NOTFOUND                                              
         MVI   NCHUERF,NCHUUIDQ                                                 
         B     DOUP900                                                          
*                                                                               
         GOTO1 AIO,DMCB,UNT+FILE+GET+UPDATE,AIOAREA1                            
         MVC   AIOAREA,AIOAREA1                                                 
*  SEED THE UNIT IN NETBLOCK                                                    
DOUP60   LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         MVI   NBFUNCT,NBFVAL                                                   
         MVC   NBAIO,AIOAREA1                                                   
         MVC   NBACOM,ACOMFACS                                                  
         MVC   NBNETVAL,VNETVAL                                                 
*                                                                               
         CLI   TWMEDTYP,C'O'                                                    
         BE    *+10                                                             
         MVC   NBSTATYP,TWMEDTYP                                                
         MVC   NBSTSTAT,TWMEDTYP                                                
         MVC   NBPOSTYP,TWPSTTYP                                                
         MVC   NBSTPSTT,TWPSTTYP                                                
         MVC   NBBKTYP,TWBKTYP                                                  
         MVC   NBSUBMED,TWSUBMED                                                
         CLI   TWNTISTA,X'40'                                                   
         BNH   *+10                                                             
         MVC   NBNTISTA,TWNTISTA  IF EXISTS SET OVERRIDE NTI STATION            
*                                                                               
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
*  GET THE PROGRAM RECORD                                                       
DOUP70   MVI   NCHUERNO+1,0                                                     
         MVC   DUB(6),NCHUPROG                                                  
         GOTO1 VDATCON,DMCB,(0,NCHUWOD),(2,DUB+6)                               
         CLI   NCHUACT,C'A'                                                     
         BE    *+16                                                             
         MVC   DUB(6),TWKEY+9       NOT ADD PULL CODE FROM UNIT ID              
         MVC   DUB+6(2),TWKEY+2     NOT ADD PULL DAY FROM UNIT ID               
         BAS   RE,GETPRG                                                        
         CLI   NCHUERNO+1,PROGERR                                               
         BNE   DOUP80                                                           
         MVI   NCHUERNO+1,NOTFOUND                                              
         MVI   NCHUERF,NCHUPRGQ                                                 
         B     DOUP900                                                          
*                                                                               
*  INITIALIZE THE EDIT                                                          
DOUP80   MVC   PACK,TWPKG          PUT PACKAGE NUMBER IN COMMON STORAGE         
         LA    R5,BLOCK            INITIALIZE EDIT BLOCK AND CALL               
         USING UNBLOCKD,R5         EDIT FOR INITIALIZATION                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         MVC   UNALOCAL,AIOAREA3                                                
* SET STAION RECORD VALUES                                                      
         MVC   NBSTSTAT,TWMEDTYP                                                
         MVC   NBPOSTYP,TWPSTTYP                                                
         MVC   NBBKTYP,TWBKTYP                                                  
         MVC   NBSUBMED,TWSUBMED                                                
*                                                                               
         MVI   ACTION,B            SET ACTION TO BUY                            
         CLI   NCHUACT,C'A'        TEST FOR ACTION ADD                          
         BE    DOUP100             NO                                           
         MVI   ACTION,C            SET ACTION TO CHANGE                         
         MVC   UNODATE,NBACTDAT    SET ORIGINAL RECORD VALUES                   
         MVC   UNOSUB,NBACTSUB                                                  
         MVC   UNOTIME,NBACTSQH                                                 
         MVC   UNODAY,NBDAY                                                     
         SPACE                                                                  
         DROP  R6                                                               
         PRINT GEN                                                              
DOUP100  GOTO1 VEDIT,DMCB,(C'I',(R5))                                           
         PRINT NOGEN                                                            
*                                                                               
*  ADD DEFAULT TRAFFIC ELEMENT                                                  
         CLI   NCHUACT,C'A'        TEST FOR ACTION ADD                          
         BNE   DOUP120             NO                                           
         LA    R6,WORKAREA                                                      
         USING NUCMLEL,R6          UNIT RECORD DSECT                            
         XC    WORKAREA(100),WORKAREA                                           
         MVI   NUCMLEID,X'21'                                                   
         MVI   NUCMLELN,80                                                      
         BAS   RE,PUTUEL                                                        
         DROP  R6                                                               
*  CHECK FOR PREEMPT                                                            
DOUP120  CLI   NCHUPRE,C'Y'                                                     
         BNE   DOUP130                                                          
         BAS   RE,DOPREMPT                                                      
         CLI   NCHUERF,0                                                        
         BE    DOUP880                                                          
         BNE   DOUP900                                                          
*                                                                               
*  EDIT THE DAY FIELD                                                           
*                                                                               
DOUP130  OC    TWDAYLIT,TWDAYLIT                                                
         BZ    DOUP160              NOT INPUTTED GET NEXT FIELD                 
         CLI   NCHUACT,C'A'        TEST FOR ACTION ADD                          
         BE    *+10                YES                                          
         MVC   NUKDATE,TWPDATE     MOVE NEW DATE INTO THE RECORD                
         XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,11                                                      
         MVI   WORKAREA+5,3                                                     
         MVC   WORKAREA+8(3),TWDAYLIT                                           
         LA    R2,WORKAREA                                                      
*        GOTO1 VGETFLD                                                          
*        LA    R2,FLDH                                                          
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,UDAY                                                     
*                                                                               
         PRINT GEN                                                              
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         PRINT NOGEN                                                            
         CLI   UNERROR,0                                                        
         BE    DOUP160                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUWODQ                                                 
         B     DOUP900                                                          
*                                                                               
*  EDIT THE TIME FIELD                                                          
*                                                                               
DOUP160  CLI   ACTION,B            CHECK BUY ACTION                             
         BE    *+14                                                             
         OC    TWPRSTIM,TWPRSTIM                                                
         BZ    DOUP170              NOT INPUTTED GET NEXT FIELD                 
         XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,19                                                      
         MVI   WORKAREA+5,11                                                    
         PRINT GEN                                                              
         GOTO1 VUNTIME,DMCB2,TWPRSTIM,WORKAREA+8                                
         PRINT NOGEN                                                            
         OC    WORKAREA+8(11),SPACES                                            
         LA    R2,WORKAREA                                                      
*        GOTO1 VGETFLD                                                          
*        LA    R2,FLDH                                                          
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,UTIME                                                    
*                                                                               
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    DOUP170                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUSTMQ                                                 
         B     DOUP900                                                          
*                                                                               
*  EDIT THE PROGRAM NAME                                                        
*                                                                               
DOUP170  CLI   ACTION,B            CHECK BUY ACTION                             
         BE    *+14                                                             
         OC    TWPRNAME,TWPRNAME                                                
         BZ    DOUP180              NOT INPUTTED GET NEXT FIELD                 
         XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,24                                                      
         MVI   WORKAREA+5,16                                                    
         MVC   WORKAREA+8(16),TWPRNAME                                          
         LA    R2,WORKAREA                                                      
*        GOTO1 VGETFLD                                                          
*        LA    R2,FLDH                                                          
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,UPRGN                                                    
*                                                                               
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    DOUP180                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUPNMQ                                                 
         B     DOUP900                                                          
*                                                                               
*  EDIT THE LENGTH                                                              
*                                                                               
DOUP180  CLI   ACTION,B            CHECK BUY ACTION                             
         BE    *+14                                                             
         OC    TWPLEN,TWPLEN                                                    
         BZ    DOUP200              NOT INPUTTED GET NEXT FIELD                 
         XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,15                                                      
******   MVI   WORKAREA+4,X'08'                                                 
         MVI   WORKAREA+5,7                                                     
         MVC   WORKAREA+8(7),NCHUULN                                            
         LA    R2,WORKAREA                                                      
*        GOTO1 VGETFLD                                                          
*        LA    R2,FLDH                                                          
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,ULEN                                                     
*                                                                               
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    DOUP200                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUULNQ                                                 
         B     DOUP900                                                          
*                                                                               
*  EDIT UNIVERSE                                                                
*                                                                               
DOUP200  CLI   ACTION,B            CHECK BUY ACTION                             
         BE    *+12                                                             
         CLI   TWUNIVL,X'40'                                                    
         BE    DOUP220              NOT INPUTTED GET NEXT FIELD                 
         XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,12                                                      
         MVI   WORKAREA+4,X'08'                                                 
         MVI   WORKAREA+5,4                                                     
         MVC   WORKAREA+8(4),TWUNIVL                                            
         LA    R2,WORKAREA                                                      
*        GOTO1 VGETFLD                                                          
*        LA    R2,FLDH                                                          
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,UUNCD                                                    
*                                                                               
         PRINT GEN                                                              
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         PRINT NOGEN                                                            
         CLI   UNERROR,0                                                        
         BE    DOUP220                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUACTQ                                                 
         B     DOUP900                                                          
*                                                                               
*  EDIT SPECIAL REP                                                             
*                                                                               
DOUP220  CLI   ACTION,B            CHECK BUY ACTION                             
         BE    *+12                                                             
         CLI   TWSREPL,X'40'                                                    
         BE    DOUP240              NOT INPUTTED GET NEXT FIELD                 
         XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,12                                                      
         MVI   WORKAREA+4,X'08'                                                 
         MVI   WORKAREA+5,3                                                     
         MVC   WORKAREA+8(3),TWSREPL                                            
         LA    R2,WORKAREA                                                      
*        GOTO1 VGETFLD                                                          
*        LA    R2,FLDH                                                          
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,USREP                                                    
*                                                                               
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    DOUP240                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUACTQ                                                 
         B     DOUP900                                                          
*                                                                               
*  EDIT ACTUAL COST                                                             
*                                                                               
DOUP240  CLI   ACTION,B            CHECK BUY ACTION                             
         BE    *+12                                                             
         CLI   NCHUTRT,X'40'                                                    
         BE    DOUP260              NOT INPUTTED GET NEXT FIELD                 
         XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,18                                                      
*****    MVI   WORKAREA+4,X'08'                                                 
         MVI   WORKAREA+5,10                                                    
         MVC   WORKAREA+8(10),NCHUTRT                                           
         LA    R2,WORKAREA                                                      
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,UACT                                                     
*                                                                               
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    DOUP260                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUTRTQ                                                 
         B     DOUP900                                                          
*                                                                               
*  EDIT INTEGRATION COST                                                        
*                                                                               
DOUP260  XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,17                                                      
         MVC   WORKAREA+8(9),NCHUIRT                                            
         CLI   NCHUIRT,X'40'                                                    
         BNE   DOUP265                                                          
         CLI   ACTION,B            NO INPUT AND ADD                             
         BNE   DOUP280             GET INTG FROM TABLE                          
         TM    TWPCNTRL,X'80'      IS PACKAGE SET TO TABLE                      
         BNZ   DOUP263             YES HANDLE INTEGRATION TABLE                 
*                                                                               
*--GET INTEGRATION FROM PACKAGE                                                 
         EDIT  (4,TWPINTG),(9,WORKAREA+8),FILL=0                                
         B     DOUP265                                                          
*                                                                               
DOUP263  LA    RF,INTGTBL                                                       
         ST    RF,UNINGTBL         PASS BACK INTG RATES HERE                    
         MVI   INTGREAD,0          DO 1ST READ                                  
         LA    RF,SVINTTBL                                                      
         ST    RF,INTHLDIT                                                      
         MVC   INTGAIO,AIOAREA2                                                 
         MVC   INTGSTDT,TWPDATE                                                 
         MVC   INTGEDDT,TWPDATE                                                 
         B     DOUP270                                                          
*******        DOUP265  MVI   WORKAREA+4,X'08'                                  
DOUP265  MVI   WORKAREA+5,9                                                     
DOUP270  LA    R2,WORKAREA                                                      
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,UINT                                                     
*                                                                               
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    DOUP280                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUIRTQ                                                 
         B     DOUP900                                                          
*                                                                               
*  EDIT ASSIGNED COST                                                           
*                                                                               
DOUP280  CLI   ACTION,B            CHECK BUY ACTION                             
         BE    *+12                                                             
         CLI   NCHUACST,X'40'                                                   
         BE    DOUP300              NOT INPUTTED GET NEXT FIELD                 
         XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,17                                                      
*****    MVI   WORKAREA+4,X'08'                                                 
         MVI   WORKAREA+5,9                                                     
         MVC   WORKAREA+8(10),NCHUACST                                          
         LA    R2,WORKAREA                                                      
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,UASS                                                     
*                                                                               
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    DOUP300                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUACSQ                                                 
         B     DOUP900                                                          
*                                                                               
*  EDIT PRODUCT                                                                 
*                                                                               
DOUP300  CLI   ACTION,B            CHECK BUY ACTION                             
         BE    *+12                                                             
         CLI   NCHUPRD,X'40'                                                    
         BE    DOUP320              NOT INPUTTED GET NEXT FIELD                 
         XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,15                                                      
         CLC   NCHUPRD(3),=CL3'###' SHOULD I UNALLOCATE                         
         BE    DOUP310                                                          
         MVI   WORKAREA+5,7                                                     
         MVC   WORKAREA+8(7),NCHUPRD                                            
DOUP310  LA    R2,WORKAREA                                                      
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,UPRD                                                     
*                                                                               
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    DOUP320                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUPRDQ                                                 
         B     DOUP900                                                          
*                                                                               
*  1ST PRODUCT PERCENTAGE                                                       
*                                                                               
DOUP320  CLI   ACTION,B            CHECK BUY ACTION                             
         BE    *+12                                                             
         CLI   NCHUPCT,X'40'                                                    
         BE    DOUP380              NOT INPUTTED GET NEXT FIELD                 
         XC    WORKAREA,WORKAREA                                                
         MVI   WORKAREA,14                                                      
         CLC   NCHUPCT(3),=CL3'###' SHOULD I UNALLOCATE                         
         BE    DOUP330                                                          
         MVI   WORKAREA+5,6                                                     
         MVC   WORKAREA+8(6),NCHUPCT                                            
DOUP330  LA    R2,WORKAREA                                                      
         ST    R2,UNFLDH                                                        
         MVI   UNEDATA,UP1SHR                                                   
*                                                                               
         GOTO1 VEDIT,DMCB,(C'E',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    DOUP380                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUPCTQ                                                 
         B     DOUP900                                                          
*                                                                               
*  DEAL CONTRACT SERIAL NUMBER                                                  
*                                                                               
DOUP380  L     R3,AIOAREA1          RESET UNIT RECORD POINTER                   
         GOTO1 VHELLO,DMCB,(C'G',=C'UNTFILE '),(X'75',AIOAREA1),0               
         CLI   12(R1),0                                                         
         BNE   DOUP400                                                          
         L     R6,12(R1)                                                        
         USING NUSQD,R6                                                         
         CLI   NCHUSERN,X'40'       SERIAL #                                    
         BE    *+10                                                             
         MVC   NUSQSER,NCHUSERN                                                 
         CLI   NCHUCNT,X'40'       CONTRACT #                                   
         BE    *+10                                                             
         MVC   NUSQCON,NCHUCNT                                                  
         CLI   TWDEAL,X'40'         DEAL #                                      
         BE    *+10                                                             
         MVC   NUSQDEAL,TWDEAL                                                  
         B     DOUP420                                                          
*                                                                               
DOUP400  LA    R6,WORKAREA                                                      
         XC    WORKAREA(100),WORKAREA                                           
         MVI   NUSQEL,X'75'                                                     
         MVI   NUSQLEN,NUSQDQ                                                   
         CLI   NCHUSERN,X'40'       SERIAL #                                    
         BE    *+10                                                             
         MVC   NUSQSER,NCHUSERN                                                 
         CLI   NCHUCNT,X'40'       CONTRACT #                                   
         BE    *+10                                                             
         MVC   NUSQCON,NCHUCNT                                                  
         CLI   TWDEAL,X'40'         DEAL #                                      
         BE    *+10                                                             
         MVC   NUSQDEAL,TWDEAL                                                  
         BAS   RE,PUTUEL                                                        
         DROP  R6                                                               
*                                                                               
*  BILLBOARD                                                                    
*                                                                               
DOUP420  GOTO1 VHELLO,DMCB,(C'G',=C'UNTFILE '),(X'21',AIOAREA1),0               
         CLI   12(R1),0                                                         
         BNE   DOUP440                                                          
         L     R6,12(R1)                                                        
         USING NUCMLEL,R6                                                       
*                                                                               
         CLI   NCHUBLAT,X'40'                                                   
         BE    DOUP460                                                          
         NI    NUCMLFLG,X'FB'      TURN OFF COMMERCIAL FLAG                     
         CLI   NCHUBLAT,C'Y'                                                    
         BNE   DOUP460                                                          
         OI    NUCMLFLG,X'04'      TURN ON COMMERCIAL FLAG                      
         B     DOUP460                                                          
*                                                                               
DOUP440  LA    R6,WORKAREA                                                      
         XC    WORKAREA(100),WORKAREA                                           
         MVC   NUCMLEID,=XL2'2150'                                              
         CLI   NCHUBLAT,X'40'                                                   
         BE    DOUP450                                                          
         NI    NUCMLFLG,X'FB'      TURN OFF COMMERCIAL FLAG                     
         CLI   NCHUBLAT,C'Y'                                                    
         BNE   DOUP450                                                          
         OI    NUCMLFLG,X'04'      TURN ON COMMERCIAL FLAG                      
DOUP450  BAS   RE,PUTUEL                                                        
*                                                                               
*  ADU/ROTATION                                                                 
*                                                                               
DOUP460  GOTO1 VHELLO,DMCB,(C'G',=C'UNTFILE '),(X'02',AIOAREA1),0               
         CLI   12(R1),0                                                         
         BNE   DOUP480                                                          
         L     R6,12(R1)                                                        
         USING NUSDRD,R6                                                        
         OI    NUSDST3,X'04'        CABLE UPLAOD INDICATOR                      
*                                                                               
         CLI   NCHUADU,X'40'                                                    
         BE    DOUP470                                                          
         NI    NUSDST3,X'FD'        TURN OFF ADU FLAG                           
         CLI   NCHUADU,C'Y'                                                     
         BNE   DOUP470                                                          
         OI    NUSDST3,X'02'        TURN ON ADU FLAG                            
         OC    NUACTUAL,NUACTUAL                                                
         BNZ   *+14                                                             
         OC    NUINTEG,NUINTEG                                                  
         BZ    DOUP470                                                          
         MVI   NCHUERNO+1,ADUERR                                                
         MVI   NCHUERF,NCHUADUQ                                                 
         B     DOUP900                                                          
*                                                                               
DOUP470  CLI   TWPRROT,0                                                        
         BE    DOUP500                                                          
         MVC   NUSDROT,TWPRROT      MOVE IN ROTATION                            
         B     DOUP500                                                          
*                                                                               
DOUP480  LA    R6,WORKAREA                                                      
         XC    WORKAREA(100),WORKAREA                                           
         MVC   NUSDREL,=XL2'0214'                                               
         CLI   NCHUADU,X'40'                                                    
         BE    DOUP485                                                          
         NI    NUSDST3,X'FD'        TURN OFF ADU FLAG                           
         CLI   NCHUADU,C'Y'                                                     
         BNE   DOUP485                                                          
         OI    NUSDST3,X'02'        TURN ON ADU FLAG                            
*                                                                               
DOUP485  CLI   TWPRROT,0                                                        
         BE    DOUP490                                                          
         MVC   NUSDROT,TWPRROT      MOVE IN ROTATION                            
DOUP490  BAS   RE,PUTUEL                                                        
*                                                                               
*  NO PAY/NO IMP FLAG                                                           
*                                                                               
DOUP500  GOTO1 VHELLO,DMCB,(C'G',=C'UNTFILE '),(X'18',AIOAREA1),0               
         CLI   12(R1),0                                                         
         BNE   DOUP520                                                          
         L     R6,12(R1)                                                        
         USING NUDTAD,R6                                                        
*                                                                               
         CLI   NCHUNPY,X'40'                                                    
         BE    DOUP510                                                          
         NI    NUUNST5,X'7F'       TURN OFF NO PAY                              
         CLI   NCHUNPY,C'Y'                                                     
         BNE   DOUP510                                                          
         OI    NUUNST5,X'80'       TURN ON NO PAY                               
DOUP510  CLI   NCHUNIM,X'40'                                                    
         BE    DOUP580                                                          
         NI    NUUNST5,X'BF'       TURN OFF NO IMP                              
         CLI   NCHUNIM,C'Y'                                                     
         BNE   DOUP580                                                          
         OI    NUUNST5,X'40'       TURN ON NO IMP                               
         B     DOUP580                                                          
*                                                                               
DOUP520  LA    R6,WORKAREA                                                      
         XC    WORKAREA(100),WORKAREA                                           
         MVC   NUDTAEL,=XL2'1832'                                               
         CLI   NCHUNPY,X'40'                                                    
         BE    DOUP530                                                          
         NI    NUUNST5,X'7F'        TURN OFF NO PAY                             
         CLI   NCHUNPY,C'Y'                                                     
         BNE   DOUP530                                                          
         OI    NUUNST5,X'80'        TURN ON NO PAY                              
DOUP530  CLI   NCHUNIM,X'40'                                                    
         BE    DOUP540                                                          
         NI    NUUNST5,X'BF'        TURN OFF NO IMP                             
         CLI   NCHUNIM,C'Y'                                                     
         BNE   DOUP540                                                          
         OI    NUUNST5,X'40'        TURN ON NO IMP                              
DOUP540  BAS   RE,PUTUEL                                                        
*                                                                               
*  LOOKUP DEMOS                                                                 
*                                                                               
DOUP580  CLI   NCHUWOD,X'40'        IS DEMO LOOKUP NEEDED                       
         BE    DOUP600                                                          
         GOTO1 VEDIT,DMCB,(C'D',(R5))                                           
         CLI   UNERROR,0                                                        
         BE    DOUP600                                                          
         MVC   NCHUERNO+1(1),UNERROR                                            
         MVI   NCHUERF,NCHUACTQ                                                 
         B     DOUP900                                                          
*                                                                               
*  HANDLE COMMENTS                                                              
*                                                                               
DOUP600  BAS   RE,DOCOMNTS          PROCESS COMMENTS                            
         CLI   BYTE,0                                                           
         BZ    DOUP850                                                          
         MVI   NCHUERNO+1,INVERR                                                
         MVC   NCHUERF,BYTE                                                     
         B     DOUP900                                                          
*                                                                               
*  CREATE ACTIVETY ELEMENT                                                      
*                                                                               
DOUP850  GOTO1 VEDIT,DMCB,(C'F',(R5))                                           
*                                                                               
*                                                                               
DOUP880  BAS   RE,SETDAY            SET THE DAY NUMBER                          
         BAS   RE,DOIOUN                                                        
         CLI   TWERROR,0                                                        
         BE    OVXIT2                                                           
         MVC   NCHUERNO+1(1),TWERROR                                            
         MVI   NCHUERF,NCHUACTQ                                                 
         MVI   TWERROR,0                                                        
         B     OVXIT2                                                           
*                                                                               
DOUP900  B     OVXIT2                                                           
         DROP  R5                                                               
         EJECT                                                                  
* LIST OF FIELDS TO EDIT                                                        
*                                                                               
EDLIST   DS    0H                                                               
* EDIT ROUTINE/FIELD LENGTH + HEADER/FIELD LENGTH/ERROR NUMBER                  
         DC    AL1(UACT),XL1'18',XL1'A',AL1(NCHUTRTQ)                           
         DC    AL1(UINT),XL1'17',XL1'9',AL1(NCHUIRTQ)                           
         DC    AL1(UASS),XL1'17',XL1'9',AL1(NCHUACSQ)                           
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*  PUT ELEMENT TO THE UNIT RECORD                                               
*                                                                               
PUTUEL   NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         B     OVXIT2                                                           
         EJECT                                                                  
*                                                                               
*--PROCESS THE COMMENTS                                                         
*                                                                               
DOCOMNTS NTR1                                                                   
         MVI   BYTE,0                                                           
         CLC   TWCMMNT1(120),SPACES                                             
         BE    DOCOMEX                                                          
* READ EXISTING COMMENTS                                                        
         MVI   WORKAREA+71,1                                                    
         MVI   WORKAREA+136,2                                                   
         MVC   WORKAREA+72(60),SPACES                                           
         MVC   WORKAREA+137(60),SPACES                                          
         LA    R3,WORKAREA+70                                                   
         GOTO1 VHELLO,DMCB,(C'G',=C'UNTFILE '),(X'04',AIOAREA1),0               
         CLI   12(R1),0                                                         
         BNE   DOCOM50                                                          
         L     R6,12(R1)                                                        
         USING NUCOMD,R6                                                        
         MVC   WORKAREA+70(2),NUCOMTYP                                          
         ZIC   RE,NUCOMLEN                                                      
         SH    RE,=H'5'                                                         
         EX    RE,*+4                                                           
         MVC   WORKAREA+72(0),NUCOMMNT                                          
* CHECK SECOND COMMENT                                                          
         ZIC   RE,NUCOMLEN                                                      
         AR    R6,RE                                                            
         CLI   0(R6),X'04'                                                      
         BNE   DOCOM50                                                          
         MVC   WORKAREA+135(2),NUCOMTYP                                         
         ZIC   RE,NUCOMLEN                                                      
         SH    RE,=H'5'                                                         
         EX    RE,*+4                                                           
         MVC   WORKAREA+137(0),NUCOMMNT                                         
* DELETE THE COMMENT ELEMENTS                                                   
DOCOM50  GOTO1 VHELLO,DMCB,(C'D',=C'UNTFIL  '),(X'04',AIOAREA1),0               
* PROCESS COMMENTS                                                              
         CLC   TWCMMNT1,SPACES                                                  
         BE    *+14                                                             
         MVC   WORKAREA+72(60),TWCMMNT1                                         
         MVI   WORKAREA+70,C'C'                                                 
         CLC   TWCMMNT2,SPACES                                                  
         BE    *+14                                                             
         MVC   WORKAREA+137(60),TWCMMNT2                                        
         MVI   WORKAREA+135,C'C'                                                
* BUILD THE ELEMENTS                                                            
         CLC   WORKAREA+72(60),SPACES                                           
         BE    DOCOM100                                                         
         CLC   WORKAREA+72(3),=CL3'###'                                         
         BE    DOCOM100                                                         
         LA    R6,WORKAREA+70                                                   
         BAS   RE,BDCOMELM                                                      
         B     *+8                                                              
DOCOM100 MVI   WORKAREA+136,1                                                   
         CLC   WORKAREA+137(60),SPACES                                          
         BE    DOCOMEX                                                          
         CLC   WORKAREA+137(3),=CL3'###'                                        
         BE    DOCOMEX                                                          
         LA    R6,WORKAREA+135                                                  
         BAS   RE,BDCOMELM                                                      
DOCOMEX  B     OVXIT2                                                           
         SPACE 2                                                                
*                                                                               
* BUILD THE COMMENT ELEMENT (R6 POINTS TO INPUT DATA)                           
*                                                                               
BDCOMELM NTR1                                                                   
         XC    WORKAREA(64),WORKAREA                                            
         MVI   WORKAREA,X'04'                                                   
         MVC   WORKAREA+2(2),0(R6)                                              
* GET LENGTH OF COMMENT                                                         
         LR    RF,R6                                                            
         LA    RE,60                                                            
         LA    RF,61(RF)             POINT TO END OF COMMENT                    
BCMEL30  CLI   0(RF),X'40'                                                      
         BH    BCMEL50                                                          
         BCTR  RF,0                  POINT TO NEXT FIELD                        
         BCT   RE,BCMEL30                                                       
BCMEL50  LA    RE,4(RE)                                                         
         STCM  RE,1,WORKAREA+1                                                  
         MVC   WORKAREA+4(60),2(R6)                                             
         BAS   RE,PUTUEL                                                        
         B     OVXIT2                                                           
*                                                                               
*--VALIDATE THE FIELDS PASSED                                                   
*                                                                               
DOUPVAL  NTR1                                                                   
*                                                                               
         MVI   NCHUERF,0                                                        
*                                                                               
*--UNIT ID                                                                      
         XC    TWKEY,TWKEY                                                      
         CLC   NCHUUID,SPACES                                                   
         BE    DOVAL05                                                          
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CHEXIN                                                        
         GOTO1 (RF),DMCB,NCHUUID,TWKEY,L'NCHUUID,0                              
         DROP  RE                                                               
*                                                                               
*--PROGRAM CODE                                                                 
DOVAL05  MVC   TWPRCODE,NCHUPROG                                                
*                                                                               
*--COMMENTS                                                                     
         MVC   TWCMMNT1,NCHUCM1                                                 
         MVC   TWCMMNT2,NCHUCM2                                                 
*                                                                               
*--PROGRAM NAME                                                                 
         XC    TWPRNAME,TWPRNAME                                                
         CLI   NCHUPNM,X'40'                                                    
         BNE   DOVAL10                                                          
         CLI   NCHUACT,C'A'                                                     
         BNE   DOVAL20                                                          
* GET DEFAULT NAME                                                              
         L     RE,APROGEL                                                       
         USING NPGEL92,RE                                                       
         MVC   TWPRNAME,NPGNAME                                                 
         B     DOVAL20                                                          
*                                                                               
DOVAL10  MVC   TWPRNAME,NCHUPNM                                                 
         DROP  RE                                                               
*                                                                               
*--CALCULATE PROGRAM LENGTH                                                     
DOVAL20  MVI   TWPLEN,0                                                         
         CLI   NCHUULN,X'40'                                                    
         BNE   *+18                                                             
         CLI   NCHUACT,C'A'                                                     
         BNE   DOVAL30                                                          
         MVC   NCHUULN,=CL7'30     '    DEFAULT LENGTH ON ADD                   
*                                                                               
         MVI   TWPLEN,X'FF'         LENGTH INPUTTED SWITCH                      
*                                                                               
*--CALCULATE ROTATION                                                           
DOVAL30  MVI   TWPRROT,0                                                        
         CLI   NCHUROT,X'40'                                                    
         BNE   DOVAL50                                                          
         CLI   NCHUACT,C'A'                                                     
         BNE   DOVAL220                                                         
* GET DEFAULT ROTATION                                                          
         L     RE,APROGEL                                                       
         USING NPGEL92,RE                                                       
         MVC   TWPRROT,NPGROT                                                   
         B     DOVAL220                                                         
         DROP  RE                                                               
*                                                                               
DOVAL50  XC    TWPRROT(4),TWPRROT  CLEAR THE DAY FIELDS                         
         MVC   TWPROTE,NCHUROT                                                  
         LA    R1,ROTABLE                                                       
         LA    RE,NCHUROT                                                       
DOVAL100 CLI   0(R1),X'FF'                                                      
         BE    DOVAL180                                                         
         CLI   0(RE),C'Y'                                                       
         BNE   DOVAL140                                                         
         OC    TWPRROT,2(R1)                                                    
         NI    TWPRROTN,X'F0'      CLEAR END DAT NUMBER                         
         OC    TWPRROTN,5(R1)      END DAY NUMBER                               
         TM    TWPRROTN,X'F0'                                                   
         BNZ   DOVAL140                                                         
         NI    TWPRROTN,X'0F'      CLEAR START DAY NUMBER                       
         OC    TWPRROTN,4(R1)      START DAY NUMBER                             
         MVC   TWDAYNO,5(R1)      START DAY NUMBER (NUMERIC)                    
         MVC   TWDAYHEX,3(R1)     START DAY NUMBER (HEX)                        
DOVAL140 LA    R1,6(R1)                                                         
         LA    RE,1(RE)                                                         
         B     DOVAL100                                                         
*                                                                               
DOVAL180 CLI   TWDAYNO,0                                                        
         BNE   DOVAL220                                                         
         MVI   NCHUERNO+1,INVERR                                                
         MVI   NCHUERF,NCHUROTQ                                                 
         B     DOVAL900                                                         
         SPACE 2                                                                
*                                                                               
*--CALCULATE START AND END TIMES                                                
*                                                                               
DOVAL220 XC    TWPRSTIM(4),TWPRSTIM                                             
         CLI   NCHUSTM,X'40'                                                    
         BNE   DOVAL240                                                         
         CLI   NCHUACT,C'A'                                                     
         BNE   DOVAL300                                                         
* GET DEFAULT ROTATION                                                          
         L     RE,APROGEL                                                       
         USING NPGEL92,RE                                                       
         MVC   TWPRSTIM(4),NPGTIME                                              
         B     DOVAL280                                                         
         DROP  RE                                                               
*                                                                               
DOVAL240 PACK  DUB(8),NCHUSTM(4) START TIME                                     
         CVB   RE,DUB                                                           
         STCM  RE,3,TWPRSTIM                                                    
*                                                                               
         PACK  DUB(8),NCHUETM(4) END TIME                                       
         CVB   RE,DUB                                                           
         STCM  RE,3,TWPRETIM                                                    
*-CHECK START AND END FOR GREATER THEN 2400                                     
*                                                                               
         CLC  TWPRETIM,=XL2'0961'                                               
         BL   *+16                                                              
         MVI  NCHUERF,NCHUSTMQ                                                  
         MVI  NCHUERNO+1,INVERR                                                 
         B    DOVAL900                                                          
*                                                                               
         CLC  TWPRSTIM,=XL2'0961'                                               
         BL   *+16                                                              
         MVI  NCHUERF,NCHUETMQ                                                  
         MVI  NCHUERNO+1,INVERR                                                 
         B    DOVAL900                                                          
*-CHECK START AND END FOR ZERO                                                  
         OC   TWPRETIM,TWPRETIM                                                 
         BNZ  *+16                                                              
         MVI  NCHUERF,NCHUETMQ                                                  
         MVI  NCHUERNO+1,INVERR                                                 
         B    DOVAL900                                                          
*                                                                               
         OC   TWPRSTIM,TWPRSTIM                                                 
         BNZ  *+16                                                              
         MVI  NCHUERF,NCHUSTMQ                                                  
         MVI  NCHUERNO+1,INVERR                                                 
         B    DOVAL900                                                          
*                                                                               
*  SET START QUARTER HOUR                                                       
*                                                                               
DOVAL280 SR    R1,R1                                                            
         ICM   R1,3,TWPRSTIM       START TIME                                   
         SR    R0,R0                                                            
         D     R0,=F'100'          R1=HOURS, R0=REMAINDER MINUTES               
         MH    R1,=H'60'           CONVERT HOURS TO MINUTES                     
         AR    R1,R0               SUM TOTAL MINUTES                            
         CH    R1,=H'360'          TEST FOR LESS THAN 6 AM                      
         BNL   *+8                                                              
         AH    R1,=Y(60*24)        ADD MINUTES OF 24 HOURS                      
         SH    R1,=H'360'          SUBTRACT 6 HOURS TO BASE OFF 6AM             
         SR    R0,R0                                                            
         D     R0,=F'15'           DIVIDE BY MINUTES IN A QUARTER HOUR          
         STC   R1,TWQTRHR                                                       
*                                                                               
*--SET AIR DATE                                                                 
*                                                                               
DOVAL300 XC    TWPDATE,TWPDATE                                                  
         XC    TWDAYLIT,TWDAYLIT                                                
         CLI   NCHUWOD,X'40'                                                    
         BNE   DOVAL320                                                         
         CLI   NCHUACT,C'A'                                                     
         BNE   DOVAL310                                                         
         MVI   NCHUERNO+1,INVERR                                                
         MVI   NCHUERF,8                                                        
         B     DOVAL900                                                         
*                                                                               
DOVAL310 MVC   TWPDATE,TWKEY+2                                                  
         B     DOVAL900                                                         
*                                                                               
DOVAL320 GOTO1 VGETDAY,DMCB,NCHUWOD,TWDAYLIT                                    
         GOTO1 VDATCON,DMCB,(0,NCHUWOD),(2,TWPDATE)                             
         GOTO1 VDATCON,DMCB,(2,TWPDATE),(0,DUB)                                 
*                                                                               
         L     R5,AIOAREA4                                                      
         USING BUYVALD,R5                                                       
*                                                                               
*******  CLC   NCHUWOD(6),ESTSTART     TEST IF DATE BEFORE EST START            
         CLC   DUB(6),ESTSTART         TEST IF DATE BEFORE EST START            
         BL    DOVAL350                                                         
         CLC   DUB(6),ESTEND           TEST IF DATE AFTER EST END               
         BH    DOVAL350                                                         
*******  SR    R0,R0                                                            
*******  ICM   R0,1,BUYPROF+14                                                  
*******  BZ    DOVAL350                                                         
*******  GOTO1 VADDAY,(R1),ESTEND,DUB2,(R0)                                     
*******  CLC   DUB(6),DUB2         TEST IF ESTEND + PROFILE                     
*******  BH    DOVAL350            NO-SAME YEAR AS EST START                    
*                                                                               
*  CHECK THAT DATE IS A VALID CALANDER DATE                                     
*                                                                               
         GOTO1 VDATCON,(R1),(0,DUB),(5,DUB2)                                    
         GOTO1 VDATVAL,(R1),(0,DUB2),DUB                                        
         OC    0(4,R1),0(R1)                                                    
         BNZ   DOVAL900                                                         
*                                                                               
DOVAL350 MVI   NCHUERNO+1,DATERR                                                
         MVI   NCHUERF,NCHUWODQ                                                 
         B     DOVAL900                                                         
*                                                                               
*                                                                               
DOVAL900 B     OVXIT2                                                           
*                                                                               
ROTABLE  DC    CL2'MO',XL4'40401001'         MON                                
         DC    CL2'TU',XL4'20202002'         TUE                                
         DC    CL2'WE',XL4'10103003'         WED                                
         DC    CL2'TH',XL4'08084004'         THU                                
         DC    CL2'FR',XL4'04045005'         FRI                                
         DC    CL2'SA',XL4'02026006'         SAT                                
         DC    CL2'SU',XL4'01017007'         SUN                                
         DC    CL2'MF',XL4'7C401001'         MON-FRI                            
         DC    CL2'WK',XL4'7F401001'         MON-SUN                            
         DC    X'FF'                                                            
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    READS THE PROGRAM RECORD                                                   
*    SETS THE POINTERS TO BE USED IN THE VEDIT MODULE                           
*                                                                               
GETPRG   NTR1                                                                   
         DS    0H                                                               
         L     R4,UPACURUN         ADDRESS OF UNIT RECORD                       
         USING NCHUD,R4            UNIT RECORD DSECT                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   NCHUERNO+1,0        CLEAR ERROR BYTE ON ENTRANCE                 
         LA    R5,KEY                                                           
         USING NPGRECD,R5                                                       
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,TWAGYMED                                                  
         MVC   NPGKNET,TWMKT                                                    
         MVC   NPGKPROG(8),DUB                                                  
         L     R2,APROGREC                                                      
         CLC   NPGKEY,0(R2)        TEST IF KEY CHANGED                          
         BE    GETPROGX            NO-ALREADY HAVE RIGHT RECORD                 
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
*                                                                               
GETPRG1  CLC   KEY(NPGKEND-NPGKEY),KEYSAVE TEST IF SAME PROGRAM                 
         BE    GETPRG3                                                          
*                                                                               
         MVI   NCHUERNO+1,PROGERR                                               
         B     GETPROGX                                                         
*                                                                               
GETPRG2  GOTO1 AIO,DMCB,SPT+DIR+SEQ                                             
         B     GETPRG1                                                          
         SPACE                                                                  
GETPRG3  GOTO1 AIO,DMCB,SPT+FILE+GET,(R2)                                       
         SPACE                                                                  
         MVI   ELCODE,X'93'                                                     
         BAS   RE,GTPRGEL                                                       
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BNE   GETPRG4                                                          
         ICM   RE,15,12(R1)                                                     
         USING NPG2ELEM,RE                                                      
         OC    NPG2STD,NPG2STD                                                  
         BZ    GETPRG4                                                          
         CLC   HALF,NPG2STD      MAKE SURE INPUT DATE > PRG START DATE          
         BL    GETPRG2                                                          
         DROP  RE                                                               
*                                                                               
GETPRG4  MVC   TWPRCODE,NPGKPROG   SAVE PROGRAM CODE                            
*                                                                               
         MVI   ELCODE,X'5D'        BOOK ELEMENT SEARCH                          
         BAS   RE,GTPRGEL                                                       
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ABOOKEL,12(R1)      SAVE ELEMENT ADCON                           
*                                                                               
GETPRG5  DS    0H                                                               
         MVI   ELCODE,X'92'        PROGRAM ELEMENT SEARCH                       
         BAS   RE,GTPRGEL                                                       
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BE    *+6                                                              
         DC    H'0'                MUST HAVE ONE                                
         MVC   APROGEL,12(R1)      SAVE ELEMENT ADCON                           
         B     GETPROGX                                                         
         SPACE                                                                  
GETPROGX B     OVXIT2                                                           
         DROP  R4,R5                                                            
*                                                                               
*  GET PROGRAM RECORD ELEMENTS                                                  
GTPRGEL  NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE '),(ELCODE,(R2)),0                  
         B     OVXIT2                                                           
         EJECT                                                                  
* CKREASN - CHECKS THAT REASON CODE IS VALID                                    
*                                                                               
* AT ENTRY P1 CONTAINS A(4 BYTE REASON CODE)                                    
*                                                                               
CKREASN  NTR1                                                                   
         L     R4,UPACURUN         ADDRESS OF UNIT RECORD                       
         USING NCHUD,R4            UNIT RECORD DSECT                            
         MVI   NCHUERF,0                                                        
*                                                                               
         XC    AUDREASN,AUDREASN                                                
         MVC   AUDREASN(3),NCHURESN                                             
         OC    AUDREASN,SPACES                                                  
*                                                                               
*****    L     RE,APACKREC                                                      
*****    USING NPRECD,RE                                                        
         TM    TWPKSTAT,X'02'      IS REASON CODE REQUIRED                      
         BO    CKREA50             YES                                          
         CLC   AUDREASN,SPACES     WAS REASON CODE INPUTTED                     
         BE    CKREASNX                                                         
         B     CKREAER2                                                         
CKREA50  CLC   AUDREASN,SPACES     WAS REASON CODE INPUTTED                     
         BE    CKREAER2                                                         
*                                                                               
CKREA100 LA    RE,KEY                                                           
         USING RSNRECD,RE                                                       
         XC    KEY,KEY                                                          
         MVC   RSNKTYPE,=X'0D77'                                                
         MVC   RSNKAGY,TWAGY                                                    
         MVI   RSNKMED,C'N'                                                     
         MVC   RSNKCODE(3),AUDREASN                                             
         OC    RSNKCODE,SPACES                                                  
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
                                                                                
         CLC   KEY(9),KEYSAVE      TEST IF SAME RECORD APPLIES                  
         BE    CKREASNX            YES                                          
CKREAER1 MVI   NCHUERNO+1,INVREASN                                              
         MVI   NCHUERF,NCHURESQ                                                 
         B     CKREASNX                                                         
CKREAER2 MVI   NCHUERNO+1,AUDITERR                                              
         MVI   NCHUERF,NCHURESQ                                                 
CKREASNX B     OVXIT2                                                           
         DROP  R4,RE                                                            
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO EDIT PREEMPTS                                                  
*                                                                               
DOPREMPT NTR1                                                                   
         L     R4,UPACURUN         ADDRESS OF UNIT RECORD                       
         USING NCHUD,R4            UNIT RECORD DSECT                            
*                                                                               
         MVI   NCHUERF,0                                                        
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
         MVC   BYTE,NUUNITST                                                    
         NI    NUUNITST,X'BF'                                                   
*                                                                               
         CLI   NCHUPRE,NO          TEST FOR 'NO'                                
         BE    EPREEX              YES                                          
         MVI   NCHUERNO+1,INVERR                                                
         CLI   NCHUPRE,C'Y'        TEST FOR 'YES'                               
         BNE   EPREERR                                                          
         MVI   NCHUERNO+1,PREERR   MUST REMOVE MAKE-GOOD REFERENCE              
         TM    NUUNITST,X'02'      TEST IF UNIT MISSED                          
         BO    EPREERR                                                          
         OI    NUUNITST,X'40'      UNIT STATUS IS PRE-EMPT                      
*                                                                               
         L     R3,AIOAREA1         R3 POINTS TO RECORD                          
         GOTO1 VHELLO,DMCB,(C'G',=C'UNTFILE '),(X'02',(R3)),0                   
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)                                                        
         OI    5(RE),X'08'         LAST CHANGE IS A PREEMPT                     
         SPACE 1                                                                
*-CHECK PROFILE TO SEE IF PRE-EMPT CAN BE CHANGED IF UNIT IS PAYED              
         LA    R5,BLOCK                                                         
         USING UNBLOCKD,R5                                                      
         TM    BYTE,X'40'          WAS IT SET                                   
         BNZ   EPREEX              YES, NO CHANGE IN STATUS                     
         CLI   BUYPROF2+13,C'A'                                                 
         BE    EPR100                                                           
         CLI   BUYPROF2+13,C'B'                                                 
         BE    EPR100                                                           
         CLI   BUYPROF2+13,C'C'                                                 
         BNE   EPREEX                                                           
EPR100   CLI   UNPAYSW,YES                                                      
         BNE   EPREEX                                                           
         MVI   NCHUERNO+1,PAYCHGNA                                              
         B     EPREERR                                                          
*                                                                               
EPREERR  MVI   NCHUERF,NCHUPREQ                                                 
EPREEX   B     OVXIT2                                                           
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* GET NEXT OBJECT                                                     *         
***********************************************************************         
OVNEXT   NTR1                                                                   
OVNEXT1  SR    R1,R1                                                            
         L     R4,UPACUROB                                                      
         ICM   R1,3,0(R4)          ADVANCE TO NEXT OBJECT                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RE,2(R1,R4)                                                      
         CLC   14(4,RE),=CL4'PROG'                                              
         BE    OVNEXT2                                                          
         CLC   14(4,RE),=CL4'UNIT'                                              
         BE    OVNEXT2                                                          
         CLC   14(4,RE),=CL4'UNUP'                                              
         BNE   *+8                                                              
OVNEXT2  LA    RE,2(RE)                                                         
         ST    RE,UPACUROB                                                      
         LR    R4,RE                                                            
*                                                                               
         CLC   0(2,R4),=X'FFFF'    TEST END OF TWA                              
         BNE   OVNEXT5                                                          
         BAS   RE,OVWRTTWA         YES-WRITE CURRENT TWA                        
         ZIC   R1,UPCURTWA                                                      
         LA    R1,1(R1)                                                         
         STC   R1,UPCURTWA                                                      
         BAS   RE,OVRDTWA          GET NEXT TEMPSTR PAGE                        
         L     R4,UPACUROB                                                      
*                                                                               
OVNEXT5  B     OVXIT2                                                           
         EJECT                                                                  
***********************************************************************         
* GET TEMPSTOR PAGE                                                   *         
***********************************************************************         
         SPACE 1                                                                
OVRDTWA  NTR1  ,                                                                
         CLI   UPCURTWA,10         DON'T READ BEYOND TWA10                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         IC    R4,UPCURTWA         READ TWA3 INTO TIA                           
         SLL   R4,24                                                            
         ICM   R4,3,TERM           TERMINAL NUMBER                              
         MVC   DMCB+20(2),=C'L='   LARGE TEMPSTR SIZE                           
         MVC   DMCB+22(2),LENTWAO                                               
         GOTO1 VDATAMGR,DMCB,DMREADO,TEMPSTRO,(R4),ATIA                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,ATIA                                                          
         CLC   14(4,RE),=CL4'PROG'                                              
         BE    OVRDTW80                                                         
         CLC   14(4,RE),=CL4'UNUP'                                              
         BE    OVRDTW80                                                         
         CLC   14(4,RE),=CL4'UNIT'                                              
         BNE   OVRDTWX                                                          
OVRDTW80 LA    RE,2(RE)                                                         
*                                                                               
OVRDTWX  ST    RE,UPACUROB                                                      
         B     OVXIT2                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE TIA BACK TO TEMPSTOR PAGE                                     *         
***********************************************************************         
         SPACE 1                                                                
OVWRTTWA NTR1  ,                                                                
         IC    R4,UPCURTWA                                                      
         SLL   R4,24                                                            
         ICM   R4,3,TERM           TERMINAL NUMBER                              
         GOTO1 VDATAMGR,DMCB,DMWRITEO,TEMPSTRO,(R4),ATIA                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     OVXIT2                                                           
         EJECT                                                                  
*                                                                               
*                                                                               
*--DO IO LOGIC FOR UNIT                                                         
*                                                                               
SETDAY   NTR1                                                                   
*                                                                               
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
*                                                                               
         MVI   TWDAYNO,X'0A'       PRE-SET DAY CODE TO VAR                      
         LA    RE,DAYNOTAB                                                      
         LA    R0,9                                                             
SETDAY10 CLC   NUDAY,0(RE)                                                      
         BE    SETDAY20                                                         
         LA    RE,L'DAYNOTAB(RE)                                                
         BCT   R0,SETDAY10                                                      
         B     SETDAYEX            NOT IN TABLE                                 
SETDAY20 MVC   TWDAYNO,1(RE)       EXTRACT DAY VALUE FROM TABLE                 
*                                                                               
SETDAYEX B     OVXIT2                                                           
*                                                                               
DAYNOTAB DS    0CL2                                                             
         DC    X'4001'              MONDAY                                      
         DC    X'2002'              TUESDAY                                     
         DC    X'3003'              WED                                         
         DC    X'0804'              THU                                         
         DC    X'0405'              FRIDAY                                      
         DC    X'0206'              SAT                                         
         DC    X'0107'              SUN                                         
         DC    X'7C08'              M-FR                                        
         DC    X'7F09'              M-SU                                        
         EJECT                                                                  
*                                                                               
*                                                                               
*--DO IO LOGIC FOR UNIT                                                         
*                                                                               
DOIOUN   NTR1                                                                   
         MVI   TWTIMCH,C'N'                                                     
*                                                                               
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
*                                                                               
         USING NCHUD,R4            UNIT RECORD DSECT                            
         ST    R4,UPACURUN         ADDRESS OF UNIT RECORD                       
*                                                                               
*-TEST FOR ADD                                                                  
         CLI   NCHUACT,C'A'                                                     
         BNE   DOIOU050                                                         
         BAS   RE,WRITREC           WRITE RECORD OUT                            
         B     DOIOUX                                                           
*                                                                               
*-SEE IF KEY CHANGED                                                            
DOIOU050 CLC   TWKEY,NUKCLT                                                     
         BNE   DOIOU80                                                          
         MVC   KEY(20),NUKEY                                                    
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,DMCB,UNT+FILE+GET+UPDATE,AIOAREA2                            
         GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA1                                   
         B     DOIOUX                                                           
*                                                                               
*-HANDLE KEY CHANGE                                                             
*                                                                               
*                                                                               
DOIOU80  MVC   KEY(2),NUKTYPE                                                   
         MVC   KEY+2(18),TWKEY                                                  
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+16                                                             
         MVI   NCHUERNO+1,53                                                    
         MVI   NCHUERF,NCHUACTQ                                                 
         B     DOIOU900                                                         
*-GET THE RECORD                                                                
         L     R3,AIOAREA3                                                      
         GOTO1 AIO,DMCB,UNT+FILE+GET+UPDATE,AIOAREA3                            
*-DELETE THE RECORD                                                             
*                                                                               
         MVC   FULL(1),NUDAY                                                    
         OI    NURSTAT,X'80'                                                    
*-ADD HISTORY RECORD INFORMATIO TO THE UNIT                                     
*                                                                               
         XC    WORKAREA(30),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUNKYD,RE                                                        
         MVI   NUNKYEL,X'90'                                                    
         MVI   NUNKYLEN,22                                                      
         MVC   NUNKYKEY,NUKEY                                                   
         DROP  RE                                                               
         L     R3,AIOAREA3                                                      
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA3,WORKAREA,0              
*                                                                               
         GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA3                                   
*--DELETE THE 04 KEY                                                            
         LA    R3,KEY                                                           
         OI    NUKSTAT,X'80'                                                    
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
*-BUILD 94 KEY                                                                  
         XC    KEY,KEY                                                          
         MVI   NUKDTYPE,X'94'                                                   
         MVC   NUKDAM,TWAGYMED                                                  
         MVC   NUKDCLT,TWKEY                                                    
         MVC   NUKDEST,TWKEY+15                                                 
         MVC   NUKDNET,TWKEY+5                                                  
         MVC   NUKDTIME,TWKEY+4                                                 
         MVC   NUKDPROG,TWKEY+9                                                 
         MVC   NUKDDATE,TWKEY+2                                                 
         MVC   NUKDSUB,TWKEY+16                                                 
*-GET DAY                                                                       
         LA    RE,ROTABLE                                                       
         LA    RF,9                                                             
DOIOU300 CLC   FULL(1),3(RE)                                                    
         BE    DOIOU320                                                         
         LA    RE,6(RE)                                                         
         BCT   RF,DOIOU300                                                      
         DC    H'0'                                                             
*                                                                               
DOIOU320 MVC   NUKDDAY,5(RE)                                                    
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE   *+6                                                               
         DC   H'0'                                                              
*--DELETE THE 94 KEY                                                            
         LA    R3,KEY                                                           
         OI    NUKSTAT,X'80'                                                    
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
*-CHECK FOR TIME CHANGE ONLY                                                    
         L     R3,AIOAREA1                                                      
         CLC   TWKEY(4),NUKEY+2                                                 
         BNE   *+14                                                             
         CLC   TWKEY+5(13),NUKEY+7                                              
         BE    DOIOU500                                                         
*-UPDATE THE LINE NUMBER                                                        
         L     R3,AIOAREA1                                                      
         BAS   RE,GETLST                                                        
*-BUILD 84 KEY                                                                  
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,TWAGYMED                                                  
         MVC   NUKPCLT,TWKEY                                                    
         MVC   NUKPNET,TWKEY+5                                                  
         MVC   NUKPPROG,TWKEY+9                                                 
         MVC   NUKPDATE,TWKEY+2                                                 
         MVC   NUKPEST,TWKEY+15                                                 
         MVC   NUKPSUB,TWKEY+16                                                 
         MVC   NUKPDP,TWKEY+17                                                  
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE   *+6                                                               
         DC   H'0'                                                              
*--DELETE THE 84 KEY                                                            
         OI    NUKSTAT,X'80'                                                    
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
*-WRITE THE RECORD BACK                                                         
*                                                                               
         BAS   RE,WRITREC                                                       
         B     DOIOU700                                                         
*                                                                               
*  CHANGE OF TIME ONLY                                                          
*                                                                               
DOIOU500 MVC   TWLSTLIN,TWKEY+16                                                
*                                                                               
         MVC   KEY(20),NUKEY                                                    
         GOTO1 AIO,DMCB,UNT+DIR+HIGH+PASSDEL+UPDATE                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   DOIOU520                                                         
         TM    KEY+20,X'80'        IF DELETED                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TWDSKADR(4),KEY+21                                               
         NI    KEY+20,X'7F'                                                     
         GOTO1 AIO,DMCB,UNT+DIR+WRITE  WRITE BACK 04 KEY                        
         GOTO1 AIO,DMCB,UNT+FILE+GET+UPDATE+PASSDEL,AIOAREA2                    
         GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA1  WRITE BACK THE RECORD            
         B     DOIOU530                                                         
*                                                                               
DOIOU520 L     R3,AIOAREA1         THEN ADD IT                                  
         GOTO1 AIO,DMCB,UNT+FILE+ADDREC,(R3)                                    
         MVC   TWDSKADR,NDXDA      SAVE THE DISK ADDRESS                        
*                                                                               
DOIOU530 BAS   RE,BLDKEY94                                                      
         GOTO1 AIO,DMCB,UNT+DIR+HIGH+PASSDEL                                    
         CLC   KEY(20),KEYSAVE                                                  
         BE    DOIOU550                                                         
         MVC   KEY(20),KEYSAVE                                                  
         NI    KEY+20,X'FF'-X'83'                                               
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
         GOTO1 AIO,DMCB,UNT+DIR+ADD                                             
         B     DOIOU600                                                         
*                                                                               
DOIOU550 TM    KEY+20,X'80'        IF DELETED                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                  THEN UNDELETE IT AND WRITE RECORD            
         NI    KEY+20,X'FF'-X'83'                                               
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
*                                                                               
DOIOU600 BAS   RE,BLDKEY84                                                      
         GOTO1 AIO,DMCB,UNT+DIR+HIGH+PASSDEL                                    
         CLC   KEY(20),KEYSAVE                                                  
         MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
         DROP  R3                                                               
*                                                                               
* UPDATE UNIT BILLING RECORDS                                                   
DOIOU700 L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
         XC    WORKAREA,WORKAREA                                                
         MVC   WORKAREA(20),NUKEY          NEW KEY                              
         MVC   WORKAREA+20(2),NUKTYPE      OLD KEY                              
         MVC   WORKAREA+22(18),TWKEY                                            
         PRINT GEN                                                              
         GOTO1 VCHGBILL,DMCB,WORKAREA,WORKAREA+20,AIOAREA3,WORKAREA+40          
         PRINT NOGEN                                                            
*                                                                               
DOIOUX   MVI   NCHUERNO+1,0                                                     
         MVI   NCHUERF,0                                                        
DOIOU900 B     OVXIT2                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
*--THIS ROUTINE WRITES THE BUY RECORD TO THE FILE.                              
*                                                                               
WRITREC  NTR1                                                                   
WRTREC   DS    0H                                                               
*                                                                               
         MVI   TWRITESW,C'Y'       UNIT BEING WRITTEN SWITCH                    
*                                  READ DELETED RECORDS                         
         BAS   RE,GETLST           GET LAST LINE NUMBER                         
         CLI   TWERROR,0                                                        
         BNE   WRITEX                                                           
*                                  READ FOR UPDATE                              
         BAS   RE,BLDKEY84         BUILD 84 KEY                                 
         GOTO1 AIO,DMCB,UNT+DIR+HIGH+PASSDEL+UPDATE                             
*                                  IF RECORD ISN'T ALREADY THERE                
         CLC   KEY(20),KEYSAVE                                                  
         BE    WRITE110                                                         
*                                                                               
         L     R3,AIOAREA1         THEN ADD IT                                  
         GOTO1 AIO,DMCB,UNT+FILE+ADDREC,(R3)                                    
         MVC   TWDSKADR,NDXDA      SAVE THE DISK ADDRESS                        
         MVI   WRITESW,C'Y'        SET UNITS WRITTEN TO YES                     
*                                                                               
         BAS   RE,BLDKEY84         BUILD 84 KEY                                 
         CLI   TWTIMCH,C'Y'                                                     
         BNE   WRITE020                                                         
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+21(4),TWDSKADR                                               
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
         B     WRITE030                                                         
*                                                                               
WRITE020 MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
         GOTO1 AIO,DMCB,UNT+DIR+ADD                                             
*                                                                               
WRITE030 BAS   RE,BLDKEY94         BUILD 94 KEY                                 
         MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
         GOTO1 AIO,DMCB,UNT+DIR+ADD                                             
*                                                                               
         B     WRITEX                                                           
*                                                                               
WRITE110 TM    KEY+20,X'80'  CHECK DELETE                                       
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWDSKADR(4),KEY+21                                               
         TM    KEY+20,X'80'        IF DELETED                                   
         BZ    WRITE120                                                         
*                                  THEN UNDELETE IT AND WRITE RECORD            
         NI    KEY+20,X'FF'-X'83'                                               
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
*                                                                               
WRITE120 BAS   RE,BLDKEY94                                                      
         GOTO1 AIO,DMCB,UNT+DIR+HIGH+PASSDEL                                    
         CLC   KEY(20),KEYSAVE                                                  
         BE    WRITE125                                                         
         MVC   KEY(20),KEYSAVE                                                  
         NI    KEY+20,X'FF'-X'83'                                               
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
         GOTO1 AIO,DMCB,UNT+DIR+ADD                                             
         B     WRITE130                                                         
*                                                                               
WRITE125 TM    KEY+20,X'80'        IF DELETED                                   
         BZ    WRITE130                                                         
*                                  THEN UNDELETE IT AND WRITE RECORD            
         NI    KEY+20,X'FF'-X'83'                                               
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
*                                                                               
WRITE130 L     RE,AIOAREA1         BUILD 04 KEY                                 
         USING NURECD,RE                                                        
         MVC   KEY(20),NUKEY                                                    
         GOTO1 AIO,DMCB,UNT+DIR+HIGH+PASSDEL                                    
         CLC   KEY(20),KEYSAVE                                                  
         BE    WRITE135                                                         
         MVC   KEY(20),KEYSAVE                                                  
         NI    KEY+20,X'FF'-X'83'                                               
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
         GOTO1 AIO,DMCB,UNT+DIR+ADD                                             
         B     WRITE140                                                         
*                                                                               
WRITE135 TM    KEY+20,X'80'        IF DELETED                                   
         BZ    WRITE140                                                         
*                                  THEN UNDELETE IT AND WRITE RECORD            
         NI    KEY+20,X'FF'-X'83'                                               
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
*                                                                               
WRITE140 GOTO1 AIO,DMCB,UNT+FILE+GET+UPDATE+PASSDEL,AIOAREA3                    
*                                                                               
         MVC   AIOAREA,AIOAREA1    WRITE OUR RECORD IN ITS PLACE                
         GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA1   PUT NEW RECORD                  
         MVI   WRITESW,C'Y'        SET UNITS WRITTEN TO YES                     
*                                  DON'T READ DELETED RECORDS                   
WRITEX   B     OVXIT2              RETURN TO CALLER                             
         DROP  RE                                                               
         EJECT                                                                  
*-SUB-ROUTINE TO BUILD THE 94 KEY                                               
*                                                                               
BLDKEY94 NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING NURECD,RE                                                        
         MVI   NUKDTYPE,X'94'                                                   
         MVC   NUKDAM,TWAGYMED                                                  
         MVC   NUKDCLT,TWCLIENT                                                 
         MVC   NUKDEST,TWEST                                                    
         MVC   NUKDNET,TWNET                                                    
         MVC   NUKDDAY,TWDAYNO                                                  
         MVC   NUKDTIME,TWQTRHR                                                 
         MVC   NUKDPROG,TWPRCODE                                                
         MVC   NUKDDATE,TWPDATE                                                 
         MVC   NUKDSUB,TWLSTLIN                                                 
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         B     OVXIT2                                                           
         DROP  RE                                                               
         EJECT                                                                  
*-SUB-ROUTINE TO BUILD THE 84 KEY                                               
*                                                                               
BLDKEY84 NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING NURECD,RE                                                        
*        L     RF,AIOAREA1                                                      
*        USING NURECD,RF                                                        
*                                                                               
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,TWAGYMED                                                  
         MVC   NUKPCLT,TWCLIENT                                                 
         MVC   NUKPNET,TWNET                                                    
         MVC   NUKPPROG,TWPRCODE                                                
         MVC   NUKPDATE,TWPDATE                                                 
         MVC   NUKPEST,TWEST                                                    
         MVC   NUKPSUB,TWLSTLIN                                                 
         MVC   NUKPDP,TWDYPT                                                    
*                                                                               
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         B     OVXIT2                                                           
         DROP  RE                                                               
*                                                                               
*                                                                               
*--GET LAST LINE NUMBER FOR 84 KEY                                              
*                                                                               
GETLST   NTR1                                                                   
         MVI   TWERROR,0                                                        
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
         L     R4,UPACURUN                                                      
         USING NUNTD,R4                                                         
         MVI   TWLSTLIN,0                                                       
         BAS   RE,BLDKEY84                                                      
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         B     GETL150                                                          
GETL100  GOTO1 AIO,DMCB,UNT+DIR+SEQ                                             
GETL150  CLC   KEY(17),KEYSAVE                                                  
         BNE   GETL200                                                          
         MVC   TWLSTLIN,KEY+17                                                  
         B     GETL100                                                          
GETL200  ZIC   R5,TWLSTLIN                                                      
         LA    R5,1(R5)                                                         
         STC   R5,TWLSTLIN                                                      
         STC   R5,NUNTLINE                                                      
         STC   R5,NUKSUB                                                        
         CLI   NUKSUB,193                                                       
         BL    *+8                                                              
         MVI   TWERROR,SUBERR                                                   
         B     OVXIT2                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
* CASH PERCENTAGE ROUTINE                                                       
*                                                                               
CALCASHP L     R5,AIOAREA1                                                      
         USING NURECD,R5                                                        
         L     R6,AIOAREA4         R5 COVERS BUY VALUES                         
         USING BUYVALD,R6                                                       
         GOTO1 VHELLO,DMCB,(C'G',=C'UNTFILE '),(X'18',AIOAREA1),0               
         CLI   12(R1),0                                                         
         BNE   OVXIT2                                                           
         L     R3,12(R1)                                                        
         USING NUDTAD,R3                                                        
*                                                                               
         OC    NUDTCASH,NUDTCASH    IS THERE A PCT                              
         BZ    OVXIT2                                                           
*                                                                               
         ZAP   WORK(16),=PL1'0'                                                 
         ICM   R1,15,NUACTUAL       COST                                        
         CVD   R1,WORK+8                                                        
         SR    R1,R1                                                            
         ICM   R1,3,NUDTCASH        PERCENTAGE XX                               
         CVD   R1,DUB                                                           
         DROP  R3                                                               
*                                                                               
         MP    WORK(16),DUB+4(4)    MULT COST BY PERCENTAGE                     
         AP    WORK(16),=PL2'50'    ROUND                                       
         DP    WORK(16),=PL2'100'   ROUND                                       
         CVB   R2,WORK+6                                                        
*                                                                               
         ICM   R1,15,NUACTUAL       CREATE CREDIT AMOUNT                        
         SR    R2,R1                                                            
*                                                                               
*  DELETE CURRENT SPECIAL CHARGE ELEMENT                                        
         MVI   ELCODE,X'03'                                                     
         MVI   WORKAREA,0                                                       
         MVI   WORKAREA+1,C'C'                                                  
         GOTO1 VHELLO,DMCB,(C'D',=C'UNTFILE '),(ELCODE,AIOAREA1),      X        
               (2,WORKAREA)                                                     
*                                                                               
*  BUILD SPECIAL CHARGE ELEMENT                                                 
         XC    WORKAREA,WORKAREA                                                
         LA    R3,WORKAREA                                                      
         USING NUSPRD,R3                                                        
         MVI   NUSPREL,X'03'                                                    
         MVI   NUSPRLEN,NUSPRLN4                                                
         MVI   NUSPRSEQ,0                                                       
         MVI   NUSPRTYP,C'C'                                                    
         MVI   NUSPRCOM,C'C'                                                    
         STCM  R2,15,NUSPRAMT                                                   
         MVC   NUSPRBPC,CLICPRD                                                 
         BAS   RE,PUTUEL                                                        
*                                                                               
         B     OVXIT2                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
* LOCAL WORKAREA FOR TABLES, ETC                                                
LENTWAO  DC    H'14336'                                                         
TEMPSTRO DC    C'TEMPSTR'                                                       
DMREADO  DC    C'DMREAD '                                                       
DMWRITEO DC    C'DMWRT  '                                                       
         EJECT                                                                  
OVXIT2   XMOD1 1                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
*                                                                               
VEDIT    DS    A                                                                
*                                                                               
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
*                                                                               
WORKAREA DS    CL200                                                            
*                                                                               
*     FOLLOWING FIELDS ARE DEVELOPED FROM THE REQUEST SENT                      
*     FROM THE PC.                                                              
*                                                                               
SCRATCH  DS    0CL1                SCRATCH SPACE                                
*--KEY FIELDS                                                                   
TWWSSVR  DS    F                   WSSVR                                        
TWDUMP   DS    CL1                                                              
TWTYPE   DS    CL3                 UPLOAD TYPE                                  
TWKEY    DS    CL18                UNIT ID                                      
TWAGYMED DS    CL1                 AGENCY/MEDIA                                 
TWAGY    DS    CL2                 AGENCY                                       
TWCLI    DS    CL3                 3 BYTE CLIENT                                
TWCLIENT DS    CL2                 CLIENT                                       
TWNET    DS    CL4                 NETWORK                                      
TWPRCD   DS    CL1                 PRODUCT CODE                                 
TWPRINPC DS    CL1                 PROGRAM CODE INPUTTED Y, N                   
TWMKT    DS    CL2                 MARKET NUMBER                                
TWEST    DS    CL1                 ESTIMATE                                     
TWPKG    DS    CL1                 PACKAGE                                      
TWRATE   DS    CL1                 RATE TYPE                                    
TWCVRGE  DS    CL1                 COVERAGE FACTOR                              
TWPSTTYP DS    CL1                 POSTING TYPE                                 
TWTRTYP  DS    CL1                 TRAFFIC MEDIA TYPE                           
TWMEDTYP DS    CL1                 MEDIA TYPE                                   
TWBKTYP  DS    CL1                 BOOK TYPE                                    
TWSUBMED DS    CL1                 SUB MEDIA TYPE                               
TWNTISTA DS    CL4                 NTI STATION                                  
TWHOMES  DS    CL4                 HOMES IMPRESSION                             
TWDEMOS  DS    CL55                DEMO CODES 3 BYTE FORMAT                     
TWDEMVE  DS    CL76                DEMO VALUES EST                              
TWDEMVA  DS    CL76                DEMO VALUES ACT                              
TWPLEN   DS    CL1                 LENGTH                                       
TWPRLNTP DS    CL1                 LENGTH TYPE  M, OR S                         
TWPRACT  DS    CL4                 ACTUAL COST                                  
TWPRINT  DS    CL4                 INTEGRATION COST                             
TWPRASS  DS    CL4                 ASSIGNED COST                                
TWEDF    DS    CL1                 EST DEMO FORM                                
TWADF    DS    CL1                 ACT DEMO FORM                                
TWACT    DS    CL1                 ACTION 'C' MEANS CONTINUATION                
TWPRMRCD DS    CL1                 MIRROR CODE A-E                              
TWPRTRAT DS    CL1                 TIME RATE                                    
TWPRIRAT DS    CL1                 INTEGRATION RATE                             
TWLSTLIN DS    CL1                 HIGHEST UNIT LINE NUMBER                     
TWUDATE  DS    CL6                 UNIT DATE                                    
TWPKSTAT DS    CL1                 PACKAGE STATUS                               
TWSTAT   DS    CL1                 UNIT STATUS                                  
TWHUTST  DS    CL1                 HUT STATUS BIT SETTINGS                      
TWPSTBIT DS    CL1                 BIT SETTINGS FOR POSTING TYPE                
TWUNST3  DS    CL1                 UNIT STATUS 3                                
TWSPSEQ  DS    CL1                 SPECIAL CHARGE SEQUENCE NUMBER               
TWSPDEL  DS    CL1                 IF NO DELETE SPECIAL CHARGE ELEMENTS         
TWHUTSCH DS    CL1                 HUT SCHEME                                   
TWTIMCH  DS    CL1                 IF YES TIME CHANGED                          
TWOLTIM  DS    CL1                 QUARTER HOUR ON OLD RECORD                   
TWSVDATE DS    CL6                 SAVE WEEK OF DATE                            
TWLINHLD DS    CL1                 LINE HOLD AREA                               
TWPRGHLD DS    CL6                 PROGRAM CODE HOLD AREA                       
*                                                                               
TWCMMNT1 DS    CL60                1ST COMMENT                                  
TWCMMNT2 DS    CL60                2ND COMMENT                                  
*                                                                               
TWPKEY   DS    CL20                PACKAGE KEY                                  
TWRITESW DS    CL1                 WRITE TO PACKAGE SWITCH                      
*                                                                               
TWDYPT   DS    CL1                 DAYPART                                      
TWUNIV   DS    CL2                 UNIVERSE CODE                                
TWUNIVL  DS    CL4                 UNIVERSE CODE (LITERAL)                      
TWSREP   DS    CL2                 SPECIAL REP                                  
TWSREPL  DS    CL3                 SPECIAL REP (LITERAL)                        
TWMSTPRD DS    CL1                 MASTER PRODUCT CODE                          
TWMSTPRA DS    CL3                 MASTER PRODUCT CODE ALPHA                    
TWBUYTYP DS    CL1                 BUY TYPE (U,S,O)                             
TWHPPCT  DS    CL5                 HP PERCENTAGE                                
TWPCNTRL DS    CL1                 PACKAGE CONTROL BYTE                         
TWPINTG  DS    XL4                 PACKAGE INTEGRATION COST                     
TWVTYPE  DS    XL2                 PACKAGE V TYPE                               
TWTRADP  DS    XL2                 PACKAGE TRADE PCTG                           
TWCASHP  DS    XL2                 PACKAGE CASH PCTG                            
*                                                                               
TWHUTTYP DS    CL1                 HUT TYPE                                     
TWFILTER DS    CL11                FILTER                                       
TWSTATUS DS    CL1                 PACKAGE STATUS                               
*                                                                               
TWPRCODE DS    CL6                 PROGRAM CODE                                 
TWPRINNM DS    CL1                 PROGRAM CODE INPUTTED Y, N                   
TWPROTE  DS    CL7                 PROGRAM EXPANDED ROTATION                    
TWPRROT  DS    CL1                 PROGRAM ROTATION                             
TWPRROTN DS    CL1                 4 BIT START END ROTATION NUMBERS             
TWDAYNO  DS    CL1                 DAY NUMBER (FOR X'94' KEY)                   
TWDAYHEX DS    CL1                 DAY NUMBER (HEX)                             
TWDAYLIT DS    CL3                 DAY LITERAL                                  
TWBONUS  DS    CL1                 BONUS UNIT C'B' OR BLANK                     
TWPRSTIM DS    H                   PROGRAM START TIME                           
TWPRETIM DS    H                   PROGRAM END TIME                             
TWPRMIR  DS    H                   MIRROR  TIME                                 
TWPRNAME DS    CL16                PROGRAM NAME                                 
TWQTRHR  DS    CL1                 START QUARTER HOUR                           
TWPDATE  DS    CL2                 DATE FOR UNIT RECORDS                        
TCOMCODE DS    CL8                 COMMERCIAL CODE                              
TCOMPOST DS    CL3                 COMMERCIAL POSITION                          
*                                                                               
TWLNCT   DS    CL1                 START OF LINE COUNT                          
TWUNTCT  DS    CL1                 NUMBER OF UNITS TO BE ADDED                  
TWBLBCT  DS    CL1                 NUMBER OF BILLBOARDS                         
TWDSKADR DS    CL4                 DISK ADDRESS OF LAST ADD                     
*                                                                               
TWERROR  DS    XL1                 ERROR NUMBER                                 
*--CLIENT SAVE AREA                                                             
TWCLIPRD DS    255XL4                                                           
*--ESTIMATE SAVE AREA                                                           
TWESSTRT DS    CL6                 START DATE                                   
TWESEND  DS    CL6                 END DATE                                     
TWESNDEM DS    X                   NUMBER OF DEMOS                              
TWESDEMS DS    XL64                DEMOS                                        
*                                                                               
WRITESW  DS    CL1                 Y=UNIT WAS WRITTEN TO FILE                   
*                                                                               
NFLDS    DS    C                   NUMBER OF DEMOS ON PACKAGE HEADER            
*                                                                               
UPACURUN DS    F                   ADDRESS OF CURRENT UNIT RECORD               
*                                                                               
TWDEAL   DS    CL10                DEAL #                                       
TWCON    DS    CL10                CONTRACT #                                   
TWSER    DS    CL12                SERIAL #                                     
*                                                                               
SVINTTBL DS    CL70                                                             
WSSVRLEN EQU   *-TWTYPE                                                         
*                                                                               
LSCRATCH EQU   *-SCRATCH                                                        
UPACUROB DS    F                   ADDRESS OF CURRENT OBJECT                    
UPCURTWA DS    XL1                 CURRENT TWA                                  
*                                                                               
REQHDR   DS    CL26                                                             
REQUEST  DS    CL80                                                             
*                                                                               
BLOCK    DS    CL256                                                            
         SPACE                                                                  
*                                                                               
MYWORKLE EQU   *-WORKD                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE NEBUYWRK                                                       
       ++INCLUDE NEUPLOADD                                                      
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE NEGETNUND                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
WSSVRDD  DSECT                                                                  
       ++INCLUDE FAWSSVRD                                                       
         EJECT                                                                  
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072NEBUY40   03/03/20'                                      
         END                                                                    
