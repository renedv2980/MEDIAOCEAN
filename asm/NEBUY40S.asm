*          DATA SET NEBUY40S   AT LEVEL 012 AS OF 05/01/02                      
*          DATA SET NEBUY40    AT LEVEL 050 AS OF 03/10/00                      
*PHASE T31140A,+0                                                               
*INCLUDE RECUP                                                                  
*INCLUDE CLPACK                                                                 
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
MAIN10   BAS   RE,PROCESS          READ TAPE WRITE RECORDS                      
         SPACE 2                                                                
***********************************************************************         
* END OF PROCESSING                                                   *         
***********************************************************************         
MAIN20   XC    WORKAREA,WORKAREA                                                
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
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB                                                       
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
         BE    *+14                                                             
         CLC   14(4,RE),=CL4'UNIT'                                              
         BNE   *+8                                                              
         LA    RE,2(RE)                                                         
         ST    RE,UPACUROB                                                      
         LR    R4,RE                                                            
*                                                                               
         CLC   0(2,R4),=X'FFFF'    TEST END OF TWA                              
         BNE   NEXT2                                                            
         BAS   RE,WRTTWA           YES-WRITE CURRENT TWA                        
         ZIC   R1,UPCURTWA                                                      
         LA    R1,1(R1)                                                         
         STC   R1,UPCURTWA                                                      
         BAS   RE,READTWA          GET NEXT TEMPSTR PAGE                        
         L     R4,UPACUROB                                                      
*                                                                               
NEXT2    B     XIT                                                              
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
         CLC   12(4,R4),=CL4'HDR*'                                              
         BNE   PROC050                                                          
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
         CLC   12(4,R4),=CL4'EUNT'                                              
         BE    PROC360                                                          
         CLC   12(4,R4),=CL4'EPKG'                                              
         BE    PROC380                                                          
         CLC   12(4,R4),=CL4'EOD*'                                              
         BE    PROC380                                                          
         CLC   12(4,R4),=CL4'EOF*'                                              
         BE    PROC400                                                          
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
PROC360  BAS   RE,DOEUNT           HANDLE END OF UNIT                           
         L     R4,UPACUROB                                                      
         CLI   TWERROR,0                                                        
         BNE   PROC050                                                          
         B     PROC020                                                          
*                                                                               
PROC380  BAS   RE,DOEPKG           HANDLE END OF PACKAGE                        
         L     R4,UPACUROB                                                      
         B     PROC020                                                          
*                                                                               
PROC400  BAS   RE,DOEOF            HANDLE EOF RECORD                            
         L     R4,UPACUROB                                                      
*                                                                               
PROCEX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* HANDLE DEAL RECORD                                                  *         
* R4=ADDRESS OF DEAL RECORD                                                     
***********************************************************************         
DODEAL   NTR1                                                                   
         USING NDLD,R4                                                          
         PRINT GEN                                                              
*                                                                               
DEAL020  MVC   TWERROR,NETERR                                                   
         OC    NDLNET,NDLNET       CHECK NETWORK INPUTTED                       
         BZ    DEAL030                                                          
         MVC   TWCLI,NDLCLT        CLIENT                                       
         MVC   TWNET,NDLNET        NETWORK                                      
         MVC   TWEDF,NDLEDF        ESTIMATE DEMO FORM                           
         MVC   TWADF,NDLADF        ACTUAL DEMO FORM                             
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
DEAL100  BAS   RE,SETDEMO                                                       
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
* THIS ROUTINE:                                                                 
*    VALIDATES AND CONVERTS THE DEMOS INTO 3 BYTE                               
*    FORMAT.  THEN COMPARES THE PACKAGE DEMOS TO THE                            
*    ESTIMATE RECORD DEMOS, AND REMOVES THE DEMOS THAT                          
*    ARE NOT ON BOTH LISTS.                                                     
*                                                                               
SETDEMO  NTR1                                                                   
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
         CLI   0(RE),C'P'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C'V'          CHANGE PEOPLE TO VIEWERS                     
*                                                                               
         LA    R6,6                                                             
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
         BE    XIT                 NO ERROR                                     
         B     STDMX                                                            
*                                                                               
STDMX    MVI   TWERROR,0                                                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
*                                                                               
*--CHECK THAT ALL DEMOS RESIDE IN THE ESTIMATE HEADER                           
DEMEST   NTR1                                                                   
*                                                                               
         MVI   TWERROR,INVERR                                                   
         L     R5,AIOAREA4                                                      
         USING BUYVALD,R5                                                       
         LA    RE,TWDEMOS                                                       
DMEST050 LA    RF,TWESDEMS                                                      
         ZIC   R1,TWESNDEM                                                      
*                                                                               
DMEST100 OC    0(3,RF),0(RF)       END OF EST LIST                              
         BZ    XIT                                                              
         CLC   0(1,RE),0(RF)                                                    
         BNE   DMEST150                                                         
         CLC   2(1,RE),2(RF)                                                    
         BE    DMEST200                                                         
DMEST150 LA    RF,3(RF)                                                         
         BCT   R1,DMEST100                                                      
*                                                                               
DMEST200 LA    RE,3(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   DMEST050                                                         
*                                                                               
         MVI   TWERROR,0                                                        
         B     XIT                                                              
         DROP  R4,R5                                                            
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
         MVC   NPKGERNO+1,TWERROR                                               
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
DOPKG090 CLI   NPKGUNV,X'40'                                                    
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
         MVC   NPKGERNO+1,UNERROR                                               
         MVI   NPKGERF,6                                                        
         B     DOPKG500                                                         
*                                                                               
         MVC   TWUNIV,DUB+5        CONVERTED UNIVERSE NUMBER                    
         MVC   NPAKUNCD,TWUNIV                                                  
*--CALCULATE SPECIAL REP                                                        
DOPKG100 CLI   NPKGREP,X'40'                                                    
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
         MVC   NPKGERNO+1,UNERROR                                               
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
DOPKG300 MVC   NPKGERNO+1,TWERROR                                               
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
         DC    X'FF'                                                            
*                                                                               
NAMETAB  DC    CL8'THE     ',XL1'03'                                            
         DC    X'FF'                                                            
VOWELLST DC    CL5'AEIOU'                                                       
         DC    X'FF'                                                            
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
         MVI   TWLSTLIN,0                                                       
*                                                                               
*                                                                               
*--READ A PROGRAM RECORD SEE IF IT ALREADY EXISTS                               
*                                                                               
         BAS   RE,READPROG         READ HIGH CALL FOR PROGRAM                   
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
         MVI   NPGACT,C'A'                                                      
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
         MVC   TWLSTLIN,1                                                       
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
*SET POSTING TYPE BITS                                                          
         LA    RE,POSTAB                                                        
*                                                                               
         CLC   TWPSTTYP,0(RE)                                                   
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     *-14                                                             
         OC    TWPSTBIT,1(RE)                                                   
***                                                                             
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
         CLI   NUNTADU,C'Y'                                                     
         BNE   *+8                                                              
         OI    NUSDST3,X'02'                                                    
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'02' ELEMENT                            
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RE                                                               
*                                                                               
*--BUILD 21 ELEMENT (FOR TRAFFIC)                                               
DOUNT100 XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUCMLEL,RE                                                       
*                                                                               
         MVC   NUCMLEID(2),=XL2'2134'                                           
*                                                                               
         CLI   NUNTBLAT,C'Y'                                                    
         BNE   *+8                                                              
         OI    NUCMLFLG,X'04'      SET FOR BILLBOARD                            
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'21' ELEMENT                            
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RE                                                               
*--BUILD 35 ELEMENT (FOR VPH)                                                   
         XC    WORKAREA(70),WORKAREA                                            
         LA    RF,WORKAREA                                                      
         USING NUEHD,RF                                                         
*                                                                               
         MVC   NUEHEL(3),=XL3'350902'                                           
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'35' ELEMENT                            
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RF                                                               
*--BUILD 75 ELEMENT (SERIAL NUMBER)                                             
         XC    WORKAREA(70),WORKAREA                                            
         LA    RF,WORKAREA                                                      
         USING NUSQD,RF                                                         
*                                                                               
         MVC   NUSQEL(2),=XL2'7514'                                             
         MVC   NUSQSEQ(12),NUNTSERN                                             
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RF                                                               
*--BUILD 45 ELEMENT                                                             
         XC    WORKAREA(70),WORKAREA                                            
         LA    RF,WORKAREA                                                      
         USING NUAHD,RF                                                         
*                                                                               
         MVC   NUAHEL(2),=XL2'4509'                                             
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'45' ELEMENT                            
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
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
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
         DROP  RE                                                               
*--CREATE UNIVERSE ELEMENT                                                      
*        BAS   RE,BLDUNIV                                                       
         GOTO1 =A(OVFLRTN),DMCB,(7,DUB),(RC),(R9),RR=MYRELO   BLDUNIV           
         MVC   AIOAREA,AIOAREA1                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
*                                                                               
*--BUILD 99 ELEMENT                                                             
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUACTD,RE                                                        
*                                                                               
         MVC   NUACTEL(2),=XL2'990C'                                            
         GOTO1 VDATCON,DMCB,(5,0),(3,NUACTADT)                                  
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD X'99' ELEMENT                            
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
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
POSTAB   DC    C'N',X'00',C'C',X'01',C'S',X'02',C'O',X'03'                      
DAYTAB   DC    XL7'40201008040201'                                              
         EJECT                                                                  
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
         MVC   NUSPREL(2),=XL2'030C'                                            
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
DOSPC180 GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
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
         L     RE,AIOAREA4                                                      
         USING BUYVALD,RE                                                       
*--PRODUCT 1                                                                    
         LA    R3,TWCLIPRD                                                      
         LA    R1,220                                                           
*                                                                               
DOPRD060 CLC   NPRDPRD,0(R3)                                                    
         BE    DOPRD100                                                         
         LA    R3,4(R3)                                                         
         BCT   R1,DOPRD060                                                      
         MVI   NPRDERNO+1,INVERR                                                
         MVI   NPRDERF,1                                                        
         B     DOPRD500                                                         
*                                                                               
*-CHECK PROD1                                                                   
DOPRD100 CLC   NUPRD,3(R3)                                                      
         BE    DOPRD120                                                         
         GOTO1 CKPAID,DMCB,(X'FF',DUB)                                          
         BNZ   DOPRD120                                                         
         MVI   NPRDERF,1                                                        
         MVI   NPRDRNUM+1,107                                                   
         B     DOPRD500                                                         
DOPRD120 MVC   NUPRD,3(R3)         ONE BYTE PRODUCT CODE                        
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
         MVI   NPRDRNUM+1,106                                                   
         B     DOPRD500                                                         
*--PRODUCT 2                                                                    
         CLI   NPRDPPRD,X'40'                                                   
         BNH   DOPRDX                                                           
*                                                                               
         LA    R3,CLILIST                                                       
         LA    R1,220                                                           
*                                                                               
DOPRD160 CLC   NPRDPPRD,0(R3)                                                   
         BE    DOPRD200                                                         
         LA    R3,4(R3)                                                         
         BCT   R1,DOPRD160                                                      
         MVI   NPRDERNO+1,INVERR                                                
         MVI   NPRDERF,2                                                        
         B     DOPRD500                                                         
*                                                                               
*                                                                               
*-CHECK PROD1                                                                   
DOPRD200 CLC   NUPRD2,3(R3)                                                     
         BE    DOPRD220                                                         
         GOTO1 CKPAID,DMCB,(X'FF',DUB)                                          
         BNZ   DOPRD220                                                         
         B     DOPRD500                                                         
DOPRD220 MVC   NUPRD2,3(R3)        ONE BYTE PRODUCT CODE                        
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
         MVI   NPRDRNUM+1,106                                                   
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
         BNH   DOPRDX                                                           
         MVI   NPRDERNO+1,INVERR                                                
         MVI   NPRDERF,NPRDPCSQ                                                 
         B     DOPRD500                                                         
*                                                                               
*--ERROR CODE                                                                   
DOPRD500 BAS   RE,BYUNIT                                                        
         B     XIT                                                              
*                                                                               
DOPRDX   MVI   TWERROR,0                                                        
         B     XIT                                                              
         DROP  R4,R5,RE                                                         
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
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
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
         BAS   RE,GETPROG                                                       
         CLI   TWERROR,0            INPUTTED CODE NOT FOUND                     
         BNE   DOEUN500                                                         
         CLI   TWLSTLIN,X'FF'       PROGRAM NOT FOUND, ADD IT NOW               
         BE    DOEUN080                                                         
         BAS   RE,GETLST                                                        
         CLI   TWLSTLIN,191         NO ROOM UNDER PROGRAM ADD A NEW ONE         
         BNH   DOEUN100                                                         
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
         BAS   RE,WRTREC                                                        
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
         BAS   RE,WRTREC                                                        
         LA    R5,1(R5)                                                         
         STC   R5,TWLSTLIN                                                      
         B     DOEUNX                                                           
*--NON MIRROR ADD                                                               
DOEUN200 BAS   RE,GETPROG                                                       
         CLI   TWERROR,0            INPUTTED PROGRAM CODE NOT FOUND             
         BNE   DOEUN500                                                         
         CLI   TWLSTLIN,X'FF'       MAX REAC0ED FOR THIS CODE                   
         BE    DOEUN260                                                         
         BAS   RE,GETLST                                                        
         CLI   TWLSTLIN,191         NO ROOM UNDER PROGRAM ADD A NEW ONE         
         BNH   DOEUN300                                                         
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
         BAS   RE,WRTREC                                                        
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
GETLST   NTR1                                                                   
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
         L     R4,UPACURUN                                                      
         USING NUNTD,R4                                                         
         MVI   TWLSTLIN,1                                                       
         BAS   RE,BLDKEY84                                                      
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
DOCHNG NTR1                                                                     
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
         BAS   RE,WRTREC           ADD NEW RECORD                               
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
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
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
         MVC   NUOVVAL,0(R2)                                                    
         MVC   NUOVCAT,0(R4)       THE CATEGORY NUMBER                          
         CLI   NUOVCAT,0           CHECK FOR NAD                                
         BE    *+8                                                              
         OI    NUOVFLG,X'80'                                                    
         MVC   NUOVNUM,2(R4)       DEMO NUMBER                                  
*                                                                               
         MVC   AIOAREA,AIOAREA1    ADD OVERRIDE ELEMENT                         
         GOTO1 VHELLO,DMCB,(C'P',=C'UNTFIL  '),AIOAREA1,WORKAREA,0              
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
         CLI   NUOVMOD,C'V'        CHECK FOR VPH                                
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
         MVI   NUOVMOD,C'T'                                                     
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
*--THIS ROUTINE WRITES THE BUY RECORD TO THE FILE.                              
*                                                                               
WRTREC   NTR1                                                                   
*                                                                               
         MVI   TWRITESW,C'Y'       UNIT BEING WRITTEN SWITCH                    
*                                  READ DELETED RECORDS                         
*        BAS   RE,GETLST           GET LAST LINE NUMBER                         
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
WRITEX   B     XIT                 RETURN TO CALLER                             
         DROP  RE                                                               
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
         OI    NPAKSTAT,X'20'      PACKAGE IS LOCKED                            
         GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA1                                   
*                                                                               
PACKUPEX B     XIT                                                              
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
*        MVI   KEY,X'84'                                                        
*        MVC   KEY+1(1),NUKAM                                                   
*        MVC   KEY+2(2),NUKCLT                                                  
*        MVC   KEY+4(4),NUKNET                                                  
*        MVC   KEY+8(6),NUKPROG                                                 
*        MVC   KEY+14(2),NUKDATE                                                
*        MVC   KEY+16(1),NUKEST                                                 
*        MVC   KEY+17(1),NUKSUB                                                 
*        MVC   KEY+18(1),NUKDP                                                  
         OC    KEY+20(1),TWPSTBIT  SET POSTING TYPE IN CONTROL                  
         B     XIT                                                              
         DROP  RE                                                               
         SPACE 2                                                                
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
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOCAL WORKAREA FOR TABLES, ETC                                                
LENTWA   DC    H'14336'                                                         
TEMPSTR  DC    C'TEMPSTR'                                                       
DMREAD   DC    C'DMREAD '                                                       
DMWRITE  DC    C'DMWRT  '                                                       
         DS    0D                                                               
         EJECT                                                                  
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
         LA    RE,CLILIST                                                       
         LA    RF,880                                                           
         LR    R1,RF                                                            
         LA    R0,CLIST            PRODUCT LIST                                 
         MVCL  RE,R0               MOVE IT TO BUY VALUES                        
         LA    RE,TWCLIPRD                                                      
         LA    RF,880                                                           
         LR    R1,RF                                                            
         LA    R0,CLIST            PRODUCT LIST                                 
         MVCL  RE,R0               MOVE IT TO BUY VALUES                        
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
VALEST14 MVC   ESTDEMS,EDEMLST                                                  
         MVC   ESTWLST,EWGTLST                                                  
         MVC   ESTUSNS,EUSRNMS                                                  
         MVC   ESTWNAM,EWGTNM                                                   
         MVC   ESTFILT,EPROF                                                    
         MVC   ESTSREP,EREP                                                     
         MVC   ESTRATE,ERATE                                                    
         MVC   ESTRATEC,ERATECST                                                
         MVC   ESTCOST2,ECOST2                                                  
*                                                                               
         LA    R0,20               MAXIMUM DEMOS                                
         SR    R1,R1               COUNTER                                      
         LA    R2,ESTDEMS                                                       
VALEST15 OC    0(3,R2),0(R2)                                                    
         BZ    *+16                                                             
         LA    R1,1(R1)            INCREMENT DEMO COUNT                         
         LA    R2,3(R2)            NEXT DEMO                                    
         BCT   R0,VALEST15                                                      
         STC   R1,ESTNDEMS                                                      
         STC   R1,TWESNDEM                                                      
         MVC   TWESDEMS,ESTDEMS                                                 
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
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE     TEST FOR ERROR                               
         BNE   OVXIT                                                            
         MVC   TWPKEY,KEY                                                       
         SPACE 1                                                                
         GOTO1 AIO,DMCB,UNT+FILE+GET,AIOAREA2                                   
         L     R4,AIOAREA2                                                      
         SPACE                                                                  
         MVI   TWERROR,PAKLERR                                                  
         TM    NPAKSTAT,X'20'      CHECK IF LOCKED                              
         BNZ   OVXIT                                                            
         MVI   TWERROR,INVERR                                                   
         TM    NPAKCNTL,X'20'      CHECK IF UNITS UNDER PACKAGE                 
         BZ    OVXIT               YES ERROR                                    
         MVC   TWPKSTAT,NPAKSTAT                                                
         OI    TWPKSTAT,X'20'      SAVE PACKAGE STATUS                          
         MVC   TWDYPT,NPAKDP       DAYPART                                      
         MVC   TWSREP,NPAKSREP     SPECIAL REP                                  
         MVC   TWUNIV,NPAKUNCD     UNIVERSE                                     
         MVC   TWPCNTRL,NPAKCNTL   CONTROL                                      
         MVC   TWMSTPRD,NPAKMAST   MASTER PRODUCT                               
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
         MVC   TWMEDTYP,STYPE                                                   
         PACK  DUB,SMKT(4)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,TWMKT                                                       
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
         DROP  R5                                                               
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
         LA    R3,FASIN-FACTSD(,RE)                                             
         LA    R4,REQUEST+5                                                     
         SPACE                                                                  
         EDIT  (B4,0(R3)),(6,(R4)),FILL=0                                       
         MVC   REQUEST+12(31),=CL31'0207CBRECAP 0303REP 0506OV,T/A '            
         MVC   REQUEST+43(10),=CL10'0606DIRECT'                                 
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(8,DUB)                                       
         OC    DUB+7,X'40'                                                      
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
OVXIT    XMOD1 1                                                                
         SPACE 3                                                                
         LTORG                                                                  
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
TWMEDTYP DS    CL1                 MEDIA TYPE                                   
TWHOMES  DS    CL4                 HOMES IMPRESSION                             
TWDEMOS  DS    CL55                DEMO CODES 3 BYTE FORMAT                     
TWDEMVE  DS    CL76                DEMO VALUES EST                              
TWDEMVA  DS    CL76                DEMO VALUES ACT                              
TWPLEN   DS    CL1                 LENGTH                                       
TWPRLNTP DS    CL1                 LENGTH TYPE  M, OR S                         
TWPRACT  DS    CL4                 ACTUAL COST                                  
TWPRINT  DS    CL4                 INTEGRATION COST                             
TWPRASS  DS    CL4                 INTEGRATION COST                             
TWEDF    DS    CL1                 EST DEMO FORM                                
TWADF    DS    CL1                 ACT DEMO FORM                                
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
TWPKEY   DS    CL20                PACKAGE KEY                                  
TWRITESW DS    CL1                 WRITE TO PACKAGE SWITCH                      
*                                                                               
TWDYPT   DS    CL1                 DAYPART                                      
TWUNIV   DS    CL2                 UNIVERSE CODE                                
TWSREP   DS    CL2                 SPECIAL REP                                  
TWMSTPRD DS    CL1                 MASTER PRODUCT CODE                          
TWPCNTRL DS    CL1                 PACKAGE CONTROL BYTE                         
*                                                                               
TWHUTTYP DS    CL1                 HUT TYPE                                     
TWFILTER DS    CL11                FILTER                                       
TWSTATUS DS    CL1                 PACKAGE STATUS                               
*                                                                               
TWPRCODE DS    CL6                 PROGRAM CODE                                 
TWPRINNM DS    CL1                 PROGRAM CODE INPUTTED Y, N                   
TWPRROT  DS    CL1                 PROGRAM ROTATION                             
TWPROTE  DS    CL7                 PROGRAM EXPANDED ROTATION                    
TWPRROTN DS    CL1                 4 BIT START END ROTATION NUMBERS             
TWDAYNO  DS    CL1                 DAY NUMBER (FOR X'94' KEY)                   
TWDAYHEX DS    CL1                 DAY NUMBER (HEX)                             
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
TWCLIPRD DS    220XL4                                                           
*--ESTIMATE SAVE AREA                                                           
TWESSTRT DS    CL6                 START DATE                                   
TWESEND  DS    CL6                 END DATE                                     
TWESNDEM DS    X                   NUMBER OF DEMOS                              
TWESDEMS DS    XL60                DEMOS                                        
*                                                                               
WRITESW  DS    CL1                 Y=UNIT WAS WRITTEN TO FILE                   
*                                                                               
NFLDS    DS    C                   NUMBER OF DEMOS ON PACKAGE HEADER            
*                                                                               
UPACURUN DS    F                   ADDRESS OF CURRENT UNIT RECORD               
*                                                                               
LSCRATCH EQU   *-SCRATCH                                                        
UPACUROB DS    F                   ADDRESS OF CURRENT OBJECT                    
UPCURTWA DS    XL1                 CURRENT TWA                                  
*                                                                               
REQHDR   DS    CL26                                                             
REQUEST  DS    CL80                                                             
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
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE IUNRECDS                                                       
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
         EJECT                                                                  
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEBUY40S  05/01/02'                                      
         END                                                                    
