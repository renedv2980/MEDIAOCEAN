*          DATA SET NESFM05S   AT LEVEL 154 AS OF 05/01/02                      
*PHASE T31C05A                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE XSORT                                                                  
T31C05   TITLE 'NESFM06 - MARKET '                                              
***********************************************************************         
*  MARKET PROGRAM NEEDS TO PRODUCE A TURNAROUND FOR THE REQUEST RECORDS         
*  THAT WAS IN THE SPLFM24 PROGRAM                                              
***********************************************************************         
T31C05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C05,R7,RR=R8                                                
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         B     MAIN15                                                           
RELO     DS    A                                                                
*                                                                               
MAIN15   BAS   RE,SETUP                                                         
         CLI   MODE,SETFILE        SET FILE NAME                                
         BE    SF                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
DLCHK    CLI   MODE,RECDEL         DELETE RECORDS                               
         BNE   LRCHK                                                            
         BAS   RE,DL                                                            
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         B     XIT                                                              
**************************************                                          
* SET FILE                                                                      
SF       DS    0H                                                               
         OI    GENSTAT4,NODELLST                                                
         BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF           SET FILENAME & OTHER DEFAULTS                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
DL       NTR1                                                                   
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
********************************************************************            
* VALIDATE KEY                                                                  
********************************************************************            
VK       XC    SVQMED,SVQMED                                                    
*                                                                               
         LA    R2,MRTMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 VALIMED                                                          
         MVC   SVQMED,QMED                                                      
*                                                                               
         LA    R2,MRTMKTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK30                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+14                                                             
         MVC   SVQMKT,ZEROES                                                    
         B     VK50                                                             
         B     MISSERR                                                          
*                                                                               
VK30     TM    4(R2),X'08'                                                      
         BZ    INVERR                                                           
*                                                                               
* =====  FORMAT MARKET TO BE 4 CHARACTER FORM ====== *                          
         GOTO1 ANY                                                              
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),WORK(0)                                                   
         NI    DUB+7,X'F0'                                                      
         OI    DUB+7,X'0C'                                                      
         CVB   RE,DUB                                                           
         XC    DUB,DUB                                                          
         STCM  RE,15,DUB                                                        
         EDIT  (B4,DUB),(4,SVQMKT),FILL=0                                       
*                                                                               
*   BUILD THE KEY                                                               
VK50     LA    R4,KEY                                                           
         USING MKTREC,R4                                                        
         XC    KEY,KEY                                                          
         MVC   MKTKEY,ZEROES                                                    
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,SVQMKT                                                   
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF                                                        
*                                                                               
VKX      B     XIT                                                              
         DROP  R4                                                               
***************************************************************                 
*================ VAL REC ====================================*                 
*                                                                               
VR       LA    R2,MRTMKTNH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
         L     R6,AIO                                                           
         USING MKTREC,R6                                                        
*                                                                               
         MVC   MKTNAME,8(R2)                                                    
*                                                                               
         LA    R2,MRTTZONH                                                      
         MVI   MKTZONE,0                                                        
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         MVC   MKTZONE,8(R2)                                                    
*                                                                               
VR20     XC    MKTRANK,MKTRANK                                                  
         LA    R2,MRTRANKH                                                      
         CLI   5(R2),0                                                          
         BNE   VR40                                                             
         CLC   =C'BO',AGENCY        REQUIRED FOR BO                             
         BNE   VR50                                                             
         B     MISSERR                                                          
*                                                                               
* =====  FORMAT RANK TO BE 3 BYTE NUMERIC FORM === *                            
VR40     TM    4(R2),X'08'                                                      
         BZ    NUMERR                                                           
         GOTO1 ANY                                                              
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),WORK(0)                                                   
         NI    DUB+7,X'F0'                                                      
         OI    DUB+7,X'0C'                                                      
         CVB   RE,DUB                                                           
         XC    DUB,DUB                                                          
         STCM  RE,15,DUB                                                        
         EDIT  (B3,DUB+1),(3,MKTRANK),FILL=0                                    
*  MARKET HOMES                                                                 
VR50     XC     MKTHOMES,MKTHOMES                                               
         LA     R2,MRTHOMSH                                                     
         CLI    5(R2),0                                                         
         BE     VR60                                                            
*                                                                               
         TM     4(R2),X'08'                                                     
         BZ     NUMERR                                                          
         GOTO1  ANY                                                             
         ZIC    RE,5(R2)                                                        
         BCTR   RE,0                                                            
         EX     RE,*+8                                                          
         B      *+10                                                            
         PACK   DUB(8),WORK(0)                                                  
         NI     DUB+7,X'F0'                                                     
         OI     DUB+7,X'0C'                                                     
         CVB    RE,DUB                                                          
         XC     DUB,DUB                                                         
         STCM   RE,15,DUB                                                       
         EDIT   (B4,DUB),(8,MKTHOMES),FILL=0                                    
*  NTA                                                                          
VR60     LA     R2,MRTNTAH                                                      
         XC     MKTNTA,MKTNTA                                                   
         CLI    5(R2),0                                                         
         BNE    VR70                                                            
         CLC    =C'BO',AGENCY                                                   
         BNE    VR80                                                            
         B      MISSERR                                                         
*                                                                               
VR70     TM     4(R2),X'08'                                                     
         BZ     NUMERR                                                          
         GOTO1  ANY                                                             
         ZIC    RE,5(R2)                                                        
         BCTR   RE,0                                                            
         EX     RE,*+8                                                          
         B      *+10                                                            
         PACK   DUB(8),WORK(0)                                                  
         NI     DUB+7,X'F0'                                                     
         OI     DUB+7,X'0C'                                                     
         CVB    RE,DUB                                                          
         LA     R0,0                                                            
         CR     RE,R0                                                           
         BE     INVERR                                                          
         LA     R0,29                                                           
         CR     RE,R0                                                           
         BH     INVERR                                                          
         XC     DUB,DUB                                                         
         STCM   RE,15,DUB                                                       
         EDIT   (B4,DUB),(2,MKTNTA),FILL=0                                      
*  MARKET WEIGHT                                                                
VR80     LA     R2,MRTWTH                                                       
         XC     MKTWT,MKTWT                                                     
         CLI    5(R2),0                                                         
         BE     VR90                                                            
         ZIC    R5,5(R2)                                                        
         GOTO1  CASHVAL,DMCB,(2,MRTWT),(R5)                                     
         CLI    DMCB,X'FF'                                                      
         BE     WTSHRERR                                                        
         ZICM   R5,DMCB+4,4                                                     
         ZICM   RE,=X'270F',2    9999                                           
         CR     R5,RE                                                           
         BH     WTSHRERR                                                        
         XC     DUB,DUB                                                         
         STCM   R5,15,DUB                                                       
         EDIT   (B4,DUB),(4,MKTWT),FILL=0                                       
*                                                                               
VR90     LA     R2,MRTSHRH                                                      
         XC     MKTSHR,MKTSHR                                                   
         CLI    5(R2),0                                                         
         BE     VR100                                                           
         ZIC    R5,5(R2)                                                        
         GOTO1  CASHVAL,DMCB,(2,MRTSHR),(R5)                                    
         CLI    DMCB,X'FF'                                                      
         BE     WTSHRERR                                                        
         ZICM   R5,DMCB+4,4                                                     
         ZICM   RE,=X'270F',2    9999                                           
         CR     R5,RE                                                           
         BH     WTSHRERR                                                        
         XC     DUB,DUB                                                         
         STCM   R5,15,DUB                                                       
         EDIT   (B4,DUB),(4,MKTSHR),FILL=0                                      
*                                                                               
VR100    XC    MKTRS1(3),MKTRS1    CLEAR RTG SVC AND MKT                        
         MVI   MKTCLAS1,0          AND SWEEP CLASS                              
         MVI   MKTRS1,C'0'         FORCE RTG SVC = NSI                          
*                                                                               
         LA    R2,MRTRSM1H         RATING SERVICE MARKET                        
         CLI   5(R2),0             TEST DATA                                    
         BE    VR120               NO                                           
*                                                                               
         LA    R2,MRTRSM1H         EDIT MARKET NUMBER                           
         GOTO1  ANY                                                             
         ZIC    RE,5(R2)                                                        
         BCTR   RE,0                                                            
         EX     RE,*+8                                                          
         B      *+10                                                            
         PACK   DUB(8),WORK(0)                                                  
         NI     DUB+7,X'F0'                                                     
         OI     DUB+7,X'0C'                                                     
         CVB    RE,DUB                                                          
         STCM   RE,15,DUB                                                       
         MVC    DUB+2(L'MKTRSM1),MKTRSM1                                        
*                                                                               
VR120    LA    R2,MRTSCL1H         EDIT SWEEP CLASS                             
         CLI   5(R2),0             TEST DATA                                    
         BE    VR140                                                            
         MVC   MKTCLAS1,8(R2)                                                   
*                                                                               
VR140    XC    MKTRS2(3),MKTRS2    CLEAR RTG SVC AND MKT                        
         MVI   MKTCLAS2,0          AND SWEEP CLASS                              
         MVI   MKTRS2,C'1'         FORCE RTG SVC = ARB/BBM                      
*                                                                               
         LA    R2,MRTRSM2H         RATING SERVICE MARKET                        
         CLI   5(R2),0             TEST DATA                                    
         BE    VR160               NO                                           
*                                                                               
         LA    R2,MRTRSM2H         EDIT MARKET NUMBER                           
         GOTO1  ANY                                                             
         ZIC    RE,5(R2)                                                        
         BCTR   RE,0                                                            
         EX     RE,*+8                                                          
         B      *+10                                                            
         PACK   DUB(8),WORK(0)                                                  
         NI     DUB+7,X'F0'                                                     
         OI     DUB+7,X'0C'                                                     
         CVB    RE,DUB                                                          
         STCM   RE,15,DUB                                                       
         MVC    DUB+2(L'MKTRSM2),MKTRSM2                                        
*                                                                               
VR160    LA    R2,MRTSCL2H         EDIT SWEEP CLASS                             
         CLI   5(R2),0             TEST DATA                                    
         BE    VR180                                                            
         MVC   MKTCLAS2,8(R2)                                                   
*   LIMITED ACCESS CODE                                                         
VR180    XC     MKTLTACC,MKTLTACC                                               
         LA     R2,MRTLOCKH                                                     
         CLI    5(R2),0                                                         
         BE     VR200                                                           
         CLI    5(R2),3                                                         
         BH     INVERR                                                          
         MVC    MKTLTACC,8(R2)                                                  
         LA     R4,MKTLTACC                                                     
         LA     R5,3                                                            
VR190    CLI    0(R4),0                                                         
         BE     VR195                                                           
         CLI    0(R4),C'A'                                                      
         BL     INVERR                                                          
         CLI    0(R4),C'9'                                                      
         BH     INVERR                                                          
VR195    LA     R4,1(R4)                                                        
         BCT    R5,VR190                                                        
*                                                                               
*   DAYLIGHT SAVINGS                                                            
VR200    MVI    MKTNODST,0                                                      
         LA     R2,MRTDSTH                                                      
         CLI    5(R2),0                                                         
         BE     VR220                                                           
         MVC    MKTNODST,8(R2)                                                  
         CLI    MKTNODST,C'Y'                                                   
         BNE    NODAYERR                                                        
*    MARKET LIST                                                                
VR220    XC     MKTALST,MKTALST                                                 
         LA     R2,MRTMKTSH                                                     
         CLI    5(R2),0                                                         
         BE     VRX                                                             
         BAS    RE,VALMRKT                                                      
         BZ     INVERR                                                          
*                                                                               
VRX      MVC    MKTRECL,=H'144'                                                 
         B      DR                                                              
***************************************************************                 
***********************************************************                     
*      DISPLAY REC                                                              
***********************************************************                     
DR       L     R6,AIO                                                           
         USING MKTREC,R6                                                        
*  MARKET NAME                                                                  
         LA    R2,MRTMKTNH                                                      
         MVC   MRTMKTN,MKTNAME                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,MRTTZONH                                                      
         MVC   MRTTZON,MKTZONE                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,MRTRANKH                                                      
         MVC   MRTRANK,MKTRANK                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,MRTHOMSH                                                      
         MVC   MRTHOMS,MKTHOMES                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,MRTNTAH                                                       
         MVC   MRTNTA,MKTNTA                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    MRTWT,MRTWT                                                      
         OC    MKTWT,MKTWT                                                      
         BZ    DR30                                                             
         CLC   MKTWT,=C'0000'                                                   
         BE    DR30                                                             
         EDIT  (C4,MKTWT),(5,MRTWT),2,ALIGN=LEFT                                
*                                                                               
DR30     OI    MRTWTH+6,X'80'                                                   
*                                                                               
         XC    MRTSHR,MRTSHR                                                    
         OC    MKTSHR,MKTSHR                                                    
         BZ    DR50                                                             
         CLC   MKTSHR,=C'0000'                                                  
         BE    DR50                                                             
         EDIT  (C4,MKTSHR),(5,MRTSHR),2,ALIGN=LEFT                              
*                                                                               
DR50     OI    MRTSHRH+6,X'80'                                                  
*                                                                               
DR60     DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),MKTRS1       MOVE RTG SVC ID                              
         MVC   DUB+1(2),MKTRSM1    MOVE RTG SVC MKT NUM                         
         MVC   DUB+3(1),MKTCLAS1   MOVE RTG SVC SWEEP CLASS                     
         OI    DUB+3,X'F0'                                                      
         MVC   DUB+4(1),MKTRS2                                                  
         MVC   DUB+5(2),MKTRSM2                                                 
         MVC   DUB+7(1),MKTCLAS2                                                
         OI    DUB+7,X'F0'                                                      
*                                                                               
         CLI   DUB,C'0'                                                         
         BNE   DR80                                                             
         MVI   DUB+4,C'1'                                                       
         B     DR160                                                            
*                                                                               
DR80     CLI   DUB,C'1'                                                         
         BNE   DR100                                                            
         MVI   DUB+4,C'0'                                                       
         B     DR160                                                            
*                                                                               
DR100    CLI   DUB+4,C'0'                                                       
         BNE   DR120                                                            
         MVI   DUB,C'1'                                                         
         B     DR160                                                            
*                                                                               
DR120    CLI   DUB+4,C'1'                                                       
         BNE   DR140                                                            
         MVI   DUB,C'0'                                                         
         B     DR160                                                            
*                                                                               
DR140    MVI   DUB,C'0'                                                         
         MVI   DUB+4,C'1'                                                       
*                                                                               
DR160    XC    ELEM(8),ELEM                                                     
*                                                                               
         CLI   USERPROF,C'0'        TEST NSI AGENCY                             
         BE    *+12                                                             
         CLI   USERPROF,C'2'        OR BOTH                                     
         BNE   DR180                                                            
*                                                                               
         MVC   ELEM(4),DUB                                                      
         CLI   DUB,C'0'            TEST NSI DATA                                
         BE    DR180                                                            
         MVC   ELEM(4),DUB+4                                                    
*                                                                               
DR180    CLI   USERPROF,C'1'        TEST ARB AGENCY                             
         BE    *+12                                                             
         CLI   USERPROF,C'2'        OR BOTH                                     
         BNE   DR200                                                            
*                                                                               
         MVC   ELEM+4(4),DUB+4                                                  
         CLI   DUB+4,C'1'                                                       
         BE    DR200                                                            
         MVC   ELEM+4(4),DUB                                                    
*                                                                               
DR200    LA    R2,MRTRSM1H                                                      
         LA    R3,ELEM                                                          
         BAS   RE,FMTRS                                                         
*                                                                               
         LA    R2,MRTRSM2H                                                      
         LA    R3,ELEM+4                                                        
         BAS   RE,FMTRS                                                         
*                                                                               
         LA    R2,MRTLOCKH                                                      
         MVC   MRTLOCK,MKTLTACC                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
         LA    R2,MRTMKTSH                                                      
         BAS   RE,DSPMKT                                                        
*                                                                               
         LA    R2,MRTDSTH                                                       
         MVC   MRTDST,MKTNODST                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    DR220                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'STAKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DR220    BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF                                                        
                                                                                
DRX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
***********************************************************************         
DK       L     R6,AIO                                                           
         USING MKTRECD,R6                                                       
*                                                                               
         XC    MRTMED,MRTMED                                                    
         MVC   MRTMED,MKTKMED                                                   
         OI    MRTMEDH+6,X'80'                                                  
*                                                                               
         XC    MRTMKT,MRTMKT                                                    
         MVC   MRTMKT,MKTKMKT                                                   
         OI    MRTMKTH+6,X'80'                                                  
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,0(R6)                                                    
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
***********************************************************************         
* ================   LIST RECS   ====================================*          
***********************************************************************         
LR       OC    KEY,KEY                                                          
         BNZ   LR10                                                             
*                                                                               
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
LR30     L     R6,AIO                                                           
         USING MKTREC,R6                                                        
         CLC   0(2,R6),KEYSAVE       SAME MEDIA?                                
         BNE   LRX                                                              
         CLC   MKTKAGY,AGENCY       SAME AGENCY?                                
         BNE   LR20                                                             
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTMKT,MKTKMKT                                                  
         MVC   LISTMKTN,MKTNAME                                                 
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR20                                                             
LRX      B     XIT                                                              
         DROP  R6                                                               
* *********************************************************************         
* FMTRS- FORMAT RATING SERVICE MKT/SWEEP CLASSS                                 
* ON ENTRY R2 POINTS TO NSI  OR ARB(BBM) FLDHDR                                 
* R3 POINTS TO 4 BYTE DATA VALUES                                               
* *********************************************************************         
         SPACE 1                                                                
FMTRS    DS    0H                                                               
         XC    8(4,R2),8(R2)       CLEAR MARKET NUMBER                          
         OI    6(R2),X'80'                                                      
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)          GET MKT NUM                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R2),DUB                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO SWEEP CLASS                         
         MVI   8(R2),0             CLEAR IT                                     
         OI    6(R2),X'80'         AND XMT                                      
         MVC   8(1,R2),3(R3)       MOVE SWEEP CLASS                             
         OI    8(R2),X'F0'         GUARANTEE EBCDIC                             
*                                                                               
FMTRSX   BR    RE                                                               
********************************************************************            
*VALMKT- VALIDATE MKT LIST                                                      
********************************************************************            
         SPACE 1                                                                
VALMRKT  NTR1                                                                   
         L     R6,AIO                                                           
         USING MKTRECD,R6                                                       
         XC    TEMP,TEMP           BUILD BASIC ELEMENT                          
         XC    MKTALST,MKTALST     CLEAR PREV VALUES                            
*                                  VALIDATE AN INPUT FIELD                      
VALM2    LA    R2,MRTMKTSH                                                      
         GOTO1 SCANNER,DMCB,(R2),(X'80',SCANTBL)                                
         CLI   4(R1),0                                                          
         BE    VALERR              NO INPUT                                     
         MVC   NLINES,4(R1)        SAVE NUMBER OF -INPUT FIELDS                 
         MVI   FNDX,1                                                           
         LA    R5,SCANTBL          R5=A(SCAN BLOCK ENTRY)                       
*                                                                               
VALM4    CLC   FNDX,NLINES                                                      
         BH    VALOK                                                            
         CLI   0(R5),0             L'FLD                                        
         BE    VALERR                                                           
         CLI   0(R5),3             L'FLD                                        
         BH    VALERR                                                           
*                                  VALIDATE MARKET NUMBERS                      
VALM8    TM    2(R5),X'40'         C'LHS (ALPHA)- MUST BE CHAR                  
         BO    *+8                                                              
         B     ALPMERR                                                          
*                                  ENSURE VALID MARKET                          
         LA    R1,12(R5)           R1=PTS TO MKT CODE                           
         BAS   RE,FNDMKT           FND MKT ON FILE                              
         BNZ   *+8                                                              
         B     MKTINV                                                           
*                                  ENSURE MARKET NOT PREVIOUSLY DEFINED         
VALM9A   LA    RE,MKTALST                                                       
         LA    RF,L'MKTALST(RE)                                                 
VALM10   OC    0(3,RE),0(RE)                                                    
         BZ    VALM12                                                           
         CLC   12(3,R5),0(RE)                                                   
         BNE   *+8                                                              
         B     DUPMERR                                                          
         LA    RE,3(RE)                                                         
         CR    RE,RF                                                            
         BL    VALM10                                                           
*                                                                               
VALM12   MVC   0(3,RE),12(R5)      ADD MARKET TO LIST                           
*                                  BUMP TO NEXT SCAN BLOCK ENTRY                
VALM14   ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R5,32(R5)                                                        
         B     VALM4                                                            
*                                  ADD ELEMENT TO RECORD                        
VALERR   SR   R1,R1                                                             
         B     VALX                                                             
*                                                                               
VALOK    LA    R1,1                CLEAN EXIT                                   
*                                                                               
VALX     LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* *********************************************************************         
* DSPMKT-DISPLAY MARKETS                                                        
* *********************************************************************         
         SPACE 1                                                                
DSPMKT   NTR1                                                                   
         L     R6,AIO1                                                          
         USING MKTREC,R6                                                        
         XC    MRTMKTS,MRTMKTS     CLEAR SCREEN FIELD                           
         OI    MRTMKTSH+6,X'80'    TRANSMIT                                     
         SR    R4,R4               R4=N'ENTRIES IN TABLE                        
         LA    R5,MKTALST          PT TO MKTS IN RECORD                         
*                                                                               
DSPMKT10 CH    R4,=H'9'            MAX 9 MKTS                                   
         BNL   DSPMK15                                                          
         OC    0(3,R5),0(R5)       NON ZERO?                                    
         BZ    DSPMK15                                                          
         CLC   0(3,R5),=C'0000'                                                 
         BE    DSPMK15                                                          
         LA    R4,1(R4)                                                         
         LA    R5,3(R5)                                                         
         B     DSPMKT10                                                         
*                                  FORMAT SCAN BLOCK INTO TWA                   
DSPMK15  DS    0H                                                               
         LTR   R4,R4                                                            
         BZ    DSPMKTX                                                          
*                                                                               
         BCTR  R4,0                DON'T INCLUDE PRIMARY (1ST) ALPHA            
         LTR   R4,R4                MARKET INTO SORT.                           
         BZ    DSPMK20                                                          
*                                                                               
         L     RF,=V(XSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(0,MKTALST+3),(R4),3,3,0  SORT BEFORE DIS-             
*                                    PLAY, LEAVE 1ST ALPHA MKT ALONE.           
DSPMK20  LA    R4,1(R4)            DISPLAY ALL ALPHA MARKETS.                   
         L     RF,=V(SCINKEY)                                                   
         A     RF,RELO                                                          
         LA    R5,MRTMKTSH                                                      
         GOTO1 (RF),DMCB,(1,(R5)),(3,MKTALST),(R4)                              
*                                                                               
DSPMKTX  B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
********************************************************************            
*FNDMKT- LOOK UP MARKET ON FILE                                                 
********************************************************************            
         SPACE 1                                                                
FNDMKT   NTR1                                                                   
         XC    MYKEY,MYKEY                                                      
         LA    R3,MYKEY                                                         
         USING CTDMREC,R3                                                       
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVC   CTDMKMED,SAVEKEY+1                                               
         MVC   CTDMKMKT,0(R1)                                                   
         LA    R4,1                FOUND- CC= NON-ZERO                          
*                                                                               
         L     R6,AIO2                                                          
         MVI   CTDMKSRC,C'A'                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',MYKEY,(R6)                 
         CLC   MYKEY(22),0(R6)                                                  
         BE    FNDMKX                                                           
*                                                                               
FNDMK5   MVI   CTDMKSRC,C'N'       NEILSON?                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',MYKEY,(R6)                 
         CLC   MYKEY(22),0(R6)                                                  
         BE    FNDMKX                                                           
*                                                                               
         SR    R4,R4               NOT FOUND                                    
*                                                                               
FNDMKX   DS    0H                                                               
         LTR   R4,R4                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
******************************************************                          
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DEL ALLOWED                               
         OI    GENSTAT2,DISTHSPG                                                
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
                                                                                
SAVEDEF  DS    0H                  SAVE DEFINITION BEFORE SETDEF                
         MVC   MYSYSDIR,SYSDIR                                                  
         MVC   MYSYSFIL,SYSFIL                                                  
         MVC   MYUSEIO,USEIO                                                    
         MVC   MYACELOP,ACTELOPT                                                
         MVC   MYLKEY,LKEY                                                      
         BR    RE                                                               
*                                                                               
SETDEF   MVC   SYSDIR,=C'STATION '      SET TO READ STATION FILE                
         MVC   SYSFIL,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVI   ACTELOPT,C'N'            NO ACTIVITY ELEMENTS                    
         MVC   LKEY,=H'15'              SET LENGTH OF STATION KEY               
         BR    RE                                                               
*                                                                               
RSTRDEF  DS    0H                  RESTORE DEFINITION AFTER SETDEF              
         MVC   SYSDIR,MYSYSDIR                                                  
         MVC   SYSFIL,MYSYSFIL                                                  
         MVC   USEIO,MYUSEIO                                                    
         MVC   ACTELOPT,MYACELOP                                                
         MVC   LKEY,MYLKEY                                                      
         BR    RE                                                               
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
NODAYERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=C'ENTER (Y) OR LEAVE BLANK'                         
         B     MSGERR                                                           
*                                                                               
WTSHRERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'ENTER VALUE (1 - 99.99) OR LEAVE BLANK'           
         B     MSGERR                                                           
*                                                                               
NUMERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'NUMERIC DATA ONLY IN THIS FIELD'                  
         B     MSGERR                                                           
*                                                                               
ALPMERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'ALPHABETIC DATA ONLY IN THIS FIELD'               
         B     MSGERR                                                           
*                                                                               
MKTINV   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(14),=C'INVALID MARKET'                                   
         B     MSGERR                                                           
*                                                                               
DUPMERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'DUPLICATE MARKET ERROR'                           
         B     MSGERR                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
ADDERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  OC    ERRDISP,ERRDISP     DO I NEED TO OVERRIDE CURSOR POS.            
         BZ    TRAPEND                                                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         MVC   TIOBCURI,ERRDISP+1  DISPLACMENT INTO FIELD                       
*                                                                               
TRAPEND  DS    0H                                                               
         MVI   ERROPT,0            NEVER TO RETURN                              
         GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
         PRINT GEN                                                              
                                                                                
         DS    0F                                                               
ZEROES   DC    20C'0'                                                           
ERRDISP  DS    H                                                                
         LTORG                                                                  
                                                                                
         DROP  R7,RB                                                            
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*        RETURN NETWORK IN WORK+20                                              
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC8D                                                       
         EJECT                                                                  
*        ORG   MRTWORK                                                          
*                                                                               
                                                                                
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
CBLRECD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSLST                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
* CTGENFILE (NEED CTDMREC)                                                      
       ++INCLUDE CTGENFILE                                                      
                                                                                
* SPDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
         SPACE 1                                                                
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
GEND      DSECT                                                                 
          ORG   LISTAR                                                          
          DS    CL3                                                             
LISTMKT   DS    CL4                                                             
          DS    CL5                                                             
LISTMKTN  DS    CL(L'MKTNAME)                                                   
         DS    CL5                                                              
*                                                                               
***********************************************************************         
*===================== NESFM06 (T31C05) SAVE AREA ====================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
*                                                                               
SAVEKEY  DS    CL(L'MKTKEY)                                                     
*                                                                               
SVQMED   DS    CL1                                                              
SVQMKT   DS    CL4                                                              
TEMP     DS    X                                                                
SCANTBL  DS    XL256                                                            
NLINES   DS    X                                                                
FNDX     DS    X                                                                
MYKEY    DS    CL(L'KEY)                                                        
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'154NESFM05S  05/01/02'                                      
         END                                                                    
