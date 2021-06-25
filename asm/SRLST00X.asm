*          DATA SET SRLST00X   AT LEVEL 004 AS OF 05/01/02                      
*PHASE T11900A                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE TIMEOUT                                                                
         TITLE '$LIST - LIST TERMINAL CONNECT DATA'                             
         PRINT NOGEN                                                            
LST      CSECT                                                                  
         NMOD1 WRKX-WRKD,**$LIST**,CLEAR=YES,RR=RE                              
         USING WRKD,RC                                                          
         ST    RE,RELO                                                          
         MVC   SRPARMS(24),0(R1)                                                
         L     R9,SRPARMS          R9 = SYSFAC LIST                             
         USING SYSFACD,R9                                                       
         L     RA,SRPARMS+20       RA = A(TWA)                                  
         USING SRLSTFFD,RA                                                      
         NI    SRVIDH+6,X'BF'      UNSET CURSOR                                 
         L     RF,SRPARMS+12       A(COMFACS)                                   
         USING COMFACSD,RF                                                      
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VGETFACT,CGETFACT                                                
         DROP  RF                                                               
*                                  GET TIME                                     
         TIME  TU                                                               
         ST    R0,TIMENOW1         TIME IN MVS TU'S (1/38400 SEC)               
         SRDL  R0,32                                                            
         D     R0,=F'384'                                                       
         ST    R1,TIMENOW          TIME IN 1/100 SEC                            
         XC    MSG,MSG                                                          
*                                  GET A SYSLST                                 
         GOTO1 VGETFACT,DUB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   VSYSLST,FASYSLST-FACTSD(R1)                                      
*                                                                               
         L     RE,VSSB             EXTRACT SSB DATA                             
         MVI   TIMEUNIT,0          SET 1/100 SEC UNITS                          
         TM    SSBSTAT3-SSBD(RE),SSBMVSTU                                       
         BZ    *+8                                                              
         MVI   TIMEUNIT,1          SET 1/38400 SEC UNITS                        
         MVC   SYSNAME,SSBSYSN4-SSBD(RE)                                        
         EJECT                                                                  
*                                  EDIT PARAMS                                  
         LA    R2,SRVP1H                                                        
EDITP    CLI   5(R2),0                                                          
         BE    NXTFLD                                                           
         BAS   RE,TRYAG                                                         
         BAS   RE,TRYSYS                                                        
         BAS   RE,TRYPROG                                                       
         BAS   RE,TRYTIM                                                        
         BAS   RE,TRYUSER                                                       
*                                                                               
         BAS   RE,TRYTNO                                                        
         BAS   RE,TRYLUID                                                       
         BAS   RE,TRYRPL                                                        
*                                                                               
         BAS   RE,TRYMODE                                                       
         BAS   RE,TRYPRNT                                                       
         B     ERR2                                                             
*                                                                               
NXTFLD   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST PROTECTED                               
         BZ    EDITP                                                            
         B     LIST                                                             
         SPACE 1                                                                
ERR1     MVC   MSG(19),=C'DUPLICATE PARAMETER'                                  
         B     ERRX                                                             
ERR2     MVC   MSG(17),=C'INVALID PARAMETER'                                    
         B     ERRX                                                             
ERR3     MVC   MSG(23),=C'INCOMPATIBLE PARAMETERS'                              
         B     ERRX                                                             
ERR4     MVC   MSG(15),=C'INVALID LINE ID'                                      
         B     ERRX                                                             
*                                                                               
ERRX     MVC   SRVMSG(15),=C'ED/9999 (XXXX) '                                   
         MVC   SRVMSG+09(4),SYSNAME                                             
         MVC   SRVMSG+15(L'SRVMSG-15),MSG                                       
         OI    6(R2),X'40'         CURSOR                                       
         B     EXIT                                                             
         EJECT                                                                  
TRYPROG  CLI   FPROG,0                                                          
         BNER  RE                                                               
         CLI   5(R2),3                                                          
         BLR   RE                                                               
         CLI   FSYSO,0             MUST HAVE SYS                                
         BER   RE                                                               
         L     R3,VPGMLST                                                       
         BAS   R8,SETBXLE                                                       
         USING PGMLSTD,R3                                                       
         SR    R6,R6                                                            
         IC    R6,5(R2)                                                         
         BCTR  R6,R0                                                            
TRYPROG4 EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),PGMNAME                                                  
         BE    TRYPROG6                                                         
         BXLE  R3,R4,TRYPROG4                                                   
         BR    RE                                                               
*                                                                               
TRYPROG6 MVC   FPROG,PGMNUM                                                     
         B     NXTFLD                                                           
         DROP  R3                                                               
         SPACE 1                                                                
TRYLUID  CLI   5(R2),4             LUID IS 4 THRU 8 CHRS                        
         BLR   RE                                                               
         CLI   5(R2),8                                                          
         BHR   RE                                                               
         OC    FLUID,FLUID                                                      
         BNZR  RE                                                               
         OC    FTNO,FTNO                                                        
         BNZ   ERR3                INCOMP WITH TERM NO                          
         MVC   FLUID,8(R2)                                                      
         ST    R2,LINPARAM         SAVE ADDR OF PARAM                           
         B     NXTFLD                                                           
         SPACE 1                                                                
TRYRPL   MVI   RPL,C'N'                                                         
         CLI   5(R2),3                                                          
         BNER  RE                                                               
         CLC   8(3,R2),=C'RPL'                                                  
         BNER  RE                                                               
         GOTO1 VLCWRITE,DMCB,VTGETRPL,0                                         
         MVC   ARPL,4(R1)          GET A(FIRST RPL)                             
         MVI   RPL,C'Y'                                                         
         B     NXTFLD                                                           
         SPACE 1                                                                
TRYSYS   CLI   FSYSO,0             SYSTEM                                       
         BNER  RE                                                               
         CLI   5(R2),3                                                          
         BLR   RE                                                               
         CLI   5(R2),7                                                          
         BHR   RE                                                               
         CLI   5(R2),5                                                          
         BNE   TRYSYS1                                                          
         CLC   =C'PRINT',8(R2)     PRINT=PRNT                                   
         BNE   TRYSYS1                                                          
         MVC   8(4,R2),=C'PRNT'                                                 
         MVC   12(3,R2),13(R2)                                                  
         MVI   15(R2),C' '                                                      
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,R0                                                            
         STC   R0,5(R2)                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
TRYSYS1  L     R3,VSELIST                                                       
         BAS   R8,SETBXLE                                                       
         USING SELISTD,R3                                                       
         SR    R6,R6                                                            
         IC    R6,5(R2)                                                         
         BCTR  R6,R0                                                            
TRYSYS2  EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SENAME                                                   
         BE    TRYSYS3                                                          
         BXLE  R3,R4,TRYSYS2                                                    
         BR    RE                                                               
*                                  SET OVERLAY SYS NO                           
TRYSYS3  MVC   FSYSO,SEOVSYS                                                    
         MVC   VPGMLST,SEPGMS                                                   
*                                  IF INPUT = SYS NAME EXACTLY                  
         OC    8(L'SRVP1,R2),SPACES                                             
         CLC   SENAME,8(R2)                                                     
         BNE   NXTFLD                                                           
*                                  USE FILE SYS NUM ALSO                        
         MVC   FSYSF,SESYS                                                      
         B     NXTFLD                                                           
         DROP  R3                                                               
         SPACE 1                                                                
TRYTIM   SR    R4,R4               TIME                                         
         IC    R4,5(R2)                                                         
         LA    R4,8(R2,R4)                                                      
         BCTR  R4,R0               POINT TO LAST                                
         LA    R5,3600                                                          
         CLI   0(R4),C'H'          HRS                                          
         BE    TRYTIM2                                                          
         LA    R5,60                                                            
         CLI   0(R4),C'M'          MINS                                         
         BE    TRYTIM2                                                          
         LA    R5,1                                                             
         CLI   0(R4),C'S'          SECS                                         
         BE    TRYTIM2                                                          
         CLI   0(R4),C'0'                                                       
         BLR   RE                                                               
         LA    R4,1(R4)                                                         
*                                                                               
TRYTIM2  BCTR  R4,R0               LAST BYTE OF NUMBER                          
         LA    R6,8(R2)                                                         
TRYTIM3  CLI   0(R6),C'0'                                                       
         BLR   RE                                                               
         LA    R6,1(R6)                                                         
         CR    R6,R4                                                            
         BNH   TRYTIM3                                                          
         SR    R4,R2                                                            
         SH    R4,=H'8'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         MR    R0,R5               CONVERT TO SECS                              
         MH    R1,=H'100'                                                       
         OC    FSTIM,FSTIM                                                      
         BNZ   ERR1                                                             
         L     R0,TIMENOW                                                       
         SR    R0,R1                                                            
         BNM   *+6                                                              
         SR    R0,R0                                                            
         ST    R0,FSTIM            START TIME IN 1/100 SEC                      
         SRDL  R0,32                                                            
         M     R0,=F'384'                                                       
         ST    R1,FSTIM1           START TIME IN 1/38400 SEC                    
         B     NXTFLD                                                           
         SPACE 1                                                                
TRYAG    CLI   5(R2),2             MUST BE 2 LONG                               
         BNER  RE                                                               
         CLI   8(R2),C'0'          1ST CHAR NOT NUMERIC                         
         BNLR  RE                                                               
         CLI   8(R2),C'#'          OR NUMBER SIGN                               
         BER   RE                                                               
         OC    FAGY,FAGY                                                        
         BNZ   ERR1                                                             
         MVC   FAGY,8(R2)                                                       
         B     NXTFLD                                                           
         SPACE 1                                                                
TRYTNO   CLI   8(R2),C'#'          NUMBER SIGN                                  
         BNER  RE                                                               
         CLI   5(R2),2                                                          
         BLR   RE                                                               
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,R0                                                            
         BCTR  R4,R0                                                            
         MVC   DUB,=8C'0'                                                       
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),9(R2)                                                     
         CLC   DUB,=8C'0'                                                       
         BNER  RE                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,9(0,R2)                                                      
         CVB   R0,DUB                                                           
         OC    FTNO,FTNO                                                        
         BNZ   ERR1                                                             
         STH   R0,FTNO                                                          
         OC    FLUID,FLUID                                                      
         BNZ   ERR3                INCOMP WITH LINE-ADDRESS                     
         MVI   SORTSW,C'N'         NO SORT                                      
         B     NXTFLD                                                           
         SPACE 1                                                                
TRYUSER  CLI   8(R2),C'='          TEST FOR USER FUNCTION                       
         BNER  RE                                                               
         CLI   5(R2),2             USER FIELD IS =KEYWORD                       
         BL    ERR2                                                             
         CLI   5(R2),9             KEYWORD MUST BE 1-8 CHRS                     
         BH    ERR2                                                             
         LA    RF,USERTAB          TABLE OF KEYWORDS                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
TRYUSER1 EX    R1,*+8              COMPARE FIELD                                
         B     *+10                                                             
         CLC   0(0,RF),9(R2)                                                    
         BE    TRYUSER2                                                         
         LA    RF,12(RF)           TRY  NEXT TABLE ENTRY                        
         CLI   0(RF),0                                                          
         BNE   TRYUSER1                                                         
         B     ERR2                KEYWORD NOT IN TABLE                         
*                                                                               
TRYUSER2 OC    AUSERX,AUSERX       TEST LAST SLOT STILL AVAILABLE               
         BZ    *+6                                                              
         DC    H'0'                HOW DID YOU GET MORE THAN 4 IN               
         LA    R1,AUSER                                                         
         OC    0(4,R1),0(R1)       FIND FIRST EMPTY SLOT                        
         BZ    *+12                                                             
         LA    R1,4(R1)                                                         
         B     *-14                                                             
         L     RF,8(RF)            GET A(ROUTINE)                               
         A     RF,RELO                                                          
         ST    RF,0(R1)            SAVE TRUE ADDRESS                            
         B     NXTFLD                                                           
*                                                                               
TRYMODE  CLI   5(R2),1             1 BYTE ONLY                                  
         BNE   TRYM12                                                           
         CLI   8(R2),C'C'          CONNECTED ONLY                               
         BE    TRYM2                                                            
         CLI   8(R2),C'D'          NOT CONNECTED ONLY                           
         BNE   TRYM4                                                            
         MVI   SORTSW,C'N'                                                      
TRYM2    CLI   FCON,0                                                           
         BNE   ERR1                                                             
         MVC   FCON,8(R2)                                                       
         B     NXTFLD                                                           
*                                                                               
TRYM4    CLI   8(R2),C'P'          IN PROCESS ONLY                              
         BNE   TRYM6                                                            
         MVI   FPROC,C'P'                                                       
         B     NXTFLD                                                           
*                                                                               
TRYM6    CLI   8(R2),C'N'          N = VTAM TERMINAL BUILD FAILED               
         BNE   TRYM8                                                            
         MVI   FVTBLD,C'N'                                                      
         MVI   SORTSW,C'N'         NO SORT                                      
         B     NXTFLD                                                           
*                                                                               
TRYM8    CLI   8(R2),C'*'          ONE ONLY                                     
         BNE   TRYM10                                                           
         MVI   FSTAR,C'*'                                                       
         MVI   SORTSW,C'N'         NO SORT                                      
         B     NXTFLD                                                           
*                                                                               
TRYM10   CLI   8(R2),C'B'          BROADCAST PENDING                            
         BNE   TRYM11                                                           
         MVI   FBROAD,C'B'                                                      
         B     NXTFLD                                                           
*                                                                               
TRYM11   CLI   8(R2),C'E'          STEREO EMULATOR                              
         BNE   *+12                                                             
         OI    STEREO,TST6STRO                                                  
         B     NXTFLD                                                           
         CLI   8(R2),C'F'          STEREO FULL                                  
         BNE   *+12                                                             
         OI    STEREO,TST6STRO+TST6STFU                                         
         B     NXTFLD                                                           
*                                                                               
TRYM12   CLC   8(2,R2),=C'S='      STEREO                                       
         BNE   TRYM12X                                                          
         CLI   10(R2),C'Y'         S=Y                                          
         BNE   *+8                                                              
         OI    STEREO,TST6STRO                                                  
         CLC   10(2,R2),=C'YY'     S=YY                                         
         BNE   *+8                                                              
         OI    STEREO,TST6STRO+TST6STFU                                         
         B     NXTFLD                                                           
TRYM12X  EQU   *                                                                
*                                                                               
TRYM13   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'SUBMITTED'                                            
         BNE   TRYM14                                                           
         OI    FJOBS,TJOBFANY                                                   
         B     NXTFLD                                                           
*                                                                               
TRYM14   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'READY'                                                
         BNE   TRYM16                                                           
         OI    FJOBS,TJOBFOUT                                                   
         B     NXTFLD                                                           
*                                                                               
TRYM16   DS    0H                                                               
         BR    RE                                                               
         SPACE 1                                                                
TRYPRNT  CLI   5(R2),7                                                          
         BNER  RE                                                               
         CLC   8(7,R2),=C'PRINTER'                                              
         BNER  RE                                                               
         MVI   FPRINT,C'P'                                                      
         B     NXTFLD                                                           
         EJECT                                                                  
LIST     LA    R0,SRVL1AH                                                       
         ST    R0,ANXTLIN          FIRST OUTPUT LINE                            
         ZAP   ACTRMS,=P'0'                                                     
         ZAP   NTRMS,=P'0'                                                      
         CLI   RPL,C'Y'                                                         
         BNE   LIST1                                                            
*                                                                               
         L     R7,ARPL             SCAN RPL LIST                                
         USING FARPLD,R7                                                        
RPL01    TM    FARPLFLG,FARPLBSY   LOOKING FOR A BUSY ONE                       
         BO    RPL02                                                            
RPLNXT   ICM   R7,15,FARPLNXT                                                   
         BZ    FMTTOTS             LAST RPL SO EXIT                             
         B     RPL01                                                            
RPL02    L     R3,FARPLNIB         GET A(NIB)                                   
         MVC   NSYM,NIBSYM-NIBST(R3)                                            
         L     R3,FARPLRPL                                                      
         L     R3,92(R3)           RPLUSFLD                                     
         USING UTLD,R3                                                          
         MVC   NSYM,TSYM           GET LUID FROM UTL                            
         L     R3,VUTL                                                          
         BAS   R8,SETBXLE                                                       
RPL03    CLC   TSYM,NSYM           FIND UTL ENTRY                               
         BNE   *+12                                                             
         BAS   RE,PUTTRM           DISPLAY TERMINAL                             
         B     RPLNXT                                                           
         BXLE  R3,R4,RPL03                                                      
         B     RPLNXT              LUID NOT IN UTL  (THIS IS OK)                
         DROP  R3                                                               
*                                                                               
LIST1    OC    FSTIM,FSTIM                                                      
         BNZ   *+12                                                             
         MVI   FSTIM+3,1           NON-ZERO START TIME                          
         MVI   FSTIM1+3,1                                                       
*                                                                               
         L     R3,VUTL                                                          
         BAS   R8,SETBXLE                                                       
         USING UTLD,R3                                                          
*                                                                               
         CLI   SORTSW,C'N'                                                      
         BE    LIST2                                                            
*                                  SET BSPARS                                   
         LA    R0,BSTAB                                                         
         ST    R0,BSPARS+4                                                      
         ST    R4,BSPARS+12        LENGTH                                       
         LR    R0,R4                                                            
         SH    R0,=Y(TTIME-TNUM)                                                
         ST    R0,BSPARS+16        KEY LEN AND DISPLACEMENT                     
         MVI   BSPARS+16,TTIME-TNUM                                             
         LA    R0,33               MAX                                          
         ST    R0,BSPARS+20                                                     
*                                                                               
LIST2    AP    NTRMS,=P'1'                                                      
         CLI   FLUID,0             VTAM LUID                                    
         BE    LIST2D                                                           
         CLI   FLUID,X'FF'         START LUID ALREADY REACHED                   
         BE    LIST2D                                                           
         LA    R6,TSYM             POINT TO LUID IN UTL                         
         LA    RE,FLUID                                                         
         LA    RF,8                                                             
LIST2C1  CLI   0(RE),C' '                                                       
         BE    LIST2C2                                                          
         CLI   0(RE),C'*'                                                       
         BE    LIST2C2                                                          
         CLI   0(RE),X'00'                                                      
         BE    LIST2C2                                                          
         CLC   0(1,RE),0(R6)                                                    
         BNE   LISTNXT                                                          
LIST2C2  LA    R6,1(R6)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,LIST2C1                                                       
         MVI   HAVLUID,C'Y'                                                     
*                                                                               
LIST2D   CLI   FAGY,0              AGENCY CODE                                  
         BE    LIST3                                                            
         CLC   TAGY,FAGY                                                        
         BNE   LISTNXT                                                          
*                                                                               
LIST3    CLI   FSYSF,0             SYS NO                                       
         BE    LIST4                                                            
         CLC   TSYS,FSYSF                                                       
         BNE   LISTNXT                                                          
*                                                                               
LIST4    CLI   FSYSO,0             OVERLAY SYS                                  
         BE    LIST5                                                            
         CLC   TOVSYS,FSYSO                                                     
         BNE   LISTNXT                                                          
*                                                                               
LIST5    CLI   FPROG,0             PROGRAM                                      
         BE    LIST7                                                            
         CLC   TPRG,FPROG                                                       
         BNE   LISTNXT                                                          
*                                                                               
LIST7    OC    FTNO,FTNO           START TERM NUM                               
         BE    LIST8                                                            
         CLC   TNUM,FTNO                                                        
         BL    LISTNXT                                                          
*                                                                               
LIST8    CLI   FPROC,C'P'          IN PROCESS                                   
         BNE   LIST9                                                            
         TM    TSTAT+1,X'20'                                                    
         BZ    LISTNXT                                                          
*                                                                               
LIST9    CLI   FCON,C'C'           CONNECTED                                    
         BNE   LIST10                                                           
         CLI   TAGY,0                                                           
         BE    LISTNXT                                                          
*                                                                               
LIST10   CLI   FCON,C'D'           NOT CONNECTED                                
         BNE   LIST11                                                           
         CLI   TAGY,0                                                           
         BNE   LISTNXT                                                          
*                                                                               
LIST11   CLI   FVTBLD,C'N'         VTAM TERMINAL BUILD FAILED                   
         BNE   LIST12                                                           
         TM    TSTAT5,TST5TBF                                                   
         BZ    LISTNXT                                                          
*                                                                               
LIST12   CLI   FBROAD,C'B'         BROADCAST PENDING                            
         BNE   LIST13                                                           
         TM    TSTAT+1,X'08'                                                    
         BZ    LISTNXT                                                          
*                                                                               
LIST13   ICM   RF,1,STEREO         STEREO                                       
         BZ    LIST14                                                           
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TSTAT6,0                                                         
         BNO   LISTNXT                                                          
*                                                                               
LIST14   ICM   RF,1,FJOBS          JOB SUBMIT/READY                             
         BZ    LIST30                                                           
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TJOBFLAG,0                                                       
         BNO   LISTNXT                                                          
         OC    TPRNT,TPRNT         TEST PRINTER                                 
         BNZ   LISTNXT                                                          
*                                                                               
LIST30   CLC   TIMEUNIT,0          TIME                                         
         BNE   *+14                                                             
         CLC   TTIMETU,FSTIM1                                                   
         B     *+10                                                             
         CLC   TTIMETU,FSTIM                                                    
         BNL   LIST31                                                           
         CLI   FVTBLD,C'N'                                                      
         BE    LIST31                                                           
         CLI   FCON,C'D'                                                        
         BE    LIST31                                                           
         B     LISTNXT                                                          
*                                                                               
LIST31   CLI   FPRINT,C'P'                                                      
         BNE   LIST32                                                           
         OC    TPRNT,TPRNT                                                      
         BZ    LISTNXT                                                          
*                                                                               
LIST32   LA    R1,AUSER            USER FILTERS                                 
LIST32A  ICM   RF,15,0(R1)                                                      
         BZ    LIST40                                                           
         BASR  RE,RF               GOTO USER TEST                               
         BZ    LISTNXT             IF CC = ZERO REJECT                          
         LA    R1,4(R1)                                                         
         LA    RF,AUSERX                                                        
         CR    R1,RF                                                            
         BNH   LIST32A             UP TO 4 USER TESTS                           
*                                                                               
LIST40   AP    ACTRMS,=P'1'                                                     
         CLI   SORTSW,C'N'                                                      
         BE    LIST44                                                           
*                                  COMPLIMENT SIN/TIME FOR SEQUENCE             
         MVC   WORK,TNUM                                                        
         LA    RF,WORK+TTIME-TNUM                                               
         XC    0(4,RF),=4X'FF'                                                  
*                                  PUT UTL ENTRY IN BSTAB                       
LIST41   LA    RF,WORK                                                          
         ST    RF,BSPARS                                                        
         MVI   BSPARS,1            SET TO ADD                                   
         GOTO1 =V(BINSRCH),BSPARS,RR=RB                                         
*                                                                               
         OC    BSPARS+1(3),BSPARS+1 TEST TABLE FULL                             
         BNZ   LISTNXT             NO                                           
         LA    R0,32               SHORTEN TABLE                                
         ST    R0,BSPARS+8                                                      
         B     LIST41              TRY TO ADD AGAIN                             
*                                  NO 'SORT'- PUT TO SCREEN NOW                 
LIST44   BAS   RE,PUTTRM                                                        
         B     LISTNXT                                                          
*                                                                               
LISTNXT  BXLE  R3,R4,LIST2                                                      
*                                  DONE WITH UTL                                
         CLI   FLUID,0                                                          
         BE    LIST46                                                           
         CLI   HAVLUID,C'Y'                                                     
         BE    LIST46                                                           
         L     R2,LINPARAM                                                      
         B     ERR4                INVALID LINE ID                              
*                                                                               
LIST46   CLI   SORTSW,C'N'                                                      
         BE    FMTTOTS                                                          
*                                  PUT BSTAB LIST TO SCREEN                     
         L     R3,BSPARS+4         A(TABLE)                                     
         L     R4,BSPARS+8         NUMBER IN TABLE                              
         LTR   R4,R4                                                            
         BNP   FMTTOTS                                                          
LIST50   XC    TTIME,=4X'FF'       RECOMPLIMENT TIME (IN TABLE)                 
         BAS   RE,PUTTRM                                                        
         A     R3,BSPARS+12        LENGTH                                       
         BCT   R4,LIST50                                                        
         B     FMTTOTS                                                          
         EJECT                                                                  
PUTTRM   NTR1                                                                   
         CLI   ANXTLIN,X'FF'       EOS                                          
         BE    PUTX                                                             
         L     R2,ANXTLIN                                                       
         CLI   FSTAR,C'*'          TEST PUT ONLY ONE                            
         BNE   PUT2                                                             
         LA    R0,SRVL1AH                                                       
         CR    R2,R0                                                            
         BH    PUTX                                                             
*                                                                               
         USING TRMLD,R2                                                         
PUT2     LR    R6,R3               USE R6 FOR UTL                               
         DROP  R3                                                               
         USING UTLD,R6             (FREES R3,R4,R5)                             
*                                                                               
         LH    R0,TNUM                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TRMNUM,DUB                                                       
         MVC   TRMLUID,TSYM                                                     
         MVC   TRMAGY,TAGY                                                      
*                                                                               
         CLI   TSYS,0                                                           
         BE    PUT20                                                            
         L     R3,VSELIST                                                       
         BAS   R8,SETBXLE                                                       
         USING SELISTD,R3                                                       
         CLC   TSYS,SESYS                                                       
         BE    PUT6                                                             
         BXLE  R3,R4,*-10                                                       
         DC    H'0'                                                             
*                                                                               
PUT6     MVC   VPGMLST,SEPGMS      SAVE A(PGMLST)                               
         MVC   TRMSYS,SENAME                                                    
         CLI   SEFILSET,0          TEST MULTI-SYSTEM                            
         BE    PUT7                                                             
         MVC   DUB(1),SEOVSYS                                                   
         MVC   DUB+1(1),SEFILSET                                                
         L     R3,VSYSLST                                                       
         BAS   R8,SETBXLE                                                       
         USING SYSLSTD,R3                                                       
         CLC   SYSLNUM,DUB                                                      
         BE    *+10                                                             
         BXLE  R3,R4,*-10                                                       
         DC    H'0'                                                             
         MVC   TRMSYS(L'SYSLSHRT),SYSLSHRT                                      
         ZIC   R8,DUB+1                                                         
         LA    R8,ALPHANUM(R8)                                                  
         MVC   TRMSYS+L'SYSLSHRT(1),0(R8)                                       
         DROP  R3                                                               
PUT7     CLI   TPRG,0                                                           
         BE    PUT20                                                            
         L     R3,VPGMLST                                                       
         BAS   R8,SETBXLE                                                       
         USING PGMLSTD,R3                                                       
*                                                                               
         CLC   TPRG,PGMNUM                                                      
         BE    PUT8                                                             
         BXLE  R3,R4,*-10                                                       
         MVC   TRMPRG,=C'TST'                                                   
         B     *+10                                                             
PUT8     MVC   TRMPRG,PGMNAME                                                   
         DROP  R3                                                               
*                                                                               
PUT20    MVC   FULL,TTIME          TERMINAL TIME                                
         CLI   TIMEUNIT,0                                                       
         BE    PUT20A                                                           
         SR    R0,R0                                                            
         L     R1,TTIME                                                         
         D     R0,=F'384'                                                       
         ST    R1,FULL                                                          
PUT20A   GOTO1 =V(TIMEOUT),DMCB,(1,FULL),(X'45',WORK),RR=RB                     
         MVC   TRMTIM,WORK                                                      
*                                                                               
PUT21    TM    TSTAT6,TST6STRO                                                  
         BZ    PUT22                                                            
         MVI   TRMPRG-1,C'-'       SET STEREO                                   
         TM    TSTAT6,TST6STFU                                                  
         BZ    *+8                                                              
         MVI   TRMPRG-1,C'='       SET FULL STEREO                              
*                                                                               
PUT22    CLI   RPL,C'Y'            ARE WE LISTING RPLS                          
         BNE   *+14                                                             
         MVC   TRMSTAT,FARPLEYE    PUT EYECATCHER INTO STATUS                   
         B     PUT30                                                            
         TM    TSTAT+1,X'20'                                                    
         BZ    *+8                                                              
         MVI   TRMSTAT,C'P'        IN PROC                                      
         TM    TSTAT+1,X'10'                                                    
         BZ    *+8                                                              
         MVI   TRMSTAT,C'N'        VTAM TERMINAL BUILD FAILED                   
         TM    TSTAT+1,X'08'                                                    
         BZ    *+8                                                              
         MVI   TRMSTAT+1,C'B'      BROADCAST PENDING                            
         OC    TPRNT,TPRNT                                                      
         BZ    *+10                                                             
         MVC   TRMSTAT(2),=C'RP'                                                
         TM    TJOBFLAG,TJOBFINQ+TJOBFSUB+TJOBFOUT                              
         BZ    *+8                                                              
         MVI   TRMSTAT+2,C'J'      JOB INFO                                     
         TM    TSTAT8,TST8BINT                                                  
         BZ    *+8                                                              
         MVI   TRMSTAT+2,C'B'      BINARY INTERFACE                             
PUT30    OI    6(R2),X'80'                                                      
*                                  BUMP TO NEXT FIELD                           
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS ON FIRST BUMP = DONE                     
         BNE   PUT31                                                            
         MVI   ANXTLIN,X'FF'                                                    
         B     PUTX                                                             
PUT31    IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS ON 2ND BUMP = DO RIGHT SIDE              
         BNE   PUT32                                                            
         LA    R2,SRVL1BH                                                       
PUT32    ST    R2,ANXTLIN                                                       
         B     PUTX                                                             
PUTX     XIT1                                                                   
         DROP  R6,R2                                                            
         EJECT                                                                  
FMTTOTS  LA    R2,SRVTOT                                                        
         MVC   0(6,R2),=C'TERMS='                                               
         LA    R2,6(R2)                                                         
         EDIT  (P3,NTRMS),(4,0(R2)),ALIGN=LEFT                                  
         AR    R2,R0                                                            
         MVC   0(9,R2),=C', ACTIVE='                                            
         LA    R2,9(R2)                                                         
         EDIT  (P3,ACTRMS),(4,0(R2)),ALIGN=LEFT                                 
         AR    R2,R0                                                            
         L     R3,VSSB                                                          
         USING SSBD,R3                                                          
         MVC   0(6,R2),=C', SIN='                                               
         LA    R2,6(R2)                                                         
         EDIT  (B4,SSBSIN),(7,0(R2)),ALIGN=LEFT                                 
         AR    R2,R0                                                            
         L     R3,VSELIST                                                       
         BAS   R8,SETBXLE                                                       
         USING SELISTD,R3                                                       
         SR    R1,R1                                                            
*                                                                               
         MVC   DUB(2),SEQLEN                                                    
         AH    R1,DUB                                                           
         BXLE  R3,R4,*-10                                                       
         LTR   R1,R1                                                            
         BZ    FMT2                                                             
         MVC   0(7,R2),=C', QLEN='                                              
         LA    R2,7(R2)                                                         
         EDIT  (R1),(4,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
FMT2     L     R3,VSSB                                                          
         USING SSBD,R3                                                          
         OC    SSBLSTTM,SSBLSTTM                                                
         BZ    FMT6                                                             
         L     R0,TIMENOW                                                       
         S     R0,SSBLSTTM                                                      
         LA    RF,100                                                           
         BAS   RE,DIV                                                           
         LR    RF,R1                                                            
         L     R0,SSBSIN                                                        
         S     R0,SSBLSTSN                                                      
         MH    R0,=H'10000'                                                     
         BAS   RE,DIV                                                           
         MVC   0(6,R2),=C', T/R='                                               
         LA    R2,6(R2)                                                         
         EDIT  (R1),(7,0(R2)),4,ALIGN=LEFT                                      
         AR    R2,R0                                                            
         L     R1,SSBSIN                                                        
         S     R1,SSBLSTSN                                                      
         MVC   0(8,R2),=C', NTRNS='                                             
         LA    R2,8(R2)                                                         
         EDIT  (R1),(7,0(R2)),ALIGN=LEFT                                        
*                                                                               
FMT6     OI    SRVTOTH+6,X'80'                                                  
         MVC   MSG(41),=C'(XXXX) TERMINAL CONNECT DATA DISPLAYED - '            
         MVC   MSG+1(4),SYSNAME                                                 
         GOTO1 =V(TIMEOUT),DMCB,(1,TIMENOW),(X'45',MSG+41),RR=RB                
         OI    MSG+41,C'0'                                                      
         OI    SRVIDH+6,X'40'                                                   
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
*                                  SET TIME + SIN IN SSB                        
         L     R3,VSSB                                                          
         MVC   SSBLSTSN,SSBSIN                                                  
         MVC   SSBLSTTM,TIMENOW                                                 
         DROP  R3                                                               
         SPACE 1                                                                
EXIT     XMOD1 1                                                                
         SPACE 2                                                                
SETBXLE  LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         BR    R8                                                               
         SPACE 2                                                                
DIV      SR    R1,R1                                                            
         SRDA  R0,31                                                            
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRL   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
**********************************************************                      
*      USER FILTER TABLE  CL8'KEYWORD',A(TEST)           *                      
**********************************************************                      
         SPACE 1                                                                
USERTAB  DC    CL8'TSTATCLS',A(USER0001)                                        
         DC    CL8'TST4CLIP',A(USER0002)                                        
         DC    CL8'TST4TRC ',A(USER0003)                                        
         DC    CL8'TBUFF   ',A(USER0004)                                        
         DC    CL8'TSTATWRT',A(USER0005)                                        
         DC    CL8'TSTATWIP',A(USER0006)                                        
         DC    CL8'TST4UNLG',A(USER0007)                                        
         DC    CL8'TSTATDDS',A(USER0008)                                        
         DC    CL8'TSVCREQ ',A(USER0009)                                        
         DC    CL8'TST5PSWD',A(USER0010)                                        
         DC    CL8'TFLAGRTS',A(USER0011)                                        
         DC    CL8'TPERSON ',A(USER0012)                                        
         DC    H'0'                                                             
         EJECT                                                                  
**********************************************************                      
* USER FILTER ROUTINES  R1 & RE CANNOT BE USED HERE      *                      
* ON RETURNING TO RE IF CC=ZERO TERMINAL IS REJECTED     *                      
**********************************************************                      
         SPACE 1                                                                
         USING UTLD,R3                                                          
USER0001 TM    TSTAT3,TSTATCLS     CLSDST PENDING                               
         BR    RE                                                               
USER0002 TM    TSTAT4,TST4CLIP     CLSDST IN PROCESS                            
         BR    RE                                                               
USER0003 TM    TSTAT4,TST4TRC      VTAM TRACE ACTIVE                            
         BR    RE                                                               
USER0004 OC    TBUFF,TBUFF         BUFFER ASSIGNED TO TRM                       
         BR    RE                                                               
USER0005 TM    TSTAT3,TSTATWRT     WRITE PENDING                                
         BR    RE                                                               
USER0006 TM    TSTAT3,TSTATWIP     WRITE IN PROGRESS                            
         BR    RE                                                               
USER0007 TM    TSTAT4,TST4UNLG     UNABLE TO LOGON                              
         BR    RE                                                               
USER0008 TM    TSTAT1,TSTATDDS     DDS TERMINAL                                 
         BR    RE                                                               
USER0009 OC    TSVCREQ,TSVCREQ     TSVCREQ                                      
         BR    RE                                                               
USER0010 TM    TSTAT5,TST5PSWD     TST5PSWD                                     
         BR    RE                                                               
USER0011 TM    TFLAG,TFLAGRTS      TFLAGRTS                                     
         BR    RE                                                               
USER0012 OC    TPERSON,TPERSON     TPERSON                                      
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
ALPHANUM DC    C'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'                          
         SPACE                                                                  
SPACES   DC    24C' '                                                           
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
SRPARMS  DS    6F                                                               
VPGMLST  DS    V                                                                
ANXTLIN  DS    A                                                                
BSPARS   DS    6F                                                               
TIMENOW  DS    F                   TIME IN 1/100 SEC                            
TIMENOW1 DS    F                   TIME IN 1/38400 SEC                          
RELO     DS    A                                                                
*                                                                               
FSTIM    DS    F                                                                
FSTIM1   DS    F                                                                
TIMEUNIT DS    X                                                                
         DS    X                                                                
FAGY     DS    CL2                                                              
FSYSF    DS    X                                                                
FSYSO    DS    X                                                                
FLUID    DS    CL8                                                              
FTNO     DS    XL2                                                              
FSTAR    DS    C                                                                
FPROC    DS    C                                                                
FVTBLD   DS    C                                                                
FCON     DS    C                                                                
FPROG    DS    X                                                                
FBROAD   DS    C                                                                
FPRINT   DS    C                                                                
FJOBS    DS    X                                                                
STEREO   DS    X                                                                
AUSER    DS    A                   USER FILTERS                                 
         DS    A                                                                
         DS    A                                                                
AUSERX   DS    A                   1-4                                          
*                                                                               
NTRMS    DS    PL3                                                              
ACTRMS   DS    PL3                                                              
HAVLUID  DS    X                                                                
RPL      DS    C                                                                
LINPARAM DS    A                                                                
ARPL     DS    A                                                                
NSYM     DS    CL8                                                              
SORTSW   DS    X                                                                
SYSNAME  DS    CL4                                                              
MSG      DS    CL60                                                             
WORK     DS    XL256                                                            
*                                                                               
WRKL     EQU   *-WRKD                                                           
*                                                                               
DMCB     DS    6F                                                               
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VGETFACT DS    A                                                                
VSYSLST  DS    A                                                                
BSTAB    DS    36XL256                                                          
WRKX     EQU   *                                                                
         EJECT                                                                  
* DSECT TO COVER OUTPUT LINE                                                    
*                                                                               
TRMLD    DSECT                                                                  
         DS    0CL46                                                            
         DS    CL8                                                              
TRMNUM   DS    CL4                                                              
         DS    CL1                                                              
TRMLUID  DS    CL8                                                              
         DS    CL1                                                              
TRMAGY   DS    CL2                                                              
         DS    CL1                                                              
TRMSYS   DS    CL4                                                              
         DS    CL1                                                              
TRMPRG   DS    CL3                                                              
         DS    CL1                                                              
TRMTIM   DS    CL8                                                              
         DS    CL1                                                              
TRMSTAT  DS    CL3                                                              
         EJECT                                                                  
         ISTDNIB                                                                
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FARPLD                                                                        
         PRINT OFF                                                              
       ++INCLUDE FARPLD                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         EJECT                                                                  
SRLSTFFD DSECT                                                                  
         DS    CL64                                                             
* SRLSTFFD                                                                      
       ++INCLUDE SRLSTFFD                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRLST00X  05/01/02'                                      
         END                                                                    
