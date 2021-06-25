*          DATA SET ACLDXRSIZ  AT LEVEL 070 AS OF 07/24/20                      
*PHASE ACXSIZA                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE CUREDIT                                                                
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HELLO                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*          **** DO NOT DELETE THIS MODULE ****                                  
***********************************************************************         
* THIS PROGRAM CREATES A REPORT BY AGENCY BY RECORD TYPE AND THE      *         
* RECORD SIZES OF THE DIFFERENT TYPE OF RECORDS. IT ALSO BIFURCATES   *         
* THOSE RECORDS BY THE ACTIVITY DATE OF WITHIN 7 YRS, 07 - 10 YRS,    *         
* 10 - 15 YRS AND GREATER THAN 15 YRS.                                *         
* IT ALSO DISPLAYS THE OLDEST DATE IN A SEPARATE COLUMN AND THE TOTAL *         
* RECORD SIZE.                                                        *         
*                                                                     *         
* LOAD=ACXSIZ                                                         *         
* UPSI=10000000     <- BIFURCATES BY UNIT LEDGER IF PROVIDED IN JCL   *         
***********************************************************************         
* USR  LVL DATE    TICKET     DESCRIPTION                             *         
* ---  --- ----    ------     -----------                             *         
* RKEJ 001 24JUL20 SPEC-44229 EXTERN - PRINT RECORD SIZE/OLDEST DATE  *         
***********************************************************************         
DMLDEXT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         B     DMXCTL                                                           
ORG      DC    C'*VERIFY*050129051230',C'X',X'FF'                               
         ORG   ORG+8                                                            
         DC    X'FA'                                                            
         ORG   *+5                                                              
         DC    X'FA'                                                            
         ORG                                                                    
COMPANY  DS    X                                                                
DMXRTST  DS    0H                                                               
*        B     DMXPURGE            PURGE WHEN TESTING                           
         B     DMXKEEP             KEEP WHEN LIVE                               
         EJECT                                                                  
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST(PLISTL),0(R1)                                              
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
                                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
                                                                                
         USING CPYTABD,R1                                                       
         USING CNTD,R4             R4=A(COUNTERS)                               
DMXINIT  DS    0H                                                               
         XC    PREVKEY,PREVKEY                                                  
         LHI   RF,256              256 COMPANIES                                
         L     R1,ACPYTAB                                                       
DMXIN10  LHI   RE,RECTABN          DIFF REC TYPES                               
         LA    R4,CPYCNTS                                                       
DMXIN20  ZAP   RESIZE,=P'0'                                                     
         LA    R4,CNTL(R4)                                                      
         BCT   RE,DMXIN20                                                       
         LAY   R1,CPYTABL(R1)                                                   
         BCT   RF,DMXIN10                                                       
*                                                                               
         LHI   RE,RECTABN          DIFF REC TYPES                               
         LAY   R4,FILECNTS         R4=A(FILE TOTAL COUNTERS)                    
DMXIN30  ZAP   RESIZE,=P'0'                                                     
         LA    R4,CNTL(R4)                                                      
         BCT   RE,DMXIN30                                                       
*                                                                               
         USING LDDEFND,RF                                                       
         L     RF,VLDDEFN                                                       
         L     RE,LUPSIVAL         A(UPSI)                                      
         MVC   UPSI,0(RE)                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TDATE00)                                    
         GOTO1 ADDAY,DMCB,(C'Y',TDATE00),(0,TDATI07),-7                         
         GOTO1 ADDAY,DMCB,(C'Y',TDATE00),(0,TDATI10),-10                        
         GOTO1 ADDAY,DMCB,(C'Y',TDATE00),(0,TDATI15),-15                        
         GOTO1 DATCON,DMCB,(0,TDATI07),(1,TDAT307)                              
         GOTO1 DATCON,DMCB,(0,TDATI10),(1,TDAT310)                              
         GOTO1 DATCON,DMCB,(0,TDATI15),(1,TDAT315)                              
*                                                                               
         J     DMXIT                                                            
         DROP  R1,R4,RF                                                         
         EJECT                                                                  
***********************************************************************         
DMXRET   DS    0H                                                               
         J     DMXIT                                                            
***********************************************************************         
DMXIT    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
DMXEOF   DS    0H                                                               
         B     DCPRINT                                                          
*        B     DMXIT                                                            
***********************************************************************         
         SPACE                                                                  
         EJECT                                                                  
DMXREC   MVI   RECFLAG,0                                                        
         L     R2,VREC                                                          
         USING TRNRECD,R2                                                       
*                                                                               
         CLC   TRNKEY,PREVKEY                                                   
         JE    DMXKEEP                                                          
         MVC   PREVKEY,TRNKEY                                                   
*                                                                               
         GOTO1 VRECTYP,DMCB,(C'D',TRNRECD)                                      
         MVC   WRECTYPE,0(R1)                                                   
         MVC   COMPANY,1(R1)                                                    
*                                                                               
         USING RECTABD,R3                                                       
         L     R3,ARECTAB          FIND ENTRY FOR RECORD TYPE                   
         LA    RE,RECTABL                                                       
         L     RF,ARECTABU                                                      
         XR    R4,R4               R4=DISPLACEMENT INTO COUNTER TABLE           
DCNT02   CLI   RECTABD,EOT                                                      
         BE    DCNT06                                                           
         CLC   RECTYPE,WRECTYPE    MATCH ON RECORD TYPE                         
         BNE   DCNT04                                                           
         TM    RECINDS1,RECIMST                                                 
         JZ    DMXKEEP                                                          
*&&US                                                                           
         CLI   RECTYPE,ACRTTRN     TEST TRANSACTION RECORD                      
         BNE   DCNT03                                                           
         LR    R1,RE               SAVE RE                                      
         BRAS  RE,CNTUNL                                                        
         LR    RE,R1               RESTORE RE                                   
         TM    TRNRSTA2,TRNSPEEL   TEST PEELED TRANSACTION                      
         BNO   DCNT03                                                           
         TM    RECINDS1,RECIPLD    TEST ENTRY FOR PEELED                        
         BNO   DCNT04              GET ANOTHER TABLE ENTRY                      
*&&                                                                             
DCNT03   CLI   RECTYPE,ACRTUNKN    TEST FOR UNKNOWN TYPE ENTRY                  
         BNE   DCNT10                                                           
         CLC   RECUNKEY,TRNKEY     MATCH ON FIST BYTE OF KEY                    
         BE    DCNT10                                                           
DCNT04   LA    R4,CNTL(R4)                                                      
         BXLE  R3,RE,DCNT02                                                     
*                                                                               
         MVC   RECNAME,T@UNKOTH                                                 
         MVI   RECINDS1,RECIDIR+RECIUNK+RECIFILE                                
         B     DCNT10                                                           
*                                                                               
DCNT06   MVC   RECTYPE,WRECTYPE    ADD ENTRY FOR UNKNOWN                        
         CLI   RECTYPE,ACRTUNKN    ACRECTYP RETURNED UNKNOWN                    
         BNE   DCNT08                                                           
         MVC   RECNAME,T@UNKTYP                                                 
         GOTO1 VHEXOUT,DMCB,TRNRECD,RECNAME+2,1,=C'TOG'                         
         MVI   RECINDS1,RECIDIR+RECIUNK+RECIFILE                                
         MVC   RECUNKEY,TRNKEY                                                  
         B     DCNT10                                                           
*                                                                               
DCNT08   MVC   RECNAME,T@UNKEQU    ACRECTYP EQUATE IS UNKNOWN                   
         LLC   R0,RECTYPE                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECNAME+1(3),DUB                                                 
         MVI   RECINDS1,RECIDIR+RECIUNK                                         
*                                                                               
DCNT10   TM    RECINDS1,RECIFILE   TEST FOR FILE TOTALS ONLY                    
         BZ    DCNT11                                                           
         LA    R4,FILECNTS(R4)     R4=A(COUNTERS FOR FILE)                      
         B     DCNT12                                                           
*                                                                               
         USING CPYTABD,R1                                                       
DCNT11   LLC   R1,COMPANY                                                       
         MHI   R1,CPYTABL                                                       
         A     R1,ACPYTAB                                                       
         MVC   CPYCODE,COMPANY                                                  
         OI    CPYINDS,CPYICNT                                                  
         LA    R4,CPYCNTS(R4)      R4=A(COUNTERS FOR COMPANY)                   
         DROP  R1                                                               
*                                                                               
         USING CNTD,R4             R4=A(COUNTERS)                               
DCNT12   LA    R1,CNTOUT           INCREMENT OUTPUT                             
         TM    TRNRSTAT,TRNSDELT                                                
         BZ    *+8                                                              
         LA    R1,CNTPUR           Increment purged instead                     
         ICM   RE,15,0(R1)                                                      
         AHI   RE,1                INCREASE COUNTER                             
         STCM  RE,15,0(R1)                                                      
*                                                                               
         CLI   RECDOFF,0           DATE OFFSET 0?                               
         JE    DCNT50              SKIP RECORD!                                 
         LR    R1,R2               R1 HAS THE RECORD!                           
         CLI   RECELMC,0           ELEMENT CODE PRESENT?                        
         JE    DCNT14              GET DATE FROM RECORD NOT ELEMENT!            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),(RECELMC,TRNRECD),0,0                  
         CLI   12(R1),0                                                         
         JNE   DCNT50                                                           
         L     R1,12(R1)                                                        
*                                                                               
DCNT14   LLC   RE,RECDOFF          R1 HAS RECORD OR AN ELEMENT!                 
         AR    R1,RE                                                            
         CLI   0(R1),X'00'         NO DATE SO SKIP RECORD!                      
         JE    DCNT50                                                           
         CLI   RECDTYP,X'01'       2 BYTE COMPRESSED DATE ?                     
         JE    DCNT15                                                           
         CLI   RECDTYP,X'02'       2 BYTE PACKED DATE (YM) ?                    
         JE    DCNT16                                                           
         CLI   RECDTYP,X'03'       3 BYTE PACKED DATE (YMD) ?                   
         JE    DCNT17                                                           
         CLI   RECDTYP,X'04'       4 BYTE CHAR DATE (YYMM) ?                    
         JE    DCNT18                                                           
         CLI   RECDTYP,X'05'       3 BYTE BINARY DATE (YMD) ?                   
         JE    DCNT19                                                           
         DC    H'0'                FAIL IF INVALID DATE TYPE!                   
*                                                                               
DCNT15   MVC   PREVD6(2),0(R1)                                                  
         GOTO1 DATCON,DMCB,(2,PREVD6),(1,PREVD3)   COMPRESSED DATE              
         J     DCNT40                                                           
*                                                                               
DCNT16   CLI   REC2SFL,X'01'       2 BYTE DATE YM (2'S COMP) ?                  
         JNE   DCNT42                                                           
         XR    RF,RF                                                            
         ICM   RF,3,0(R1)                                                       
         SHI   RF,1                                                             
         X     RF,=X'FFFFFFFF'                                                  
         STCM  RF,3,PREVD3                                                      
         J     DCNT40                                                           
*                                                                               
DCNT17   CLI   REC2SFL,X'01'       3 BYTE DATE YMD (2'S COMP) ?                 
         JNE   DCNT42                                                           
         XR    RF,RF                                                            
         ICM   RF,7,0(R1)                                                       
         SHI   RF,1                                                             
         X     RF,=X'FFFFFFFF'                                                  
         STCM  RF,7,PREVD3                                                      
         J     DCNT40                                                           
*                                                                               
DCNT18   MVC   PREVD6(4),0(R1)     4 BYTE CHAR DATE (YYMM)                      
         MVC   PREVD6+4(2),=C'01'                                               
         GOTO1 DATCON,DMCB,(0,PREVD6),(1,PREVD3)                                
         J     DCNT40                                                           
*                                                                               
DCNT19   MVC   PREVD6(3),0(R1)                                                  
         GOTO1 DATCON,DMCB,(3,PREVD6),(1,PREVD3)  3 BYTE YMD IN BINARY          
*                                                                               
DCNT40   LA    R1,PREVD3           ACTUAL DATE OF R1 - REVERSE 2'S COM          
DCNT42   LA    RF,CNTDT1                                                        
         CLC   TDAT307(2),0(R1)                                                 
         JNH   DCNT44                                                           
         LA    RF,CNTDT2                                                        
         CLC   TDAT310(2),0(R1)                                                 
         JNH   DCNT44                                                           
         LA    RF,CNTDT3                                                        
         CLC   TDAT315(2),0(R1)                                                 
         JNH   DCNT44                                                           
         LA    RF,CNTDT4                                                        
DCNT44   ICM   RE,15,0(RF)                                                      
         AHI   RE,1                                                             
         STCM  RE,15,0(RF)                                                      
         CLI   OLDDATE,0                                                        
         JNH   *+14                                                             
         CLC   OLDDATE(2),0(R1)       COMPARE DATES                             
         JNH   DCNT50                                                           
         MVC   OLDDATE(2),0(R1)       KEEP THE OLDER DATE TO REPORT             
*                                                                               
DCNT50   LLH   RE,TRNRLEN                                                       
         CVD   RE,DUB                                                           
         AP    RESIZE,DUB                                                       
         J     DMXKEEP                                                          
         DROP  R4,R3,R2                                                         
*                                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT COMPANY TOTALS BY RECTYPE/STATUS FROM ACCUMBUF,              *          
* ACCUMULATE AND PRINT RECTYPE/STATUS FILE TOATLS                    *          
**********************************************************************          
         SPACE 1                                                                
DCPRINT  DS    0H                                                               
         L     R8,VBOXAREA                                                      
         USING BOXD,R8                                                          
         BAS   RE,SETPRINT                                                      
*        MVC   BOXWIDTH,=F'198'                                                 
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXBLANK,C'N'                                                    
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+(PBXL-P),C'L'                                            
         MVI   BOXCOLS+(PBXC1-P),C'C'                                           
         MVI   BOXCOLS+(PBXC2-P),C'C'                                           
         MVI   BOXCOLS+(PBXC3-P),C'C'                                           
         MVI   BOXCOLS+(PBXS1-P),C'C'                                           
         MVI   BOXCOLS+(PBXS2-P),C'C'                                           
         MVI   BOXCOLS+(PBXS3-P),C'C'                                           
         MVI   BOXCOLS+(PBXS4-P),C'C'                                           
         MVI   BOXCOLS+(PBXS8-P),C'C'                                           
         MVI   BOXCOLS+(PBXRZ-P),C'C'                                           
         MVI   BOXCOLS+(PBXR-P),C'R'                                            
*                                                                               
         MVC   TITLE+((L'TITLE-L'T@TITLE)/2)(L'T@TITLE),T@TITLE                 
         MVC   MCPY,T@CPY                                                       
         MVI   MID1,0              FORCE BLANK LINES                            
         MVI   MID3,0                                                           
         MVI   MID4,0                                                           
         MVI   SUB1,0                                                           
         MVC   SUB2+(PREC-P)(L'PREC),T@REC                                      
         MVC   SUB2+(PINP-P)(L'PINP),T@INP                                      
         MVC   SUB2+(PPUR-P)(L'PPUR),T@PUR                                      
         MVC   SUB2+(POUT-P)(L'POUT),T@OUT                                      
         MVC   SUB2+(PDT1-P)(L'PDT1),T@SZ1                                      
         MVC   SUB2+(PDT2-P)(L'PDT2),T@SZ2                                      
         MVC   SUB2+(PDT3-P)(L'PDT3),T@SZ3                                      
         MVC   SUB2+(PDT4-P)(L'PDT4),T@SZ4                                      
         MVC   SUB2+(PDATE-P)(L'PDATE),T@DATE                                   
         MVC   SUB2+(PRESZ-P)(L'PRESZ),T@RSZ                                    
*                                                                               
         L     R4,ACPYTAB                                                       
         USING CPYTABD,R4          R4=A(COMPANY TABLE)                          
         LA    R3,CPYTABN                                                       
*                                                                               
DCPRT02  TM    CPYINDS,CPYICNT     TEST ANYTHING FOR COMPANY                    
         BZ    DCPRT08                                                          
         MVC   COMPCODE,CPYCODE                                                 
         GOTO1 VHEXOUT,DMCB,CPYCODE,MCODE,L'CPYCODE,=C'TOG'                     
*                                                                               
         GOTO1 INITTOT                                                          
         GOTO1 PRNTCPY,CPYCNTS                                                  
*                                                                               
DCPRT08  LAY   R4,CPYTABL(R4)                                                   
         BCT   R3,DCPRT02                                                       
         DROP  R4                                                               
*                                                                               
         MVC   MFTOTS,T@FTOTS                                                   
         GOTO1 INITTOT                                                          
         GOTO1 PRNTCPY,FILECNTS                                                 
*                                                                               
         MVI   BOXYORN,C'N'                                                     
         BAS   RE,SETPRINT                                                      
         B     DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT TOTALS FOR COMPANY                                 *         
*                                                                     *         
* NTRY: R1=A(COUNTERS TABLE)                                          *         
***********************************************************************         
         SPACE 1                                                                
PRNTCPY  NTR1  ,                                                                
         LR    R3,R1               R3=A(COUNTRES)                               
         USING CNTD,R3                                                          
         L     R2,ARECTAB                                                       
         USING RECTABD,R2          R2=A(RECORD TYPE TABLE)                      
         LAY   R4,FILECNTS         R4=A(FILE TOTAL COUNTERS)                    
*                                                                               
PCPY02   OC    CNTD(8),CNTD                                                     
         BZ    PCPY08              TEST ANY COUNTED                             
         MVC   PREC(L'PREC),RECNAME  OUTPUT ROW                                 
         GOTO1 PRNTROW,DMCB,CNTD,RECTABD                                        
         GOTO1 ADDCNT,DMCB,(R4),CNTD                                            
         TM    UPSI,UPSIUL         SHOW UNIT/LEDGERS ?                          
         JZ    PCPY08                                                           
         CLC   RECNAME(18),=C'ACCMST Transaction'                               
         JNE   PCPY08                                                           
         GOTO1 PRNTUNL,COMPCODE    PRINT UNIT/LEDGER                            
*                                                                               
PCPY08   LA    R3,CNTL(R3)                                                      
         LA    R4,CNTL(R4)                                                      
         LA    R2,RECTABL(R2)                                                   
         CLI   RECTABD,EOT                                                      
         BNE   PCPY02                                                           
         DROP  R2,R3                                                            
*                                                                               
         MVC   HALF,LINE           DRAW HORIZONTAL LINE                         
         AP    HALF,=P'1'          UNLESS AT BOTTOM OF PAGE                     
         CLC   HALF,MAXLINE                                                     
         BH    PCPY10                                                           
         BE    *+8                                                              
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
PCPY10   LAY   R2,TOT              PRINT TOTALS                                 
         USING TOTD,R2                                                          
PCPY12   OC    TOTCNT(8),TOTCNT                                                 
         BZ    PCPY18                                                           
         MVC   PREC,TOTNAME                                                     
         GOTO1 PRNTROW,DMCB,TOTCNT,0                                            
         XC    TOTCNT,TOTCNT                                                    
PCPY18   LA    R2,TOTL(R2)                                                      
         CLI   TOTD,EOT                                                         
         BNE   PCPY12                                                           
         DROP  R2                                                               
*                                                                               
         CLC   LINE,MAXLINE                                                     
         BNL   PCPY20                                                           
         MVI   BOXREQ,C'C'         CLOSE BOX                                    
         GOTO1 VPRINTER                                                         
PCPY20   ZAP   LINE,=P'99'         SET NEW PAGE                                 
         ZAP   PAGE,=P'1'                                                       
*                                                                               
         B     DMXIT                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO PRINT UNIT LEDGER FOR COMPANY                            *         
*                                                                     *         
* NTRY: R1=A(COMPANY CODE)                                            *         
***********************************************************************         
         SPACE 1                                                                
PRNTUNL  NTR1  ,                                                                
         LR    R3,R1               R3=A(COUNTRES)                               
*                                                                               
         USING UNLD,R5                                                          
         LA    R5,UNLWRK                                                        
         XC    UNLWRK,UNLWRK                                                    
         MVC   UNLCPY,0(R3)                                                     
*                                                                               
         USING BIND,R7                                                          
         L     R7,AUNLTAB                                                       
         MVC   DMCB+8(16),BININ                                                 
         LA    R6,BINTAB           A(TABLE)                                     
         GOTO1 VBINSRCH,DMCB,(X'02',UNLWRK),(R6)                                
         CLI   DMCB,1                                                           
         JE    DMXIT                                                            
*                                                                               
         L     R5,0(R1)                                                         
PUNL10   CLC   UNLCPY,0(R3)                                                     
         JNE   DMXIT                                                            
*                                                                               
         MVC   PREC(L'UNL),UNL                                                  
*                                                                               
         ICM   R4,15,UNLOUT        R4=NO. OF RECORDS OUTPUT                     
         BZ    PUNL12                                                           
         EDIT  (R4),(10,POUT)                                                   
*                                                                               
PUNL12   ICM   RE,15,UNLPUR        R5=NO. OF RECORDS PURGED                     
         BZ    PUNL14                                                           
         EDIT  (RE),(10,PPUR)                                                   
*                                                                               
PUNL14   AR    R4,RE               R4=NO. OF RECORDS INPUT                      
         EDIT  (R4),(10,PINP)                                                   
*                                                                               
PUNL16   ICM   R4,15,UNLDT1                                                     
         BZ    PUNL18                                                           
         EDIT  (R4),(8,PDT1)                                                    
*                                                                               
PUNL18   ICM   R4,15,UNLDT2                                                     
         BZ    PUNL20                                                           
         EDIT  (R4),(8,PDT2)                                                    
*                                                                               
PUNL20   ICM   R4,15,UNLDT3                                                     
         BZ    PUNL22                                                           
         EDIT  (R4),(8,PDT3)                                                    
*                                                                               
PUNL22   ICM   R4,15,UNLDT4                                                     
         BZ    PUNL24                                                           
         EDIT  (R4),(8,PDT4)                                                    
*                                                                               
PUNL24   MVI   UNLDAT+2,X'00'     NO DAY REQUIRED IN OUTPUT                     
         GOTO1 DATCON,DMCB,(1,UNLDAT),(22,PDATE)                                
*                                                                               
PUNL26   ICM   R4,15,UNLSIZ                                                     
         BZ    PUNL34                                                           
         EDIT  (R4),(15,PRESZ)                                                  
*                                                                               
PUNL34   GOTO1 VPRINTER                                                         
         LA    R5,UNLLNQ(R5)                                                    
         J     PUNL10                                                           
*                                                                               
         SPACE 1                                                                
***********************************************************************         
*  PRINT TOTALS FOR A ROW                                             *         
*                                                                     *         
* NTRY: P1=A(COUNTER TABLE ENTRY)                                     *         
*       P2=A(FILE TOTAL TABLE ENTRY OR ZERO)                          *         
***********************************************************************         
         SPACE 1                                                                
PRNTROW  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING CNTD,R2             R2=COUNTERS TABLE                            
         USING RECTABD,R3          R3=RECORD TABLE ENTRY                        
*                                                                               
         LTR   R3,R3                                                            
         BZ    PROW10                                                           
         LAY   R4,TOT                                                           
         USING TOTD,R4                                                          
PROW02   MVC   BYTE,TOTTYPE                                                     
         NC    BYTE,RECINDS1                                                    
         BZ    PROW08                                                           
         GOTO1 ADDCNT,DMCB,TOTCNT,CNTD                                          
PROW08   LA    R4,TOTL(R4)                                                      
         CLI   TOTD,EOT                                                         
         BNE   PROW02                                                           
         DROP  R4                                                               
*                                                                               
PROW10   ICM   R4,15,CNTOUT        R4=NO. OF RECORDS OUTPUT                     
         BZ    PROW12                                                           
         EDIT  (R4),(10,POUT)                                                   
*                                                                               
PROW12   ICM   R5,15,CNTPUR        R5=NO. OF RECORDS PURGED                     
         BZ    PROW14                                                           
         EDIT  (R5),(10,PPUR)                                                   
*                                                                               
PROW14   AR    R4,R5               R4=NO. OF RECORDS INPUT                      
         EDIT  (R4),(10,PINP)                                                   
*                                                                               
PROW16   ICM   R5,15,CNTDT1                                                     
         BZ    PROW18                                                           
         EDIT  (R5),(8,PDT1)                                                    
*                                                                               
PROW18   ICM   R5,15,CNTDT2                                                     
         BZ    PROW20                                                           
         EDIT  (R5),(8,PDT2)                                                    
*                                                                               
PROW20   ICM   R5,15,CNTDT3                                                     
         BZ    PROW22                                                           
         EDIT  (R5),(8,PDT3)                                                    
*                                                                               
PROW22   ICM   R5,15,CNTDT4                                                     
         BZ    PROW24                                                           
         EDIT  (R5),(8,PDT4)                                                    
*                                                                               
PROW24   MVI   OLDDATE+2,X'00'     NO DAY REQUIRED IN OUTPUT                    
         GOTO1 DATCON,DMCB,(1,OLDDATE),(22,PDATE)                               
*                                                                               
PROW32   EDIT  (P8,RESIZE),PRESZ,ALIGN=RIGHT,ZERO=NOBLANK                       
*                                                                               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         DROP  R2,R3                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD COUNTERS                                             *         
*                                                                     *         
* NTRY: P1=A(COUNTERS TO BE ADDED TO)                                 *         
*       P2=A(COUNTERS TO BE ADDED)                                    *         
***********************************************************************         
         SPACE 1                                                                
ADDCNT   STM   RE,R1,12(RD)                                                     
         LM    RE,RF,0(R1)                                                      
         USING CNTD,RE                                                          
         ICM   R0,15,CNTOUT-CNTD(RF)                                            
         BZ    ADDCNT2                                                          
         ICM   R1,15,CNTOUT                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTOUT                                                     
ADDCNT2  ICM   R0,15,CNTPUR-CNTD(RF)                                            
         BZ    ADDCNT3                                                          
         ICM   R1,15,CNTPUR                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTPUR                                                     
ADDCNT3  ICM   R0,15,CNTDT1-CNTD(RF)                                            
         BZ    ADDCNT4                                                          
         ICM   R1,15,CNTDT1                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTDT1                                                     
ADDCNT4  ICM   R0,15,CNTDT2-CNTD(RF)                                            
         BZ    ADDCNT5                                                          
         ICM   R1,15,CNTDT2                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTDT2                                                     
ADDCNT5  ICM   R0,15,CNTDT3-CNTD(RF)                                            
         BZ    ADDCNT6                                                          
         ICM   R1,15,CNTDT3                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTDT3                                                     
ADDCNT6  ICM   R0,15,CNTDT4-CNTD(RF)                                            
         BZ    ADDCNT7                                                          
         ICM   R1,15,CNTDT4                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTDT4                                                     
ADDCNT7  LA    R1,RESIZE-CNTD(RF)                                               
         AP    RESIZE,0(8,R1)                                                   
         DROP  RE                                                               
ADDCNTX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE TOTAL COUNTS TABLE                                       *         
***********************************************************************         
         SPACE 1                                                                
INITTOT  NTR1  ,                                                                
         USING TOTD,R2                                                          
         LAY   R2,TOT              PRINT TOTALS                                 
         USING CNTD,R4                                                          
*                                                                               
INTOT10  LA    R4,TOTCNT                                                        
         ZAP   RESIZE,=P'0'                                                     
         LA    R2,TOTL(R2)                                                      
         CLI   TOTD,EOT                                                         
         JNE   INTOT10                                                          
         J     DMXIT                                                            
         DROP  R2,R4                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* COUNT UNIT LEDGER ACCMST TRANSACTIONS                               *         
***********************************************************************         
         SPACE 1                                                                
CNTUNL   NTR1  ,                                                                
         USING TRNRECD,R2                                                       
         USING RECTABD,R3                                                       
         MVI   BINFLG,C'N'                                                      
*                                                                               
         TM    UPSI,UPSIUL         SHOW UNIT/LEDGERS ?                          
         JZ    DMXIT                                                            
         TM    TRNRSTA2,TRNSPEEL                                                
         BO    DMXIT               EXIT IF PEELED TRANS                         
*                                                                               
         USING UNLD,R5                                                          
         LA    R5,UNLWRK                                                        
         XC    UNLWRK,UNLWRK                                                    
         MVC   UNLCPY,COMPANY                                                   
         MVC   UNL(2),TRNKUNT                                                   
*                                                                               
         USING BIND,R7                                                          
         L     R7,AUNLTAB                                                       
         MVC   DMCB+8(16),BININ                                                 
         LA    R6,BINTAB           A(TABLE)                                     
         GOTO1 VBINSRCH,DMCB,(X'00',UNLWRK),(R6)                                
         CLI   DMCB,1                                                           
         JE    CNTUNL10                                                         
*                                                                               
         MVI   BINFLG,C'Y'         RECORD FOUND IN BINSRCH                      
         L     R5,0(R1)                                                         
         J     CNTUNL20                                                         
*                                                                               
*                                                                               
CNTUNL10 LA    R5,UNLWRK           COMPANY UNL ENTRY NOT FOUND                  
         XC    UNLWRK,UNLWRK                                                    
         MVC   UNLCPY,COMPANY                                                   
         MVC   UNL(2),TRNKUNT                                                   
                                                                                
CNTUNL20 LA    R4,UNLOUT           INCREMENT OUTPUT                             
         TM    TRNRSTAT,TRNSDELT                                                
         JZ    *+8                                                              
         LA    R4,UNLPUR           Increment purged instead                     
         ICM   RE,15,0(R4)                                                      
         AHI   RE,1                INCREASE COUNTER                             
         STCM  RE,15,0(R4)                                                      
*                                                                               
         CLI   RECDOFF,0           DATE OFFSET 0?                               
         JE    CNTUNL50            SKIP RECORD!                                 
         LR    R1,R2               R1 HAS THE RECORD!                           
         CLI   RECELMC,0           ELEMENT CODE PRESENT?                        
         JE    CNTUNL30            GET DATE FROM RECORD NOT ELEMENT!            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),(RECELMC,TRNRECD),0,0                  
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
         L     R1,12(R1)                                                        
*                                                                               
CNTUNL30 LLC   RE,RECDOFF          R1 HAS RECORD OR AN ELEMENT!                 
         AR    R1,RE                                                            
*                                                                               
CNTUNL36 LA    RF,UNLDT1                                                        
         CLC   TDAT307(2),0(R1)                                                 
         JNH   CNTUNL38                                                         
         LA    RF,UNLDT2                                                        
         CLC   TDAT310(2),0(R1)                                                 
         JNH   CNTUNL38                                                         
         LA    RF,UNLDT3                                                        
         CLC   TDAT315(2),0(R1)                                                 
         JNH   CNTUNL38                                                         
         LA    RF,UNLDT4                                                        
CNTUNL38 ICM   RE,15,0(RF)                                                      
         AHI   RE,1                                                             
         STCM  RE,15,0(RF)                                                      
         CLI   UNLDAT,0                                                         
         JNH   *+14                                                             
         CLC   UNLDAT(2),0(R1)       COMPARE DATES                              
         JNH   CNTUNL50                                                         
         MVC   UNLDAT(2),0(R1)       KEEP THE OLDER DATE TO REPORT              
*                                                                               
CNTUNL50 LLH   RE,TRNRLEN                                                       
         LA    R4,UNLSIZ                                                        
         ICM   RF,15,0(R4)                                                      
         AR    RF,RE                                                            
         STCM  RF,15,0(R4)                                                      
                                                                                
         CLI   BINFLG,C'Y'                                                      
         JE    CNTUNL99                                                         
         L     R7,AUNLTAB          R5=A(PACKAGE TABLE)                          
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB                                                        
         GOTO1 VBINSRCH,DMCB,(X'01',(R5)),(R6)                                  
         OC    DMCB(4),DMCB                                                     
         JZ    *+2                                                              
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         JNE   *+2                 ABEND!                                       
CNTUNL99 J     DMXIT                                                            
         DROP  R2,R3,R5,R7                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALIZE PRINT DSECT                                   *         
***********************************************************************         
         SPACE 1                                                                
SETPRINT MVC   TITLE,SPACES                                                     
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         MVC   MID4,SPACES                                                      
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB3,SPACES                                                      
         MVC   P,SPACES                                                         
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         BR    RE                                                               
         EJECT                                                                  
EOT      EQU   X'00'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
UPSI     DS    XL1                                                              
UPSIUL   EQU   X'80'                   UNIT/LEDGER                              
TDATE00  DS    CL6                                                              
TDATI07  DS    CL6                                                              
TDATI10  DS    CL6                                                              
TDATI15  DS    CL6                                                              
TDAT307  DS    PL3                                                              
TDAT310  DS    PL3                                                              
TDAT315  DS    PL3                                                              
*                                                                               
ACCMST   DC    C'ACCMST  '                                                      
PREVKEY  DC    XL42'00'                                                         
PREVD6   DC    XL6'00'                                                          
PREVD3   DC    XL3'00'                                                          
PREVUNL  DC    XL3'00'                                                          
ARECTAB  DC    A(RECTAB)                                                        
ARECTABU DC    A(RECTABU-1)                                                     
ACPYTAB  DC    A(CPYTAB)                                                        
AUNLTAB  DC    A(UNLTAB)                                                        
VRECTYP  DC    V(ACRECTYP)                                                      
VBINSRCH DC    V(BINSRCH)                                                       
VSORTER  DC    V(SORTER)                                                        
VHELLO   DC    V(HELLO)                                                         
DATCON   DC    V(DATCON)                                                        
ADDAY    DC    V(ADDAY)                                                         
CUREDIT  DC    V(CUREDIT)                                                       
CHEXIN   DC    V(HEXIN)                                                         
VBOXAREA DC    V(BOXAREA)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
         SPACE 1                                                                
T@TITLE  DC    C'Logical Record Counts'                                         
T@REC    DC    CL(L'PREC)'Record Type'                                          
T@INP    DC    CL(L'PINP)'     Input'                                           
T@PUR    DC    CL(L'PPUR)'    Purged'                                           
T@OUT    DC    CL(L'POUT)'    Output'                                           
T@SZ1    DC    CL(L'PDT1)'   00-07'                                             
T@SZ2    DC    CL(L'PDT2)'   07-10'                                             
T@SZ3    DC    CL(L'PDT3)'   10-15'                                             
T@SZ4    DC    CL(L'PDT4)'     >15'                                             
T@RSZ    DC    CL(L'PRESZ)'    Record Size'                                     
T@DATE   DC    CL(L'PDATE)'OLD DATE'                                            
T@FTOTS  DC    CL(L'MFTOTS)'File Totals'                                        
T@CPY    DC    CL(L'MCPY)'Company'                                              
T@UNKOTH DC    CL(L'RECNAME)'.......All other unknowns'                         
T@UNKEQU DC    CL(L'RECNAME)'''000'' Unknown record equate'                     
T@UNKTYP DC    CL(L'RECNAME)'X''00'' Unknown record type'                       
         SPACE 1                                                                
FILECNTS DC    (RECTABN)XL(CNTL)'00' FILE TOTAL COUNTERS                        
         SPACE 1                                                                
TOT      DS    0XL(TOTL)                                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of Unknowns'                               
         DC    AL1(RECIUNK),XL(CNTL)'00'                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of ACCARC records'                         
         DC    AL1(RECIARC),XL(CNTL)'00'                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of ACCMST records'                         
         DC    AL1(RECIMST),XL(CNTL)'00'                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of Records on File'                        
         DC    AL1(RECIMST+RECIARC),XL(CNTL)'00'                                
*                                                                               
         DC    CL(L'TOTNAME)'* Total of ACCDIR records'                         
         DC    AL1(RECIDIR),XL(CNTL)'00'                                        
*                                                                               
TOTX     DC    AL1(EOT)                                                         
*                                                                               
RECTAB   DS    0X                                                               
*                                                                               
         DC    CL(L'RECNAME)'ACCMST Header'                                     
         DC    AL1(ACRTHDRA)                                                    
         DC    AL1(RECIFILE+RECIDIR+RECIMST,0,0,0,0,0,0,0)                      
*                                                                               
         DC    CL(L'RECNAME)'ACCARC Header'                                     
         DC    AL1(ACRTHDRB)                                                    
         DC    AL1(RECIFILE+RECIDIR+RECIARC,0,0,0,0,0,0,0)                      
*                                                                               
         DC    CL(L'RECNAME)'ACCMST Trailer'                                    
         DC    AL1(ACRTTRLA)                                                    
         DC    AL1(RECIFILE+RECIDIR+RECIMST,0,0,0,0,0,0,0)                      
*                                                                               
         DC    CL(L'RECNAME)'ACCARC Trailer'                                    
         DC    AL1(ACRTTRLB)                                                    
         DC    AL1(RECIFILE+RECIDIR+RECIARC,0,0,0,0,0,0,0)                      
*                                                                               
         DC    CL(L'RECNAME)'Company'                                           
         DC    AL1(ACRTCPY)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'Unit'                                              
         DC    AL1(ACRTUNT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'Ledger'                                            
         DC    AL1(ACRTLDG)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'High Account'                                      
         DC    AL1(ACRTACTH)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,RSTELQ,0,RSTTDATE-RSTEL,3,0)             
*                                                                               
         DC    CL(L'RECNAME)'Low Account'                                       
         DC    AL1(ACRTACTL)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,RSTELQ,0,RSTTDATE-RSTEL,3,0)             
*                                                                               
         DC    CL(L'RECNAME)'Account Office'                                    
         DC    AL1(ACRTOFA)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'Contra-account Passive'                            
         DC    AL1(ACRTCHDP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'Contra-account Header'                             
         DC    AL1(ACRTCHDH)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'History Bucket'                                    
         DC    AL1(ACRTCAC)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,CACRHMOS-CACKEY,1,0)                 
*                                                                               
         DC    CL(L'RECNAME)'ACCMST Transaction'                                
         DC    AL1(ACRTTRN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,TRNKDATE-TRNKEY,3,0)                 
*&&US                                                                           
         DC    CL(L'RECNAME)'Peeled Transaction'                                
         DC    AL1(ACRTTRN)                                                     
         DC    AL1(RECIMST+RECIDIR+RECIPLD,0,0,0,0,0,0,0)                       
*&&                                                                             
         DC    CL(L'RECNAME)'Time Management   '                                
         DC    AL1(ACRTTIM)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,TIMKPEDT-TIMKEY,3,0)                 
*                                                                               
         DC    CL(L'RECNAME)'Person Time'                                       
         DC    AL1(ACRTTRN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'ACCARC Transaction'                                
         DC    AL1(ACRTTRNA)                                                    
         DC    AL1(RECIDIR+RECIARC,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''01'' Office'                                    
         DC    AL1(ACRTOFF)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''02'' Invoice# Passive'                          
         DC    AL1(ACRTINV)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''03'' New Batch'                                 
         DC    AL1(ACRTNBT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,TBAKBMOS-TBAKEY,2,0)                 
*                                                                               
         DC    CL(L'RECNAME)'X''04'' New Batch Passive'                         
         DC    AL1(ACRTNBP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''05'' Tax Rules'                                 
         DC    AL1(ACRTTAX)                                                     
         DC    AL1(RECIMST+RECIDIR,0,1,0,0,TAXKDATE-TAXKEY,3,0)                 
*                                                                               
         DC    CL(L'RECNAME)'X''06'' Billing Source'                            
         DC    AL1(ACRTBSC)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''07'' Dutch Reconcile Passive'                   
         DC    AL1(ACRTMRH)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''08'' Media Interface'                           
         DC    AL1(ACRTMIN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''09'' Production Media'                          
         DC    AL1(ACRTPMD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''0A'' Analysis Code'                             
         DC    AL1(ACRTWCO)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''0B'' Batch'                                     
         DC    AL1(ACRTBAT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''0C'' Comment'                                   
         DC    AL1(ACRTSCM)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,RSTELQ,0,RSTTDATE-RSTEL,3,0)             
*                                                                               
         DC    CL(L'RECNAME)'X''0F'' Person'                                    
         DC    AL1(ACRTPER)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''10'' Check Authorization'                       
         DC    AL1(ACRTCKA)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''11'' Advertiser'                                
         DC    AL1(ACRTADV)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''11'' Advertiser Passive'                        
         DC    AL1(ACRTACP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''12'' Account Group'                             
         DC    AL1(ACRTAGR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''13'' Account Group Passive'                     
         DC    AL1(ACRTAGP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''14'' Activity Passive'                          
         DC    AL1(ACRTRAP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''15'' Mos Passive'                               
         DC    AL1(ACRTMOS)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''18CA'' Company P&&L Bucket'                     
         DC    AL1(ACRTCPYP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''18CC'' Client P&&L Bucket'                      
         DC    AL1(ACRTCLIP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''18CD'' Direct Time Pointer'                     
         DC    AL1(ACRTDIRP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''19'' Adjustment Rate'                           
         DC    AL1(ACRTPAR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''1A'' Order'                                     
         DC    AL1(ACRTORD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,TRSELQ,0,TRSDATE-TRSEL,1,0)              
*                                                                               
         DC    CL(L'RECNAME)'X''1B'' Budget'                                    
         DC    AL1(ACRTBUD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''1C'' Price List'                                
         DC    AL1(ACRTPRL)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''1C01'' Article Record'                          
         DC    AL1(ACRTART)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''1C11'' Art. Pointer'                            
         DC    AL1(ACRTAPAF)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''1C12'' Art. Sequence'                           
         DC    AL1(ACRTAPAS)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''1D'' List'                                      
         DC    AL1(ACRTLST)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''1E'' Group Invoice'                             
         DC    AL1(ACRTGIN)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''1F'' Old Artist Fee Control'                    
         DC    AL1(ACRTFEEC)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''20'' Old Artist Fee Area'                       
         DC    AL1(ACRTFEEA)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''21'' Old Artist Fee Percent'                    
         DC    AL1(ACRTFEEP)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''22'' Order Reservation'                         
         DC    AL1(ACRTORES)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2401'' AA Payable Passive'                      
         DC    AL1(ACRTVAAP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''2402'' AA Receivable Passive'                   
         DC    AL1(ACRTRAAP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
                                                                                
         DC    CL(L'RECNAME)'X''2700'' Brand Estimate Record'                   
         DC    AL1(ACRTESTR)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,EMDELQ,0,EMDLDT-EMDEL,3,0)               
*                                                                               
         DC    CL(L'RECNAME)'X''2702'' Brand Est. Glob. # ptr'                  
         DC    AL1(ACRTEGNP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''2704'' Brand Est. Amended ptr'                  
         DC    AL1(ACRTEADP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''2705'' MCS Est. Recent action ptr'              
         DC    AL1(ACRTERAP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''28'' GL Bucket/Pointers'                        
         DC    AL1(ACRTGLBP)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,GLBRUPDT-GLBKEY,1,0)                 
*                                                                               
         DC    CL(L'RECNAME)'X''29'' Charge Rate (TMS)'                         
         DC    AL1(ACRTPCRT)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2A'' Charge Rate'                               
         DC    AL1(ACRTPCR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C02'' Office Group'                            
         DC    AL1(ACRTOGRG)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C04'' Office'                                  
         DC    AL1(ACRTOGRO)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C06'' Media Group'                             
         DC    AL1(ACRTMGR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C08'' Workcode Group'                          
         DC    AL1(ACRTWGR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C10'' User Field'                              
         DC    AL1(ACRTUFS)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C12'' Job Groups'                              
         DC    AL1(ACRTJGR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C20'' Options'                                 
         DC    AL1(ACRTPOP)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,PACELQ,0,PACDATE-PACEL,3,0)              
*                                                                               
         DC    CL(L'RECNAME)'X''2C22'' Auto Job Number'                         
         DC    AL1(ACRTAJN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C23'' Studio Type'                             
         DC    AL1(ACRTSTU)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C24'' Unit Pricing'                            
         DC    AL1(ACRTPRC)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C30'' Scheme Header'                           
         DC    AL1(ACRTSCH)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C32'' Category'                                
         DC    AL1(ACRTCAT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C34'' Panel'                                   
         DC    AL1(ACRTPAN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C36'' Estimate Version'                        
         DC    AL1(ACRTEVE)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,EUPELQ,0,EUPLAST-EUPEL,3,0)              
*                                                                               
         DC    CL(L'RECNAME)'X''2C38'' Text'                                    
         DC    AL1(ACRTTXT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C3A'' Group Bill'                              
         DC    AL1(ACRTGRB)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C3C'' Session Estimate'                        
         DC    AL1(ACRTSES)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C3E'' Job Cycle Bill'                          
         DC    AL1(ACRTJCB)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C40'' Project Date'                            
         DC    AL1(ACRTDAT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2C42'' Time Sheet List'                         
         DC    AL1(ACRTTSL)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D01'' Sales Tax'                               
         DC    AL1(ACRTSUT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D02'' Scribe Format'                           
         DC    AL1(ACRTRES)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D03'' Intagy Estimate'                         
         DC    AL1(ACRTINT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D04'' Intagy Journal Passive'                  
         DC    AL1(ACRTIDJ)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''2D05'' Interest Rate'                           
         DC    AL1(ACRTRAT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D06'' Scribe Keyword'                          
         DC    AL1(ACRTKWD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D07'' APG information'                         
         DC    AL1(ACRTAPG)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D08'' Bank Record'                             
         DC    AL1(ACRTBANK)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D09'' SAP Voucher'                             
         DC    AL1(ACRTSAPK)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2D40'' Date Scheme'                             
         DC    AL1(ACRTDAT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2E'' Split Billing'                             
         DC    AL1(ACRTSBL)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''2F00'' Media Detail'                            
         DC    AL1(ACRTMPD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,MPDKPER-MPDKEY,4,0)                  
*                                                                               
         DC    CL(L'RECNAME)'X''2F01'' Media Rules'                             
         DC    AL1(ACRTMPR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,MBTELQ,0,MBTCHNG-MBTEL,5,0)              
*                                                                               
         DC    CL(L'RECNAME)'X''30'' Office/Account Passive'                    
         DC    AL1(ACRTOAP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''31'' Acct/Name Change Passive'                  
         DC    AL1(ACRTANC)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''32'' Name Search Passive'                       
         DC    AL1(ACRTSRC)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3300'' Prod. Bill Control'                      
         DC    AL1(ACRTPBC)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3301'' Active Prod. Bills'                      
         DC    AL1(ACRTPBA)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3302'' Passive Prod. Bills'                     
         DC    AL1(ACRTPBP)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3303'' Prod. Bill Section def'                  
         DC    AL1(ACRTPBS)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3304'' PC Billing format'                       
         DC    AL1(ACRTBFM)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3305'' PC Billing edit'                         
         DC    AL1(ACRTBED)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''34'' Prod. Trans. Activity'                     
         DC    AL1(ACRTPTA)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3502'' Studio PO Passive'                       
         DC    AL1(ACRTSPO)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3504'' Agency PO Passive'                       
         DC    AL1(ACRTAPO)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''36'' Filter Name/Value'                         
         DC    AL1(ACRTRSF)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3700'' Expenditure Type'                        
         DC    AL1(ACRTETYR)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3701'' Expenditure Category'                    
         DC    AL1(ACRTETCT)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3702'' Appr. Limit Records'                     
         DC    AL1(ACRTALIR)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3703'' Creditor Approver Passive'               
         DC    AL1(ACRTCETP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3704'' Approver Records'                        
         DC    AL1(ACRTAPPR)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3706'' Order Req # Passive'                     
         DC    AL1(ACRTORNP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3707'' Est Rep Format Rec'                      
         DC    AL1(ACRTERFR)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3708'' Limit List Passive'                      
         DC    AL1(ACRTLMLP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3708'' Limit List Records'                      
         DC    AL1(ACRTLIML)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3709'' Draft A/C Passives'                      
         DC    AL1(ACRTDRAP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''370A'' Group List Records'                      
         DC    AL1(ACRTGRPL)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''370C'' Xdata Records'                           
         DC    AL1(ACRTXDAT)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''370D'' Format Passives'                         
         DC    AL1(ACRTFPAS)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''370E'' Approver Passives'                       
         DC    AL1(ACRTAPPP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''370F'' Job Approver Passives'                   
         DC    AL1(ACRTJOBA)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3710'' Order # Alloc. Recs.'                    
         DC    AL1(ACRTONAR)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3712'' Order # Alloc. Pass.'                    
         DC    AL1(ACRTONAP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3714'' Dept Approver Passives'                  
         DC    AL1(ACRTDPTA)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3716'' Non-Cli Approver Pass.'                  
         DC    AL1(ACRTNCLI)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3718'' Expense Claim Records'                   
         DC    AL1(ACRTEXPC)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3719'' Expense Status Passives'                 
         DC    AL1(ACRTEXPS)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''371A'' Expense Date Passives'                   
         DC    AL1(ACRTEXDP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''371B'' Expense SJ Passives'                     
         DC    AL1(ACRTEXJP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3720'' Expense Number Passives'                 
         DC    AL1(ACRTEXNP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3722'' Supplier Appr. Passives'                 
         DC    AL1(ACRTSUPP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3724'' Order SJ Passives'                       
         DC    AL1(ACRTOSJP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3725'' Audit Records'                           
         DC    AL1(ACRTAUDT)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3726'' Role Records'                            
         DC    AL1(ACRTROLE)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3727'' Team Records'                            
         DC    AL1(ACRTTEAM)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3729'' Key Stage Records'                       
         DC    AL1(ACRTKYST)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3730'' Template Records'                        
         DC    AL1(ACRTTEMP)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3731'' Pers keystage Passives'                  
         DC    AL1(ACRTPKST)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3731'' Out of Off. Records'                     
         DC    AL1(ACRTOUTO)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3732'' Resource Records'                        
         DC    AL1(ACRTRWK)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3733'' Campaign Passives'                       
         DC    AL1(ACRTCAM)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3734'' Client Campaign Pass '                   
         DC    AL1(ACRTCLCM)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3735'' XDF Record Passives'                     
         DC    AL1(ACRTXPAS)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3736'' XDl Record '                             
         DC    AL1(ACRTXDL)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3737'' Office List Passives'                    
         DC    AL1(ACRTOFL)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3721'' Master Job Passives'                     
         DC    AL1(ACRTMJB)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3738'' Order Supplier Passives'                 
         DC    AL1(ACRTOSU)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3739'' Order GAP Sent Passives'                 
         DC    AL1(ACRTOGA)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3740'' Job Date Passive '                       
         DC    AL1(ACRTJDTP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3741'' Assigned Job Passives'                   
         DC    AL1(ACRTASJP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3742'' Order Status Passives'                   
         DC    AL1(ACRTOSTP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3800'' A/C Extension Records'                   
         DC    AL1(ACRTAEXT)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3803'' Vehicle Record'                          
         DC    AL1(ACRTVEH)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3804'' Fuel Record'                             
         DC    AL1(ACRTFUE)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3805'' Engine Record'                           
         DC    AL1(ACRTENG)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3806'' Distance Record'                         
         DC    AL1(ACRTDIS)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3807'' Distance Rate Record'                    
         DC    AL1(ACRTDRT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3D01'' Account Contract Record'                 
         DC    AL1(ACRTCONT)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3D02'' Account Contract Passive'                
         DC    AL1(ACRTCONP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E01'' Cost Allocation Hist'                    
         DC    AL1(ACRTCAH)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E02'' Cost Allocation Method'                  
         DC    AL1(ACRTCMT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E03'' Cost Payroll Code'                       
         DC    AL1(ACRTPAY)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E05'' Cost Payroll History'                    
         DC    AL1(ACRTPHI)                                                     
         DC    AL1(RECIMST+RECIDIR,0,1,0,0,PHIKMOA-PHIKEY,2,0)                  
*                                                                               
         DC    CL(L'RECNAME)'X''3E07'' Cost Personal Rates'                     
         DC    AL1(ACRTCPR)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E09'' Cost Profile'                            
         DC    AL1(ACRTCAP)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E0A'' Costing Client Profile'                  
         DC    AL1(ACRTCCP)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E0B'' Cost Calendar '                          
         DC    AL1(ACRTCAS)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E0C'' Cost Calendar Passive'                   
         DC    AL1(ACRTCASP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E0D'' Cost Standard Hours'                     
         DC    AL1(ACRTSTD)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E0E'' Time Total         '                     
         DC    AL1(ACRTTTH)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,TTHKEMOA-TTHKEY,2,0)                 
*                                                                               
         DC    CL(L'RECNAME)'X''3E0F'' Timesheet Wkly Passive'                  
         DC    AL1(ACRTTSW)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E10'' Edit Hours            '                  
         DC    AL1(ACRTEDT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E11'' Timesheet Save Record '                  
         DC    AL1(ACRTSSAV)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E12'' Cost Person Id Passive'                  
         DC    AL1(ACRTPID)                                                     
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E13'' Timesheet Tempo X-Ref'                   
         DC    AL1(ACRTTPOX)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,TSXKEND-TSXKEY,3,0)                  
*                                                                               
         DC    CL(L'RECNAME)'X''3E14'' Timesheet Tempo detail'                  
         DC    AL1(ACRTTDT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,1,0,0,TSIKPEDT-TSIKEY,3,0)                 
*                                                                               
         DC    CL(L'RECNAME)'X''3E15'' 1099 Tax info'                           
         DC    AL1(ACRTT99)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E16'' Time Status Passive'                     
         DC    AL1(ACRTTSTA)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E17'' Time Date Passive'                       
         DC    AL1(ACRTTDTE)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E18'' Time account passive'                    
         DC    AL1(ACRTTACT)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E19'' Time Job Passive'                        
         DC    AL1(ACRTTSJT)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3E21'' Time audit records'                      
         DC    AL1(ACRTTAUD)                                                    
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3E22'' Time Template record'                    
         DC    AL1(ACRTTTM)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3F01'' Stored Request'                          
         DC    AL1(ACRTSRM)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,SRMKDTE-SRMKEY,3,0)                  
*                                                                               
         DC    CL(L'RECNAME)'X''3F04'' Authorization'                           
         DC    AL1(ACRTAUT)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3F05'' Funding'                                 
         DC    AL1(ACRTFUN)                                                     
         DC    AL1(RECIMST+RECIDIR,0,0,0,0,0,0,0)                               
*                                                                               
         DC    CL(L'RECNAME)'X''3F03'' Tx Serial Passive'                       
         DC    AL1(ACRTTRSP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    CL(L'RECNAME)'X''3F06'' Ref No Search Passive'                   
         DC    AL1(ACRTRNSP)                                                    
         DC    AL1(RECIDIR,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    20XL(RECTABL)'00'   FOR UNKNOWN RECORD TYPES/EQUATES             
RECTABU  DC    XL(RECTABL)'00'     IN CASE TOO MANY UNKNOWN TYPES               
*                                                                               
RECTABN  EQU   (*-RECTAB)/RECTABL                                               
RECTABX  DC    AL1(EOT)                                                         
***********************************************************************         
         SPACE 1                                                                
CPYTAB   DC    (256*CPYTABL)X'00'                                               
         DS    0X                                                               
CPYTABN  EQU   (*-CPYTAB)/CPYTABL                                               
***********************************************************************         
DUPTABN  DC    F'0'                                                             
                                                                                
CONVERTS DC    PL6'0'                                                           
DELETES  DC    PL6'0'                                                           
LASTKEY  DC    XL(L'ACTKEY)'00'                                                 
*                                                                               
* BINTABLE 1 - UNIT LEDGER TABLE                                                
*                                                                               
         DC    C'**UNLT**'             UNIT LEDGER TABLE                        
UNLTAB   DS    0D                  BINTABLE CONSTANTS FOR UNIT LEDGER           
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(UNLLNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(UNLKLNQ)            KEY LENGTH                               
         DC    AL4(UNLMAX)             MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (UNLMAX*UNLLNQ)XL1      TABLE                                    
*                                                                               
UNLMAX   EQU   10000                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* RECORD TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
RECTABD  DSECT                                                                  
RECNAME  DS    CL30                RECORD NAME                                  
RECTYPE  DS    AL1                 RECORD TYPE                                  
RECINDS1 DS    XL1                 INDICATORS                                   
RECIFILE EQU   X'80'               ADD INTO FILE TOTALS ONLY                    
RECIDIR  EQU   X'40'               ADD INTO ACCDIR TOTAL                        
RECIMST  EQU   X'20'               ADD INTO ACCMST TOTAL                        
RECIARC  EQU   X'10'               ADD INTO ACCARC TOTAL                        
RECIUNK  EQU   X'08'               ADD INTO UNKNOWN TOTAL                       
RECIPLD  EQU   X'04'               PEELED TRANSACTIONS                          
         DS    XL1                 N/D                                          
REC2SFL  DS    XL1                 01=2S COMPLEMENT DATE                        
RECELMC  DS    XL1                 ELEMENT CODE                                 
RECMINL  DS    XL1                 MIN ELEMENT LENGTH                           
RECDOFF  DS    XL1                 DATE OFFSET                                  
RECDTYP  DS    XL1                 DATE FORMAT TYPE(2=YM,3=YMD,4=YYMM)          
RECUNKEY DS    XL1                 FIRST BYTE OF KEY OF UNKNOWN TYPE            
RECTABL  EQU   *-RECTABD                                                        
         SPACE 1                                                                
***********************************************************************         
* COUNTER TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
CNTD     DSECT                                                                  
CNTOUT   DS    XL4                 NUMBER OF RECORDS OUTPUT                     
CNTPUR   DS    XL4                 NUMBER OF RECORDS PURGED                     
CNTDT1   DS    XL4                 0 TO 7 YEARS OLD                             
CNTDT2   DS    XL4                 7 TO 10 YEARS OLD                            
CNTDT3   DS    XL4                 10 TO 15 YEARS OLD                           
CNTDT4   DS    XL4                 > 15 YEARS                                   
RESIZE   DS    PL8                 RECORDS SIZE IN BYTES                        
OLDDATE  DS    PL3                 OLDEST DATE FOR RECORD TYPE                  
CNTL     EQU   *-CNTD                                                           
         SPACE 1                                                                
***********************************************************************         
* UNIT LEDGER BINARY TABLE DSECT                                      *         
***********************************************************************         
         SPACE 1                                                                
UNLD     DSECT                                                                  
UNLCPY   DS    XL1                 COMPANY CODE                                 
UNL      DS    CL2                 UNIT LEDGER                                  
UNLKLNQ  EQU   *-UNLD              LENGTH OF KEY                                
UNLOUT   DS    XL4                 NUMBER OF RECORDS OUTPUT                     
UNLPUR   DS    XL4                 NUMBER OF RECORDS PURGED                     
UNLDT1   DS    XL4                 0 TO 7 YEARS OLD                             
UNLDT2   DS    XL4                 7 TO 10 YEARS OLD                            
UNLDT3   DS    XL4                 10 TO 15 YEARS OLD                           
UNLDT4   DS    XL4                 > 15 YEARS                                   
UNLSIZ   DS    XL4                 RECORDS SIZE IN BYTES                        
UNLDAT   DS    PL3                 OLDEST DATE                                  
UNLLNQ   EQU   *-UNLD              LENGTH OF ENTRY                              
         SPACE 1                                                                
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* COMPANY TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
CPYTABD  DSECT                                                                  
CPYCODE  DS    XL1                 COMPANY CODE                                 
CPYINDS  DS    XL1                 INDICATORS                                   
CPYICNT  EQU   X'80'               RECORD COUNTED FOR THIS COMPANY              
CPYCNTS  DS    (RECTABN)XL(CNTL)   COUNTERS FOR RECORD TYPES                    
CPYTABL  EQU   *-CPYTABD                                                        
***********************************************************************         
* TOTALS TABLE (FOR COMPANY)                                          *         
***********************************************************************         
         SPACE 1                                                                
TOTD     DSECT                                                                  
TOTNAME  DS    CL(L'RECNAME)       TOTAL NAME                                   
TOTTYPE  DS    XL1                 TOTAL TYPE (SEE RECINDS1)                    
TOTCNT   DS    XL(CNTL)            COUNTERS                                     
TOTL     EQU   *-TOTD                                                           
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
APEELSA  DS    A                                                                
COUNT    DS    F                                                                
COUNT2   DS    F                                                                
WORK     DS    XL64                                                             
*                                                                               
         DS    0D                                                               
PLIST    DS    0X                                                               
VREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VCARDS   DS    V                                                                
VPEELDT  DS    A                                                                
VISREC   DS    A                                                                
PLISTL   EQU   *-PLIST                                                          
VLDDEF   DC    V(LDDEFN)                                                        
*                                                                               
SORTIT   DS    C                                                                
UNLWRK   DS    CL(UNLLNQ)                                                       
WIPEIT   DS    C                                                                
WRECTYPE  DS    X                                                               
RECFLAG  DS    X                                                                
BINFLG   DS    CL1                                                              
COMPCODE DS    XL1                                                              
BYTE     DS    X                                                                
HALF     DS    H                                                                
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         ORG   P                   ** PRINT LINE **                             
         DS    CL1                                                              
PBXL     DS    CL1                                                              
         DS    CL1                                                              
PREC     DS    CL30                                                             
PBXC1    DS    CL1                                                              
PINP     DS    CL10                                                             
PBXC2    DS    CL1                                                              
PPUR     DS    CL10                                                             
PBXC3    DS    CL1                                                              
POUT     DS    CL10                                                             
PBXS1    DS    CL1                                                              
PDT1     DS    CL8                                                              
PBXS2    DS    CL1                                                              
PDT2     DS    CL8                                                              
PBXS3    DS    CL1                                                              
PDT3     DS    CL8                                                              
PBXS4    DS    CL1                                                              
PDT4     DS    CL8                                                              
PBXS8    DS    CL1                                                              
PDATE    DS    CL8                                                              
PBXRZ    DS    CL1                                                              
PRESZ    DS    CL15                                                             
PBXR     DS    CL1                                                              
PBXLEN   EQU   *-PBXL                                                           
         SPACE 1                                                                
         ORG   MID2+(PREC-P)                                                    
MFTOTS   DS    0CL12               'FILE TOTALS'                                
MCPY     DS    CL9                 'COMPANY'                                    
         DS    CL1                                                              
MCODE    DS    CL2                 COMPANY CODE                                 
         SPACE 1                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070ACLDXRSIZ 07/24/20'                                      
         END                                                                    
