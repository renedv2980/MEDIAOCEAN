*          DATA SET PPINS00    AT LEVEL 001 AS OF 11/29/12                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 041173.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T41F00A                                                                  
*INCLUDE PPINSPRT                                                               
*INCLUDE PPONLPRT                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41F00- INSERTION ORDERS BASE'                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 06/22/05 NEED TO UNLOCK WHEN FINISH                                      
*                                                                               
* KWAN 06/11/04 CONVERT WRKIO TO LINKIO INTERFACE FOR WEB IO DELIVERY           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41F00   CSECT                                                                  
         LKSVR TYPE=UR,BLOCKS=(LIOBSB2Q,T41FFFD)                                
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 PPINS00X-PPINS00D,T41F00,R6,RR=R9,CLEAR=YES                      
*                                                                               
         BRAS  RE,INITL                                                         
         USING POLWRKD,RC                                                       
         USING T41FFFD,RA                                                       
         LR    R8,RC                                                            
         A     R8,=A(POLWRKX-POLWRKD)                                           
         USING IOWORKD,R8                                                       
*                                                                               
* R4 STILL POINTS TO FAC LIST                                                   
*                                                                               
         MVC   DATCON,36(R4)       EXPANDED FACILITIES LIST                     
         MVC   VCOMFACS,16(R1)     INITL DOES NOT DESTROY R1                    
*                                                                               
         ST    R9,RELO00                                                        
*                                                                               
         LR    R9,R8                                                            
         A     R9,=A(IOWORKX-IOWORKD)                                           
         USING POLFILED,R9                                                      
*                                                                               
         L     RF,=V(IREPORT)                                                   
         A     RF,RELO00                                                        
         ST    RF,REPORT                                                        
*                                                                               
         L     RF,=V(PRINT)                                                     
         A     RF,RELO00                                                        
         ST    RF,PRINT                                                         
*                                                                               
         L     RF,=A(FAXBUF)                                                    
         A     RF,RELO00                                                        
         ST    RF,AFAXBUF                                                       
         ST    RF,AFAXBUF2         2ND FAX BUFF IS NOT USED IN EIO              
*                                                                               
         LR    RF,R8               SINCE ADLIST NOT ADDRESSABLE                 
         AHI   RF,4000                                                          
         USING IOWORKD+4000,RF                                                  
         LA    RE,ADLIST                                                        
         ST    RE,AADLIST                                                       
         DROP  RF                                                               
*                                                                               
         BRAS  RE,INITWKST         INITIALIZE WORKING STORAGE AREAS             
*                                                                               
         ST    R1,SAVER1                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         MVI   ADBSW,0             INIT ADBUYER SWITCH                          
         BRAS  RE,CKADBYER         ANY GLOBBER CALLS?                           
         BNE   IOM0                                                             
*                                                                               
         BRAS  RE,LKIO_GET         LINKIO WILL SET FLDS                         
*                                                                               
         BRAS  RE,S#TB_INI         SERIAL# TABLE INITIALIZATION                 
*                                                                               
         BRAS  RE,LKIO_GET         PREPARE FOR MULTIPLE REPLY RECORDS           
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
IOM0     DS    0H                                                               
         L     R1,SAVER1                                                        
*                                                                               
         XC    SVERRFLD,SVERRFLD   CLEAR ERROR FIELD NUMBER (FOR ADB)           
*                                                                               
         MVI   ROUTE,1             HEADLINE EDIT                                
         BRAS  RE,GETOVLY                                                       
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD              BNE = ERROR ENCOUNTERED                      
*                                                                               
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         BZ    IOM0H                                                            
         GOTOR VT41F05,DMCB,(RC),(RA),('PRCWIO#Q',0)                            
*                                                                               
IOM0H    CLI   NEWKSW,C'Y'         TEST KEY FIELD CHANGED                       
         BE    IOM2                YES - START FRESH                            
*                                                                               
IOM1     MVC   KEY,SVLSTKEY        RESTART WITH LAST KEY                        
         XC    SVLSTKEY,SVLSTKEY                                                
         OC    KEY,KEY                                                          
         BZ    IOM2                                                             
*                                                                               
         GOTOR HIGH                                                             
         B     IOM3                                                             
*                                                                               
IOM2     XC    KEY,KEY                                                          
         XC    SCPSCOM1,SCPSCOM1   FIRST TIME - CLEAR SAVED                     
         XC    SCPSCOM2,SCPSCOM2   STANDARD COMMENTS                            
         XC    CPSCOM1,CPSCOM1                                                  
         XC    CPSCOM2,CPSCOM2                                                  
         MVI   LANG,0                                                           
         MVI   SVLANG,0                                                         
*                                                                               
IOM3     L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R4,DMCB             ADDR OF GETFACT'S WORKING STORAGE            
         USING FACTSD,R4                                                        
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO                                                    
         M     R2,=F'90'           USE 90% OF MAX                               
         D     R2,=F'100'                                                       
*                                                                               
         STH   R3,SVMAXIO          USE 90% OF MAX                               
         DROP  RF,R4                                                            
*                                                                               
IOM4     DS    0H                                                               
         XC    SVERRFLD,SVERRFLD   CLEAR ERROR FIELD NUMBER (FOR ADB)           
*                                                                               
         MVI   ROUTE,2                                                          
         BRAS  RE,GETOVLY                                                       
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
         OC    ERR(2),ERR          ERROR ENCOUNTERED?                           
         BZ    IOM4C                                                            
         OI    PRQUIDH+6,OI1C      INSERT CURSOR                                
         SR    R0,R0                                                            
         ICM   R0,3,ERR            ERROR NUMBER                                 
         BRAS  RE,GET_ETXT                                                      
         B     EXXMOD                                                           
*                                                                               
IOM4C    CLI   ERRAREA,0           ON ERROR SET END OF FILE                     
         BE    *+8                                                              
IOM4D    MVI   PBUYKEY,X'FF'                                                    
*                                  LAST BUY MUST SET LAST COMMENTS              
         CLI   PBUYKEY,X'FF'                                                    
         BNE   IOM4J                                                            
         MVC   SCPSCOM1,CPSCOM1                                                 
         MVC   SCPSCOM2,CPSCOM2                                                 
         MVC   SVLANG,LANG                                                      
*                                                                               
IOM4J    MVI   ROUTE,X'10'                                                      
         BRAS  RE,GETOVLY                                                       
         L     RF,DMCB                                                          
         LA    R4,WORK                                                          
         L     R1,0(RF)            A(*) IN T41F10                               
         LA    RF,4(RF)                                                         
*                                                                               
IOM5     DS    0H                                                               
         L     R0,0(RF)                                                         
         A     R0,DMCB                                                          
         SR    R0,R1               MUST SUBTRACT A(*) IN T41F10                 
         ST    R0,0(R4)                                                         
         LA    R4,4(R4)                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   IOM5                                                             
*                                                                               
         MVC   APINSOR(08),WORK    APINSOR AND AIOPRNT                          
*                                                                               
         L     RF,=A(LOAD12)       FMTBUY                                       
         A     RF,RELO00                                                        
         ST    RF,AFMTBUY                                                       
*                                                                               
         L     RF,=A(LOAD11)       LOADER FOR OTHER ROUTINES                    
         A     RF,RELO00                                                        
         ST    RF,ASTDCOM                                                       
         ST    RF,ACOMLIN                                                       
         ST    RF,AMATCOM                                                       
         ST    RF,ABLDREV                                                       
         MVI   ACOMLIN,4           SET BRANCH DISP                              
         MVI   AMATCOM,8                                                        
*                                                                               
         MVI   ABLDREV,12                                                       
         LA    RF,WFMOD            SET DUMMY WFMOD AND ERRMOD                   
         ST    RF,AWFMOD                                                        
         ST    RF,AERRMOD                                                       
         B     *+6                                                              
WFMOD    BR    RE                                                               
*                                                                               
         LA    RF,RECPOOL                                                       
         ST    RF,ARECPOOL                                                      
*                                                                               
         LR    RF,R8               TO ADDRESS COMTAB AND BUYMAX                 
         AHI   RF,4000                                                          
         USING IOWORKD+4000,RF                                                  
         LA    RE,COMTAB                                                        
         ST    RE,ACOMTAB                                                       
         LA    RE,BUYMAX                                                        
         ST    RE,ABUYMAX                                                       
         DROP  RF                                                               
*                                                                               
         L     R5,BUYDALST                                                      
         AHI   R5,-4                                                            
         L     R5,0(R5)            SAVE DA OF LAST BUY                          
*                                                                               
         GOTOR APINSOR,DMCB,(RC)                                                
*                                                                               
         CLI   ERRAREA,0                                                        
         BNE   EXXMODX                                                          
         OC    ERR(2),ERR                                                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PBUYREC,X'FF'                                                    
         BE    IOM7                                                             
         L     R4,BUYDALST                                                      
         AHI   R4,-4                                                            
         C     R5,0(R4)            TEST DA OF LAST BUY                          
         BE    IOM7                                                             
*                                                                               
         L     RF,INSCNT           COUNT OF INSERTIONS                          
         LA    RF,1(RF)                                                         
         ST    RF,INSCNT                                                        
*                                                                               
IOM7     DS    0H                                                               
         CLC   REFNO,PAGYIONO                                                   
         BE    IOM8                                                             
         L     RF,IOCNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,IOCNT                                                         
         MVC   PAGYIONO,REFNO                                                   
*                                                                               
         CLI   PBUYKEY,X'FF'                                                    
         BE    IOM8C               SEE IF DONE                                  
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,0,0,0                                              
         L     R4,DMCB             USE GETFACT'S RETURNED WORKAREA              
         USING FACTSD,R4                                                        
*                                                                               
         CLC   FATIOCNT,SVMAXIO    TEST REACHED LIMIT                           
         BL    IOM8                NO - CONTINUE                                
*                                                                               
* SETS PBUYREC TO X'FF'                                                         
*                                                                               
IOM7H    MVC   SVLSTKEY,KEY                                                     
         B     IOM4D               MUST GO FINISH THIS I/O                      
*                                                                               
         DROP  R4,RF                                                            
*                                                                               
IOM8     DS    0H                                                               
         CLI   PBUYKEY,X'FF'                                                    
         BNE   IOM4                GET NEXT BUY                                 
*                                                                               
* END OF BUYS                                                                   
*                                                                               
IOM8C    EDIT  INSCNT,(4,PRQLN1)                                                
         OI    PRQLN1+3,X'F0'                                                   
*                                                                               
         MVC   PRQLN1+5(10),=C'INSERTIONS'                                      
         CHI   R0,1                                                             
         BNE   *+8                                                              
         MVI   PRQLN1+14,C' '                                                   
         EDIT  IOCNT,(4,PRQLN2)                                                 
         OI    PRQLN2+3,X'F0'                                                   
*                                                                               
         MVC   PRQLN2+5(11),=C'INS. ORDERS'                                     
         CHI   R0,1                                                             
         BNE   *+8                                                              
         MVI   PRQLN2+15,C' '                                                   
         FOUT  PRQLN1H                                                          
         FOUT  PRQLN2H                                                          
*                                                                               
         XC    PRQLN3,PRQLN3                                                    
         LTR   R0,R0                                                            
         BZ    IOM9                NO IO'S                                      
*                                                                               
         MVC   PRQLN3+5(12),=C'REPORT NAME='                                    
         MVC   PRQLN3+18(8),RCJOB                                               
         L     RF,VTIA                                                          
         CLC   0(5,RF),=C'START'                                                
         BE    IOM9                                                             
         USING UKRECD,RF                                                        
         MVC   PRQLN3+28(3),UKSUBID                                             
         MVI   PRQLN3+31,C','                                                   
         EDIT  (B2,UKREPNO),(4,PRQLN3+32),ALIGN=LEFT                            
         CLI   QOPT7,C'F'      SEE IF FAXING                                    
         BNE   IOM9                                                             
*                                                                               
         L     RF,AFAXBUF                                                       
         MVC   PRQLN3+37(3),UKSUBID                                             
         MVI   PRQLN3+40,C','                                                   
         EDIT  (B2,UKREPNO),(4,PRQLN3+41),ALIGN=LEFT                            
*                                                                               
         OC    SECONDFX,SECONDFX   SECOND FAX PRESENT?                          
         BZ    IOM9                                                             
         L     RF,AFAXBUF2                                                      
         MVC   PRQLN3+46(3),UKSUBID                                             
         MVI   PRQLN3+49,C','                                                   
         EDIT  (B2,UKREPNO),(4,PRQLN3+50),ALIGN=LEFT                            
*                                                                               
         DROP   RF                                                              
*                                                                               
IOM9     DS    0H                                                               
         FOUT  PRQLN3H             END PRINTING                                 
*                                                                               
         L     RF,VTIA                                                          
         CLC   0(5,RF),=C'START'                                                
         BE    IOM10                                                            
         GOTO1 PRINT,DMCB,=X'FFFF',,VTIA,VDATAMGR                               
*                                                                               
         CLI   QOPT7,C'F'                                                       
         BNE   IOM10                                                            
         GOTO1 PRINT,DMCB,=X'FFFF',,AFAXBUF,VDATAMGR                            
*                                                                               
         OC    SECONDFX,SECONDFX   SECOND FAX PRESENT?                          
         BZ    IOM10                                                            
         GOTO1 PRINT,DMCB,=X'FFFF',,AFAXBUF2,VDATAMGR                           
*                                                                               
IOM10    DS    0H                                                               
         CLI   RCWRITE,C'N'        SEE IF 'TEST' RUN                            
         BE    IOM13               YES - DON'T WRITE BACK AGENCY HEADER         
         LA    R0,PAGYREC                                                       
         ST    R0,AREC                                                          
         MVC   KEY+27(4),AGYDA                                                  
         GOTOR GETPRT                                                           
         MVC   PAGYIONO,REFNO                                                   
         MVC   PAGYIODT,BTODAY                                                  
         GOTOR PUTPRT                                                           
*                                                                               
         CLI   ERRAREA,0                                                        
         BNE   EXXMODX                                                          
*                                                                               
IOM13    OC    SVLSTKEY,SVLSTKEY   SEE IF CONTINUATION PENDING                  
         BZ    IOM15                                                            
         LHI   R0,PENTERCP         PRESS ENTER TO CONTINUE PROCESSING           
         BRAS  RE,GET_ITXT                                                      
         OI    PRQUIDH+1,X'01'     MODIFY REQ ID FOR NEXT INPUT                 
         FOUT  PRQUIDH                                                          
         B     IOM17                                                            
*                                                                               
IOM15    LHI   R0,PRCDENXR         REQUEST PROCESSED, ENTER NEXT REQ            
         BRAS  RE,GET_ITXT                                                      
IOM17    FOUT  PRQMSGH                                                          
*                                                                               
         LA    R2,PRQMEDH                                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TM    ADBSW,AS_ADBRQ      ADBUYER REQUEST?                             
         JZ    EXXMODX             NO                                           
*                                                                               
* NEED TO BUILD REPLY DATA                                                      
*                                                                               
IOM50    L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         BRAS  RE,RPYTRIAL         REPLY WEB IO TRAILER DATA                    
*                                                                               
         LHI   RF,E#INSORD         DEFAULT TO NONE WEB IO REPLY                 
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         BZ    *+8                                                              
         LHI   RF,E#IO2FIN                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(RF))                  
*                                                                               
         OC    SVLSTKEY,SVLSTKEY   CONTINUATION PENDING?                        
         BZ    IOM60                                                            
         XC    SVLSTKEY,SVLSTKEY                                                
         LHI   R0,INCOMIMD         INCOMPLETE, INSERTIONS MARKED                
         BRAS  RE,GET_ITXT         GET INFORMATIONAL MSG                        
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRDSC),    +        
               ('LD_CHARQ',PRQMSG),(L'PRQMSG,0)                                 
*                                                                               
IOM60    TM    ADBSW,AS_WEBIO      WEB IO?                                      
         BO    *+12                                                             
         BRAS  RE,REGREPLY         DO REGULAR REPLY                             
         B     IOM66                                                            
*********GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INS),                
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TOTINS),    +        
               ('LD_UBINQ',NUMPRCIN),(L'NUMPRCIN,0)                             
*                                                                               
IOM66    L     R4,ASER#TAB         POINT TO SERIAL# TABLE                       
         USING SER#TABD,R4                                                      
         SR    R2,R2                                                            
         ICM   R2,3,NUMSER#S       NUMBER OF SERIAL#S IN TABLE                  
IOM68    CLI   S#STATUS,S#NOTU_Q   SERIAL# NOT USED IN INSERTION ORDER?         
         BNE   IOM68H                                                           
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#BYSER),     +        
               ('LD_SPAKQ',S#SERIAL),(L'S#SERIAL,0)                             
IOM68H   LA    R4,SER#TBLQ(R4)     POINT TO NEXT ENTRY IN TABLE                 
         BCT   R2,IOM68                                                         
*                                                                               
IOM70    DS    0H                                                               
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         BZ    IOM90               NO                                           
         OC    NUMPRCIN,NUMPRCIN   ANY INSERTION ORDER PROCESSED?               
         BZ    IOM90                                                            
         GOTOR VT41F05,DMCB,(RC),(RA),('PUTWIO#Q',0)                            
*                                                                               
IOM90    DS    0H                                                               
         J     ALLDONE                                                          
         DROP  R3,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET_ITXT ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    PRQMSG,PRQMSG                                                    
         L     RF,VCOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R0),0,(C'I',DMCB),0,0,0                            
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
GET_ETXT ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    PRQMSG,PRQMSG                                                    
         L     RF,VCOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R0),0,(C'E',DMCB),0,0,0                            
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
LKIO_GET ST    RE,FULL                                                          
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)                                   
         L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
GETOVLY  NTR1                                                                   
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB(1),ROUTE                                                    
         GOTO1 VCALLOV,DMCB,,(RA)                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE POLINITL1                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VREAD    LA    RF,DMREAD                                                        
         MVC   KEYSAVE,KEY                                                      
         J     DIRCTRY                                                          
*                                                                               
VSEQ     LA    RF,DMRSEQ                                                        
         J     DIRCTRY                                                          
*                                                                               
VHIGH    LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         J     DIRCTRY                                                          
*                                                                               
VADD     LA    RF,DMADD                                                         
         J     DIRCTRY                                                          
*                                                                               
VWRITE   LA    RF,DMWRT                                                         
*                                                                               
DIRCTRY  NTR1                                                                   
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
         J     DMCHECK                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VGETPRT  LA    RF,GETREC                                                        
         J     FILE                                                             
*                                                                               
VPUTPRT  LA    RF,PUTREC                                                        
         J     FILE                                                             
*                                                                               
VADDPRT  LA    RF,ADDREC                                                        
         J     FILE                                                             
*                                                                               
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   0(RF),C'A'                                                       
         JNE   *+8                                                              
         LA    R2,KEY                                                           
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PRTFILE,(R2),AREC,DMWORK                           
         J     DMCHECK                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBDIR)                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VREADPB  LA    RF,DMREAD                                                        
         MVC   KEYSAVE,KEY                                                      
         J     PUBDIRY                                                          
*                                                                               
VSEQPB   LA    RF,DMRSEQ                                                        
         J     PUBDIRY                                                          
*                                                                               
VHIGHPB  LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         J     PUBDIRY                                                          
*                                                                               
VWRIPB   LA    RF,DMWRT                                                         
         J     PUBDIRY                                                          
*                                                                               
VADDPB   LA    RF,DMADD                                                         
         J     PUBDIRY                                                          
*                                                                               
PUBDIRY  NTR                                                                    
*                                                                               
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PUBDIR,KEY,KEY                                     
         J     DMCHECK                                                          
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBFILE)                                     
*                                                                               
VGETPUB  LA    RF,GETREC                                                        
         J     PUBFIL                                                           
*                                                                               
VPUTPUB  LA    RF,PUTREC                                                        
         J     PUBFIL                                                           
*                                                                               
VADDPUB  LA    RF,ADDREC                                                        
         J     PUBFIL                                                           
*                                                                               
PUBFIL   NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   0(RF),C'A'                                                       
         JNE   *+8                                                              
         LA    R2,KEY                                                           
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PUBFILE,(R2),AREC,DMWORK                           
         J     DMCHECK                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         JNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3               LET GETMSG SORT IT OUT                       
         J     ERROR                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
         OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
         J     EXXMODX                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EXXMOD   DS    0H                                                               
         TM    ADBSW,AS_ADBRQ      ADBUYER REQUEST?                             
         JZ    EXXMODX             NO                                           
*                                                                               
* BUILD ERROR REPLY                                                             
*                                                                               
         OC    SVERRFLD,SVERRFLD   ERROR IN DETECTED?                           
         BNZ   *+12                                                             
         LHI   R0,D#REQID                                                       
         STH   R0,SVERRFLD         DEFAULT TO REPORT ID                         
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         LHI   RF,E#INSOER         DEFAULT TO NONE WEB IO REPLY                 
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         BZ    *+8                                                              
         LHI   RF,E#IO2ERR                                                      
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(RF))                  
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRNUM),    +        
               ('LD_UBINQ',SVERRFLD),(L'SVERRFLD,0)                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRDSC),    +        
               ('LD_CHARQ',PRQMSG),(L'PRQMSG,0)                                 
*                                                                               
ALLDONE  GOTOR ALINKIO,DMCB,('LIOACLO',LIOBD)                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
EXXMODX  MVI   DATALKSW,0          RESET DATA LOCKED SWITCH                     
         BRAS  RE,UNLOCK           UNLOCK KEY SET IN THIS APPLICATION           
*                                                                               
         XMOD1 1                                                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITWKST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,VCOMFACS                                                      
         MVC   ALINKIO,CLINKIO-COMFACSD(RF)                                     
*                                                                               
         LR    RF,RC                                                            
         A     RF,=A(WRKRECA-PPINS00D)                                          
         ST    RF,AWRKREC                                                       
*                                                                               
         LR    RF,RC                                                            
         A     RF,=A(WEBIODS-PPINS00D)                                          
         ST    RF,AWIODSST         ADDRESS OF WIO STORAGE BLOCK                 
*                                                                               
         XC    NUMPRCIN,NUMPRCIN   NUMBER OF PROCESSED INSERTIONS               
         MVI   ADRPYREC,0          WEB IO REPLY SWITCHS                         
         MVI   ADRPYSW1,0                                                       
         MVI   ADRPYSW2,0                                                       
         MVI   ADRPYSW3,0                                                       
         XC    SVWIO#,SVWIO#       WEB IO #                                     
         XC    SVW#REV#,SVW#REV#   WEB IO REVISION #                            
         XC    WIODISKA,WIODISKA   WEB IO MASTER RECORD DISK ADDRESS            
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CXSORT-COMFACSD)(RF)                                         
         ST    RF,XSORT                                                         
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CGLOBBER-COMFACSD)(RF)                                       
         ST    RF,VGLOBBER                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CCUREDIT-COMFACSD)(RF)                                       
         ST    RF,VCUREDIT                                                      
*                                                                               
         MVC   FULL,=X'D9000AAB'                                                
         BRAS  RE,GETCORES                                                      
         MVC   GETINS,DMCB         STORE GETINS ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000AB8'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APUBVAL,DMCB        STORE PUBVAL ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000AB9'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APUBEDIT,DMCB       STORE PUBEDIT ADDRESS                        
*                                                                               
         MVC   FULL,=X'D9000ABA'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APPGETCG,DMCB       STORE PPGETCG ADDRESS                        
*                                                                               
         MVC   FULL,=X'D9000ABB'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APPGETAD,DMCB       STORE PPGETADR ADDRESS                       
*                                                                               
         GOTOR VCALLOV,DMCB,(5,0),(RA)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VT41F05,0(R1)                                                    
*                                                                               
* SET UP LIST OF COMMON ROUTINES ALL ARE LOCATED IN THIS MODULE                 
*                                                                               
         LA    R2,VCOMMON          COMMON ENTRY POINT                           
         SR    R3,R3               INIT ROUTINE ID                              
         LA    R4,SYSCOMM          START OF ADDRESS AVEAREA                     
         LA    R5,VCOUNT           SET NUMBER OF ROUTINES                       
*                                                                               
INIWK60  ST    R2,0(R4)            SET ENTRY POINT                              
         STC   R3,0(R4)            SET ROUTINE ID                               
         LA    R3,4(R3)            BUMP ROUTINE ID                              
         LA    R4,4(R4)            BUMP TO NEXT ADDRESS SAVEAREA                
         BCT   R5,INIWK60                                                       
*                                                                               
         XC    SVIORPID,SVIORPID   PASSWORD ID NUMBER CLEARED                   
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTOR CGETFACT,DMCB,(2,0),0,0                                          
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SVSECAGY,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
         TM    FATFLAG,X'08'       CHECK IF SECET CODE IS THERE                 
         BZ    *+10                                                             
         MVC   SVIORPID,FAPASSWD   SAVE PASSWORD ID NUMBER                      
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RF,R1                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCORES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),FULL      CORE-RESIDENT PHASE TO BE CALLED             
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                 RETURN ADDRESS IN DMCB                      
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHECK FOR CALL FROM LINK TO INSERTION ORDER                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKADBYER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    VGLOBBER,VGLOBBER   MUST HAVE GLOBBER ADDRESS                    
         BZ    CKCALLER            RETURN NO CALLS                              
*                                                                               
         XC    GLOBWORK,GLOBWORK                                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',GLOBWORK,GLVXLENQ,GLVXCTL                 
         CLI   DMCB+8,GLEGNF                                                    
         BE    CKCALLER            RETURN NO CALLS                              
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GLOBWORK(12),=C'PRILINPRIIO2'                                    
         BNE   CKCALLER                                                         
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         XC    LIOBD(LIOBVIS-LIOBD),LIOBD                                       
         LA    R0,LIOBD+L'LIOB                                                  
         ST    R0,LIOBAREC                                                      
         AHI   R0,8000             MAX SIZE FOR LINKIO REC USES                 
         ST    R0,LIOBABUF                                                      
         MVC   LIOBACOM,VCOMFACS                                                
         LA    RF,MAP                                                           
         STCM  RF,15,LIOBAMAP                                                   
         MVI   LIOBMSYS,4          PRINT SYSTEM MSGS                            
         LA    R0,T41FFFD                                                       
         ST    R0,LIOBASB2                                                      
         OI    LIOBINDS,LIOBIMLT+LIOBINRM                                       
         XC    LIOBRECL,LIOBRECL   DEFAULT TO SIZE OF MAX BUFFER                
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAINI',LIOBD)                                   
         BNE   CKCALLER                                                         
         OI    ADBSW,AS_ADBRQ      INDICATE CALLED BY ADBUYER                   
         OI    ADBSW,AS_WEBIO      DELIVERING IO VIA WEB                        
*                                                                               
CKCALLX  J     SETCCEQ             EQUAL                                        
*                                                                               
CKCALLER J     SETCCNEQ            NOT EQUAL (NO CALLS FROM DDLINK)             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MAP      DC    0XL(LIORL)                                                       
         DC    AL2(M#ULIOR2,E#IO2HDR,INSORDR-MAP)                               
MAPX     DC    AL2(0)                                                           
*                                                                               
       ++INCLUDE PPMAPINS          MAP CODES FOR INSERTION ORDER                
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* INITIALIZE TABLE OF SERIAL#S FROM INSERTION ORDER REQUEST                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
S#TB_INI NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   FULL,ASER#TAB       SAVE DATA INDEX TO SERIAL#S                  
         OC    FULL,FULL                                                        
         BNZ   *+6                                                              
         DC    H'0'                ADDRESS TO SERIAL# ELEM                      
*                                                                               
         LR    R0,RC                                                            
         A     R0,=A(SER#TAB-PPINS00D)                                          
         ST    R0,ASER#TAB                                                      
         LHI   R1,SER#TABL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,FULL             POINT TO DATA INDEX                          
         SR    RF,RF                                                            
         ICM   RF,3,6(RE)          NUMBER OF SERIAL#S                           
         CHI   RF,MAXSER#Q                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    RE,8(RE)            POINT TO 1ST SERIAL#                         
*                                                                               
         L     R1,ASER#TAB                                                      
         USING SER#TABD,R1                                                      
         STCM  RF,3,NUMSER#S       SAVE NUMBER OF SERIAL#S IN TABLE             
*                                                                               
S#TB30   MVI   S#STATUS,S#NOTU_Q   INIT TO NOT USED                             
         MVI   S#MODCOD,0          NO MODIDICATION CODE YET                     
         XC    S#PRCCNT,S#PRCCNT   INIT ORDER OF PROCESSED COUNTER              
         MVC   S#SERIAL,0(RE)      PLACE SERIAL# IN TABLE                       
         LA    RE,5(RE)            POINT TO NEXT INPUT SERIAL#                  
         LA    R1,SER#TBLQ(R1)     POINT TO NEXT BLANK ENTRY IN TABLE           
         BCT   RF,S#TB30           LOOP FOR MORE                                
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R1                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMON ENTRY POINT FOR GENERAL SYSTEM ROUTINES                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VCOMMON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SRL   RF,24               SHIFT ROUTINE ID TO RIGHT NYBBLE             
         L     RF,VBRANCH(RF)      GET A(ROUTINE)                               
         A     RF,RELO00           RELOCATE ADDRESS                             
*                                                                               
         BASR  RE,RF               GO TO ROUTINE                                
*                                                                               
VCOMMONX J     EXIT                                                             
*                                                                               
* COMMON ROUTINE ADDRESSES                                                      
*                                                                               
VBRANCH  DS    0D                  ALIGNMENT                                    
         DC    A(VREAD)                                                         
         DC    A(VSEQ)                                                          
         DC    A(VHIGH)                                                         
         DC    A(VADD)                                                          
         DC    A(VWRITE)                                                        
         DC    A(VGETPRT)                                                       
         DC    A(VPUTPRT)                                                       
         DC    A(VADDPRT)                                                       
         DC    A(VREADPB)                                                       
         DC    A(VSEQPB)                                                        
         DC    A(VHIGHPB)                                                       
         DC    A(VADDPB)                                                        
         DC    A(VWRIPB)                                                        
         DC    A(VGETPUB)                                                       
         DC    A(VPUTPUB)                                                       
         DC    A(VADDPUB)                                                       
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REGREPLY NTR1  BASE=*,LABEL=*      NOT WEB IO, REGULAR REPLY                    
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INS),       +        
               ('LD_CHARQ',PRQLN1),(4,0)                                        
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOS),       +        
               ('LD_CHARQ',PRQLN2),(4,0)                                        
*                                                                               
         CLI   PRQLN3+18,C' '      REPORT NAME?                                 
         BNH   REGRPYX             NO - SKIP TO SERIAL#S ELEMENT                
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#RPTNM),     +        
               ('LD_CHARQ',PRQLN3+18),(8,0)                                     
*                                                                               
         CLI   PRQLN3+28,C' '      REPORT ID?                                   
         BNH   REGRPYX             NO - SKIP TO SERIAL#S ELEMENT                
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#RPTID),     +        
               ('LD_CHARQ',PRQLN3+28),(8,0)                                     
*                                                                               
         CLI   PRQLN3+37,C' '      FAX1 ID?                                     
         BNH   REGRPYX             NO - SKIP TO SERIAL#S ELEMENT                
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#FX1ID),     +        
               ('LD_CHARQ',PRQLN3+37),(8,0)                                     
*                                                                               
         CLI   PRQLN3+46,C' '      FAX2 ID?                                     
         BNH   REGRPYX             NO - SKIP TO SERIAL#S ELEMENT                
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#FX2ID),     +        
               ('LD_CHARQ',PRQLN3+46),(8,0)                                     
*                                                                               
REGRPYX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYTRIAL NTR1  BASE=*,LABEL=*      REPLY WEB IO TRAILER DATA                    
*                                                                               
         TM    ADBSW,AS_ADBRQ      ADBUYER REQUEST?                             
         JZ    EXIT                                                             
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         JZ    EXIT                                                             
         OC    NUMPRCIN,NUMPRCIN   ANY INSERTION GOT PROCESSED?                 
         JZ    EXIT                                                             
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         TM    ADRPYREC,RPYTRALQ                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#IO2TRA)              
         OI    ADRPYREC,RPYTRALQ                                                
*                                                                               
         L     R4,AWIODSST         POINT TO WIO STORAGE BLOCK                   
         USING WIODSD,R4                                                        
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOLKEY),    +        
               ('LD_CHARQ',H_IOLKEY),('H_IOLKYL',0)                             
*                                                                               
         CLI   VNOOPT,C'Y'         PROFILE SET FOR CLT VENDOR NUMBER?           
         BNE   RPYTR40                                                          
         OC    SVCVNUM,SPACES                                                   
         CLC   SVCVNUM,SPACES      ANYTHING IN CLIENT VENDOR NUMBER?            
         BE    RPYTR40                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CLTVNU),    +        
               ('LD_CHARQ',SVCVNUM),(L'SVCVNUM,0)                               
*                                                                               
RPYTR40  DS    0H                                                               
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UNLOCK   NTR1  BASE=*,LABEL=*      UNLOCKS LOCKS PUT OUT EARLIER                
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,PRQMED                                                 
         MVC   L.LOCKCLT,PRQCLT                                                 
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   PUB IS ALL?                                  
         BE    UNLK2               YES, GO AHEAD AND UNLOCK CLT LOCK            
         OC    BPUB(4),BPUB        NOTHING IN BASE PUB NUMBER?                  
         BZ    UNLK2               YES, GO AHEAD AND UNLOCK CLT LOCK            
         MVC   L.LOCKRTY,=C'BP'    PUB LOCK                                     
         MVC   L.LOCKPUB,BPUB                                                   
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
UNLK2    L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKUNLKQ',LKUPKEY),VCOMFACS                           
*                                                                               
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    UNLK2                                                            
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,L                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LOAD12   CSECT                                                                  
         NMOD1 0,LOAD12                                                         
         LR    RC,R8               R8 POINTS TO IOWORK                          
         S     RC,=A(POLWRKX-POLWRKD)                                           
         USING POLWRKD,RC                                                       
         CLI   LASTPH,X'12'                                                     
         BE    LD12E                                                            
         MVI   ROUTE,X'12'                                                      
         XC    DMCB,DMCB                                                        
         MVC   DMCB(1),ROUTE                                                    
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   LASTPH,X'12'                                                     
*                                                                               
LD12B    L     RF,DMCB                                                          
         L     R1,0(RF)            A(*) IN T41F12                               
         LA    RF,4(RF)                                                         
         L     R0,0(RF)                                                         
         A     R0,DMCB                                                          
         SR    R0,R1                                                            
         ST    R0,RFMTBUY          SAVE REAL ADDRESS                            
         LA    RF,4(RF)            GET PAST FMTBUY                              
         LA    R4,APPBYOUT         RELOCATE OTHERS                              
LD12D    L     R0,0(RF)                                                         
         A     R0,DMCB                                                          
         SR    R0,R1                                                            
         ST    R0,0(R4)                                                         
         LA    R4,4(R4)                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   LD12D                                                            
*                                                                               
LD12E    GOTOR RFMTBUY,DMCB,(RC)   GOT REAL FMTBUY                              
*                                                                               
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LOAD11   CSECT                                                                  
         NMOD1 0,LOAD11                                                         
         LR    RC,R8               R8 POINTS TO IOWORK                          
         S     RC,=A(POLWRKX-POLWRKD)                                           
         USING POLWRKD,RC                                                       
         ST    RF,FULL             HIGH ORDER BYTE HAS DISP                     
         MVC   DMCB+16(8),DMCB     SAVE PARAMETERS                              
         CLI   LASTPH,X'11'                                                     
         BE    LD11E                                                            
         XC    DMCB(12),DMCB                                                    
         MVI   ROUTE,X'11'                                                      
         MVC   DMCB(1),ROUTE                                                    
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   LASTPH,X'11'                                                     
*                                                                               
         LA    R4,RSTDCOM          SAVE REAL ADDRESSES                          
         L     RF,DMCB                                                          
         L     R1,0(RF)            A(*) IN T41F11                               
         LA    RF,4(RF)                                                         
LD11B    L     R0,0(RF)                                                         
         A     R0,DMCB                                                          
         SR    R0,R1               MUST SUBTRACT A(*) IN T41F11                 
         ST    R0,0(R4)                                                         
         LA    R4,4(R4)                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   LD11B                                                            
*                                                                               
LD11E    LA    R4,RSTDCOM          BRANCH TO REAL ROUTINE                       
         SR    R2,R2                                                            
         IC    R2,FULL             GET DISP                                     
         LA    R4,0(R2,R4)                                                      
         L     RF,0(R4)                                                         
         MVC   DMCB(8),DMCB+16     RESTORE DMCB                                 
         GOTOR (RF),DMCB                                                        
*                                                                               
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FAXBUF   CSECT                                                                  
         DC    14400X'00'                                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPINS00D DSECT                                                                  
*                                                                               
POLWRKQ  EQU   POLWRKX-PPWORK                                                   
         DS    (POLWRKQ)X                                                       
*                                                                               
IOWORKQ  EQU   IOWORKX-IOWORK                                                   
         DS    (IOWORKQ)X                                                       
*                                                                               
POLFILEQ EQU   POLFILEX-POLFILE                                                 
         DS    (POLFILEQ)X                                                      
*                                                                               
PPINSWKQ EQU   PPINSREX-PPINSREC                                                
         DS    (PPINSWKQ)X                                                      
*                                                                               
DUMDUM1Q EQU   DUMDUM1X-DUMDUM1                                                 
         DS    (DUMDUM1Q)X                                                      
*                                                                               
WRKRECA  DS    (WRKRECAL)X                                                      
WRKRECAL EQU   18432+8000                                                       
WRKRECAX EQU   *                                                                
*                                                                               
SER#TAB  DS    (SER#TABL)X                                                      
SER#TABL EQU   MAXSER#Q*SER#TBLQ                                                
SER#TABX EQU   *                                                                
*                                                                               
WEBIODS  DS    (WIODSDLQ)X         WEB IO DATA STREAM STORAGE AREA              
*                                                                               
PPINS00X EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPINSWRK1                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE POLFILE                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPINSWRK2                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        DSECT FOR TRANSFER CONTROLS                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBAL VARIABLES                   
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001PPINS00   11/29/12'                                      
         END                                                                    
