*          DATA SET DDTEMPREAD AT LEVEL 095 AS OF 04/18/01                      
*PHASE TEMPREAA TEMPREAD                                                        
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE FATABOFF                                                               
         SPACE 2                                                                
         TITLE 'STATREAD - PRINT TWA''S OF HEAVY USERS'                         
         PRINT NOGEN                                                            
STATREAD CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKX-WORKD,STATREAD,=V(REGSAVE),RA,CLEAR=YES                    
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         L     R1,=A(TBLOCK-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,ATBLOCK                                                       
         L     R1,=A(ADRBUFF-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AADRBUFF                                                      
*                                                                               
         BAS   RE,INIT             INIT + READ CARDS                            
         BAS   RE,OPENFLS          OPEN FILES                                   
         BAS   RE,MAIN                                                          
         BAS   RE,CLOSE            CLOSE FILES                                  
XBASE    XBASE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
         LA    R1,TITLE1           PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         L     R3,AADRBUFF                                                      
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT050                                                          
         MVC   PRL(80),0(R3)                                                    
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BNE   ERR1                NEQ MEANS INVALID KEYWORD                    
         B     INIT010                                                          
*                                                                               
INIT050  CLI   INPUT,C'T'          TEST INPUT=TAPE                              
         BE    *+12                                                             
         CLI   INPUT,C'D'          TEST INPUT=DISK                              
         BNE   ERR2                INVALID PARAMETER                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        OPEN FILES                                         *                   
*************************************************************                   
         SPACE 1                                                                
OPENFLS  NTR1                                                                   
         CLI   INPUT,C'D'          OPEN ADRFILE FOR INPUT                       
         BNE   OPEN020                                                          
         L     R3,AADRBUFF                                                      
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,SERVICE,FILELIST,(R3)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
OPEN020  CLI   INPUT,C'T'          OPEN ADRTAPE FOR INPUT                       
         BNE   OPEN030                                                          
         OPEN  ADRIN                                                            
*                                                                               
OPEN030  B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN                                               *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
         L     R3,AADRBUFF         R3=A(BUFFER)                                 
*                                                                               
MAIN010  BAS   RE,ADRGET           GET AN ADR RECORD                            
*                                                                               
         CLI   0(R3),X'FF'         TEST FOR EOF                                 
         BE    MAINXX                                                           
         CLC   0(5,R3),=C'*TTRC'   TEST FOR *TTRC                               
         BE    MAIN020                                                          
         CLI   0(R3),C'*'          IGNORE OTHER SPECIALS                        
         BE    MAIN010                                                          
*                                                                               
         B     MAIN010             IGNORE NORMAL ADR RECS                       
*                                                                               
MAIN020  LA    R2,10(R3)           HANDLE TEMPORARY RECORDS                     
         USING TMPLRECD,R2                                                      
*                                                                               
MAIN030  CLI   0(R2),C'T'          GET T RECORDS                                
         BNE   MAIN010                                                          
*                                                                               
         CLC   TMPLCODE,=Y(TLTEMPES)                                            
         BNE   *+12                                                             
         BAS   RE,TEMPOUT          OUTPUT DETAIL                                
         B     MAIN090                                                          
*                                                                               
         CLC   TMPLCODE,=Y(TIOTRC)                                              
         BNE   *+12                                                             
         BAS   RE,TEMPIOUT         OUTPUT DETAIL                                
         B     MAIN090                                                          
*                                                                               
         CLC   TMPLCODE,=Y(TWAITTRC)                                            
         BNE   *+12                                                             
         BAS   RE,TEMPWAIT         OUTPUT DETAIL                                
         B     MAIN090                                                          
*                                                                               
         CLC   TMPLCODE,=Y(TPARMEXT)                                            
         BNE   *+12                                                             
         BAS   RE,TEMPPARM         OUTPUT DETAIL                                
         B     MAIN090                                                          
*                                                                               
         CLC   TMPLCODE,=Y(TSOONTRC)                                            
         BNE   *+12                                                             
         BAS   RE,TEMPSOON         OUTPUT DETAIL                                
         B     MAIN090                                                          
*                                                                               
MAIN090  SR    R1,R1                                                            
         ICM   R1,1,1(R2)          BUMP TO NEXT                                 
         AR    R2,R1                                                            
         B     MAIN030                                                          
*                                                                               
MAINXX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        CLOSE                                              *                   
*************************************************************                   
         SPACE 1                                                                
CLOSE    NTR1                                                                   
         CLI   INPUT,C'T'          CLOSE TAPEIN IF USED                         
         BNE   CLOSEX                                                           
         CLOSE ADRIN                                                            
CLOSEX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        ROUTINES FOR READING ADR RECORDS                   *                   
*************************************************************                   
         SPACE 1                                                                
ADRGET   NTR1                                                                   
         L     R3,AADRBUFF                                                      
*                                                                               
         CLI   INPUT,C'D'          TEST DISK INPUT                              
         BNE   AGET020                                                          
         OC    ADRADR,ADRADR       TEST FIRST TIME                              
         BNZ   *+10                                                             
         MVC   ADRADR,=X'00010100'                                              
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRSEQ,ADRFILE,ADRADR,(R3)                      
         TM    8(R1),X'80'                                                      
         BO    ADREND              END-OF-FILE                                  
         CLI   8(R1),0                                                          
         BE    AGET050             GOT A RECORD                                 
         DC    H'0'                                                             
*                                                                               
AGET020  CLI   INPUT,C'T'          TEST TAPE INPUT                              
         BNE   AGET030                                                          
*                                                                               
         GET   ADRIN,(R3)          GET TAPE RECORD                              
         B     AGET050                                                          
*                                                                               
AGET030  DC    H'0'                INPUT=(UNKNOWN)                              
*                                                                               
AGET050  B     EXIT                EXIT REC IN ADRBUFF                          
*                                                                               
ADREND   MVC   0(16,R3),FFS        EXIT FFS IN ADRBUFF                          
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT TEMPEST ALLOCATION DETAIL R2=REC             *                   
*************************************************************                   
         SPACE 1                                                                
         USING TMPLRECD,R2                                                      
TEMPOUT  NTR1                                                                   
         MVC   FULL,TMPLTIME                                                    
         BAS   RE,TIMEOUT                                                       
         MVC   PRL(11),WORK1                                                    
*                                                                               
         MVC   PRL+12(8),TMPLLUID                                               
         IC    R1,TLTSESS                                                       
         LA    R1,C'A'(R1)                                                      
         STC   R1,PRL+21                                                        
         MVC   PRL+23(7),=C'SESSION'                                            
*                                                                               
         CLI   TLTALLOC,C'A'                                                    
         BNE   *+10                                                             
         MVC   PRL+31(10),=C'ALLOCATE  '                                        
         CLI   TLTALLOC,C'D'                                                    
         BNE   *+10                                                             
         MVC   PRL+31(10),=C'DEALLOCATE'                                        
*                                                                               
         SR    R1,R1                                                            
         EDIT  (B1,TLTNUM),(2,PRL+42)                                           
*                                                                               
         LA    R6,PRL+46                                                        
         LA    RF,TLTDATA                                                       
         SR    R1,R1                                                            
         IC    R1,TLTLEN                                                        
         SH    R1,=H'4'                                                         
         BZ    TEMPOX                                                           
*                                                                               
TEMPO1   EDIT  (B2,0(RF)),(4,0(R6))                                             
         LA    RF,2(RF)                                                         
         LA    R6,6(R6)                                                         
         SH    R1,=H'2'                                                         
         BNZ   TEMPO1                                                           
*                                                                               
TEMPOX   BAS   RE,PRINTL                                                        
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT IO TRACE                                     *                   
*************************************************************                   
         SPACE 1                                                                
         USING TMPLRECD,R2                                                      
TEMPIOUT NTR1                                                                   
         MVC   FULL,TMPLTIME                                                    
         BAS   RE,TIMEOUT                                                       
         MVC   PRL(11),WORK1                                                    
*                                                                               
         MVC   PRL+12(8),TMPLLUID                                               
*                                                                               
         CLI   TIOROUT,C'I'                                                     
         BNE   *+14                                                             
         MVC   PRL+22(6),=C'ISDDS '                                             
         B     TEMPIO10                                                         
*                                                                               
         CLI   TIOROUT,C'D'                                                     
         BNE   *+14                                                             
         MVC   PRL+22(6),=C'DADDS '                                             
         B     TEMPIO10                                                         
*                                                                               
         CLI   TIOROUT,C'U'                                                     
         BNE   *+14                                                             
         MVC   PRL+22(6),=C'????? '                                             
         B     TEMPIO10                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
TEMPIO10 EDIT  (B4,TIOSIN),(6,PRL+30)                                           
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,TIOCOMM,PRL+40,4                                 
*                                                                               
         MVC   PRL+50(8),TIOFILE                                                
         BAS   RE,PRINTL                                                        
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,TIOKEY,PRL,40                                    
         BAS   RE,PRINTL                                                        
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,TIOIOD,PRL,64                                    
         BAS   RE,PRINTL                                                        
*                                                                               
TEMPIOX  BAS   RE,PRINTL                                                        
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT WAIT TRACE                                   *                   
*************************************************************                   
         SPACE 1                                                                
         USING TMPLRECD,R2                                                      
TEMPWAIT NTR1                                                                   
         MVC   FULL,TMPLTIME                                                    
         BAS   RE,TIMEOUT                                                       
         MVC   PRL(11),WORK1                                                    
*                                                                               
         MVC   PRL+12(8),TMPLLUID                                               
*                                                                               
         CLI   TWTTYPE,C'W'                                                     
         BNE   *+14                                                             
         MVC   PRL+22(6),=C'WAIT  '                                             
         B     TEMPWA10                                                         
*                                                                               
         CLI   TIOROUT,C'X'                                                     
         BNE   *+14                                                             
         MVC   PRL+22(6),=C'CLEAR '                                             
         B     TEMPWA10                                                         
*                                                                               
         CLI   TIOROUT,C'P'                                                     
         BNE   *+14                                                             
         MVC   PRL+22(6),=C'POST  '                                             
         B     TEMPWA10                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
TEMPWA10 GOTO1 =V(HEXOUT),DMCB,TWTLOCK,PRL+30,8                                 
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,TWTECB,PRL+50,8                                  
         BAS   RE,PRINTL                                                        
*                                                                               
TEMPWAX  BAS   RE,PRINTL                                                        
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT PARM TRACE                                   *                   
*************************************************************                   
         SPACE 1                                                                
         USING TMPLRECD,R2                                                      
TEMPPARM NTR1                                                                   
         MVC   FULL,TMPLTIME                                                    
         BAS   RE,TIMEOUT                                                       
         MVC   PRL(11),WORK1                                                    
*                                                                               
         MVC   PRL+12(8),TMPLLUID                                               
*                                                                               
         MVC   PRL+22(8),TCALLER                                                
*                                                                               
         MVC   PRL+31(2),=C'AT'                                                 
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,TOFFS,PRL+34,4                                   
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,TPARM1,PRL+44,4                                  
         GOTO1 =V(HEXOUT),DMCB,TPARM2,PRL+54,4                                  
         GOTO1 =V(HEXOUT),DMCB,TPARM3,PRL+64,4                                  
         BAS   RE,PRINTL                                                        
*                                                                               
TEMPPARX BAS   RE,PRINTL                                                        
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT SOON TRACE                                   *                   
*************************************************************                   
         SPACE 1                                                                
         USING TMPLRECD,R2                                                      
TEMPSOON NTR1                                                                   
         MVC   FULL,TMPLTIME                                                    
         BAS   RE,TIMEOUT                                                       
         MVC   PRL(11),WORK1                                                    
*                                                                               
         MVC   PRL+12(8),TMPLLUID                                               
         LA    R4,TSOONTRY                                                      
         USING TBJOBTAB,R4                                                      
*                                                                               
         MVC   PRL+22(2),TBJCLASS                                               
         MVC   PRL+25(1),TBJPRTY                                                
         MVC   PRL+27(8),=C'USERID  ' <?? NEED TBJPQUSR                         
         MVC   PRL+36(3),TBJPQSUB   '                                           
         MVI   PRL+39,C','                                                      
         EDIT  (B2,TBJPQSEQ),(5,PRL+40)                                         
*                                                                               
         MVC   PRL+56(1),TBJPQID                                                
*                                                                               
         MVC   PRL+58(1),TBJADV                                                 
*                                                                               
         MVC   PRL+60(8),TBJLUID                                                
         MVC   PRL+69(8),TBJMVSID                                               
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,7,TBJSTIME                                                    
         MH    R1,=H'384'                                                       
         ST    R1,FULL                                                          
         BAS   RE,TIMEOUT                                                       
         MVC   PRL+79(8),WORK1                                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,7,TBJRTIME                                                    
         MH    R1,=H'384'                                                       
         ST    R1,FULL                                                          
         BAS   RE,TIMEOUT                                                       
         MVC   PRL+88(8),WORK1                                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,7,TBJETIME                                                    
         MH    R1,=H'384'                                                       
         ST    R1,FULL                                                          
         BAS   RE,TIMEOUT                                                       
         MVC   PRL+97(11),WORK1                                                 
*                                                                               
         MVC   PRL+110(7),TBJMONS                                               
*                                                                               
TEMPSOOX BAS   RE,PRINTL                                                        
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT TRANSACTION DETAIL                           *                   
*************************************************************                   
         SPACE 1                                                                
TRANSOUT NTR1                                                                   
         LR    R2,R1                                                            
         MVC   PRL(8),0(R2)                                                     
*                                                                               
         MVC   BYTE,8(R2)          PRINT SYSTEM NAME                            
         BAS   RE,GTOVSYS                                                       
         MVC   PRL+10(3),WORK                                                   
*                                                                               
         MVC   BYTE,9(R2)          PRINT SE NAME                                
         BAS   RE,GTSESYS                                                       
         MVC   PRL+15(7),WORK                                                   
*                                                                               
         MVC   BYTE,10(R2)         PRINT PROG NAME                              
         BAS   RE,GTPROG                                                        
         MVC   PRL+24(7),WORK                                                   
*                                                                               
         MVC   FULL,16(R2)         END TIME                                     
         L     RF,20(R2)                                                        
         S     RF,FULL             RF=ELAPSED TIME                              
         SR    RE,RE                                                            
         D     RE,=F'6000'         RF=MINS RE=SECS*100                          
*                                                                               
         MVC   PRL+34(3),=C'ET='                                                
         EDIT  (RF),(3,PRL+37),ZERO=NOBLANK,TRAIL=C'm'                          
         EDIT  (RE),(6,PRL+41),2,ZERO=NOBLANK,TRAIL=C's'                        
*                                                                               
         MVC   PRL+48(4),=C'from'                                               
*                                                                               
         MVC   FULL,16(R2)                                                      
         BAS   RE,TIMEOUT                                                       
         MVC   PRL+53(8),WORK1                                                  
         MVC   PRL+62(2),=C'to'                                                 
         MVC   FULL,20(R2)                                                      
         BAS   RE,TIMEOUT                                                       
         MVC   PRL+65(8),WORK1                                                  
*                                                                               
         MVC   PRL+76(4),=C'CPU='                                               
         EDIT  (B4,24(R2)),(6,PRL+80),2,ALIGN=LEFT                              
         MVC   PRL+88(4),=C'I/O='                                               
         EDIT  (B2,34(R2)),(6,PRL+92),ALIGN=LEFT                                
*                                                                               
         MVC   PRL+99(4),=C'SIN='                                               
         GOTO1 =V(HEXOUT),DMCB,12(R2),PRL+103,4                                 
         BAS   RE,PRINTL                                                        
         BAS   RE,PRINTL                                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINTER ROUTINES                                   *                   
*************************************************************                   
         SPACE 1                                                                
PRINTT   ST    RE,SAVERE           SET TITLES AND FORCE PAGE THROW              
         LTR   RF,R1                                                            
         L     R1,=V(CPRINT)                                                    
         USING DPRINT,R1                                                        
         BZ    PRINTT1             IF R1 WAS ZERO JUST FORCE NEW PAGE           
         MVC   TITLE,SPACES        CLEAR TITLE TO SPACES                        
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8              EX TITLES INTO TITLE                         
         B     *+10                                                             
         MVC   TITLE(0),1(RF)                                                   
PRINTT1  ZAP   LINE,=P'99'         ZAP LINE TO FORCE NEW PAGE                   
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
         SPACE 1                                                                
PRINTL   ST    RE,SAVERE           PRINT A LINE                                 
         L     R1,=V(CPRINT)                                                    
         MVC   P,PRL                                                            
         GOTO1 =V(PRINTER)         GOTO PRINTER                                 
         MVC   PRL,SPACES1                                                      
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
         DROP  R1                                                               
         EJECT                                                                  
*************************************************************                   
*        MISC SUBROUTINES                                   *                   
*************************************************************                   
         SPACE 1                                                                
GTOVSYS  ST    RE,SAVERE           GET SYS NAME INTO WORK                       
         SR    R1,R1                                                            
         LA    R1,SYSLST                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   0(1,R1),BYTE        SYSTEM NUMBER IN BYTE                        
         BE    GTOV010                                                          
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
GTOV010  MVC   WORK(8),9(R1)       OV SYSTEM                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GTSESYS  ST    RE,SAVERE           GET SE NAME INTO WORK                        
         SR    R1,R1               AND A(PGMS) TAB INTO FULL                    
         L     R1,=V(SELIST)                                                    
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   7(1,R1),BYTE        SE NUMBER IN BYTE                            
         BE    GTSE010                                                          
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
GTSE010  MVC   WORK(8),0(R1)       SE SYSTEM                                    
         MVC   FULL,24(R1)         A(PROG TABLE)                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GTPROG   ST    RE,SAVERE           GET PROGNAME INTO WORK                       
         L     R1,FULL             FULL MUST BE A(PGMS)                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   8(1,R1),BYTE        BYTE=PROG NUMBER                             
         BE    GTP010                                                           
         BXLE  R1,RE,*-10                                                       
         MVC   FULL(1),BYTE                                                     
         MVC   WORK(7),=C'PROG=??'                                              
         GOTO1 =V(HEXOUT),DMCB,FULL,WORK+5,1                                    
         B     *+10                                                             
GTP010   MVC   WORK,0(R1)          PROGRAM                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***************************************************                             
*        TIME EDIT SUBROUTINE FULL TO WORK1       *                             
***************************************************                             
         SPACE 1                                                                
TIMEOUT  ST    RE,SAVERE                                                        
         L     R1,FULL                                                          
         XR    R0,R0                                                            
         D     R0,=F'384'                                                       
         ST    R1,FULL                                                          
         MVC   WORK1(11),=C'00:00:00.00'                                        
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,=F'360000'                                                    
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         EDIT  (RE),(2,WORK1+9),FILL=0    100/SEC                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        ALLOCATE STORAGE AREA                              *                   
*        ENTRY R1=LENGTH OF AREA IN K.                      *                   
*************************************************************                   
         SPACE 1                                                                
GETMAIN  ST    RE,SAVERE                                                        
*                                                                               
         SLL   R1,10               MULTIPLY R1 BY 1K                            
         LR    R0,R1                                                            
         GETMAIN RU,LV=(0),LOC=(BELOW,ANY)                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        ERRORS                                             *                   
*************************************************************                   
         SPACE 1                                                                
ERR1     MVC   PRL(32),=C'PARAMETER CARD - INVALID KEYWORD'                     
         B     ERRX                                                             
ERR2     MVC   PRL(34),=C'PARAMETER CARD - INVALID PARAMETER'                   
         B     ERRX                                                             
ERRX     L     RD,SAVERD                                                        
         BAS   RE,PRINTL                                                        
         B     XBASE                                                            
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1),AL1(OP LEN-1),AL3(OUTPUT)               
*                                                                               
         SPACE 1                                                                
CARDTAB  DS    0F                                                               
         DC    C'INPUT  ',AL1(4),AL1(0),AL3(INPUT)                              
         DC    C'TRACE  ',AL1(4),AL1(0),AL3(TRACE)                              
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
INPUT    DC    C'D'                INPUT=DISK/TAPE                              
TRACE    DC    C'D'                TRACE=PARM/TEMPEST/WAIT                      
         EJECT                                                                  
VALCARD  NTR1                                                                   
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,FULL             SAVE LAST CHR ADDR IN FULL                   
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    VALCEXIT                                                         
VALC001  LA    R4,CARDTAB                                                       
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,12(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     VALCERR             ERROR IF ALL DONE                            
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
         CLI   0(R2),C'='                                                       
         BNE   VALCERR                                                          
         IC    R1,8(R4)            GET LEN FOR MOVE                             
         SR    RF,RF                                                            
         ICM   RF,7,9(R4)          GET ADDRESS FOR MOVE                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC030  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,FULL             TEST FOR END OF CARD                         
         BL    VALC030                                                          
*                                                                               
VALCEXIT CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
VALCERR  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
TITLE1   DC    AL1(L'TITLE1T)                                                   
TITLE1T  DC    C'PARAMETER CARDS'                                               
TITLE2   DC    AL1(L'TITLE2T)                                                   
TITLE2T  DC    C'TWA''S OF HEAVY CPU USERS'                                     
*                                                                               
BOXTOP   DC    X'AC',80X'BF',X'BC'                                              
BOXBOT   DC    X'AB',80X'BF',X'BB'                                              
*                                                                               
ADRIN    DCB   DDNAME=ADRIN,DSORG=PS,MACRF=(GM),EODAD=ADREND,          X        
               RECFM=FB,DCB=BLKSIZE=9500                                        
*                                                                               
GET      DC    C'GET '                                                          
PUT      DC    C'PUT '                                                          
END      DC    C'END '                                                          
*                                                                               
SPACES1  DC    CL132' '                                                         
FFS      DC    16X'FF'                                                          
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMRDIR   DC    CL8'DMRDIR'                                                      
GETREC   DC    CL8'GETREC'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
ADRFILE  DC    CL8'ADRFILE'                                                     
DMOPEN   DC    CL8'DMOPEN'                                                      
STATS    DC    CL8'STATS'                                                       
TWAT     DC    CL8'TWA DUMP'                                                    
*                                                                               
SERVICE  DC    CL8'SERVICE'                                                     
FILELIST DC    C'NADRFIL X'                                                     
*                                                                               
       ++INCLUDE FASYSLST                                                       
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
DMCB     DS    6F                                                               
TEMP     DS    CL80                                                             
WORK     DS    CL80                                                             
WORK1    DS    CL80                                                             
PRL      DS    CL132                                                            
*                                                                               
ADRADR   DS    F                                                                
SAVERE   DS    A                                                                
SAVERD   DS    A                                                                
*                                                                               
ATBLOCK  DS    A                                                                
AADRBUFF DS    A                                                                
ABUFFER  DS    A                                                                
*                                                                               
ADRHDR   DS    CL8                 SORT HEADER                                  
ADRBUFF  DS    6400C               ADR BLOCK                                    
TBLOCK   DS    0D                                                               
         DS    6400C                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
SSB      CSECT                                                                  
         DS    0D                                                               
         DC    X'00000002'                                                      
         SPACE 2                                                                
UTL      CSECT                                                                  
         DS    0D                                                               
         DC    F'0',X'01'                                                       
         SPACE 2                                                                
*                                                                               
* FAADRREC                                                                      
       ++INCLUDE FAADRREC                                                       
         ORG   ADRRECD             INTERNAL VERSION OF 1ST 16 BYTES             
ADRTYPE  DS    C                                                                
ADRTYPE1 DS    C                                                                
ADRNAME1 DS    CL7                                                              
ADRNAME2 DS    CL7                                                              
         EJECT                                                                  
*DDSTATREC                                                                      
*DDTEMPREC                                                                      
       ++INCLUDE DDSTATREC                                                      
       ++INCLUDE DDTEMPREC                                                      
       ++INCLUDE FATABSJOB                                                      
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'095DDTEMPREAD04/18/01'                                      
         END                                                                    
