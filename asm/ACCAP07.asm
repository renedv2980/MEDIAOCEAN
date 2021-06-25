*          DATA SET ACCAP07    AT LEVEL 017 AS OF 12/11/09                      
*PHASE T61D07A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP07 -- CALENDAR/LIST SCREEN                      *         
*                                                                     *         
*  COMMENTS:     LISTS CALENDAR BY YEAR/PERIOD/OFFICE                 *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       N/A                                                  *         
*                                                                     *         
*  OUTPUTS:      LIST                                                 *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
** DCUR L15 - LIST YEAR AS PREVIOUS YEAR IF CALENDAR GOES OVER INTO   *         
**            THE NEXT YEAR.                                                    
** DKEL L16 - FIXED YEAR FILTER SO DOESN'T DISPLAY YEARS AFTER THAT             
**            ENTERED, BUILDS NEW LIST ON FILTER CHANGE                         
***********************************************************************         
         TITLE 'T61D07 - CALENDAR/LIST'                                         
T61D07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D07**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          RA=A(TWA)                                    
         L     R9,ASYSD                                                         
         USING SYSD,R9             R8=A(BASE SAVED STORGAE)                     
         ST    R3,RELO                                                          
         ST    RC,SAVERC                                                        
*                                                                               
         LA    R2,CONRECH                                                       
*                                                                               
         MVC   AIO,AIO1            DEFAULT AIO                                  
         MVI   TSARSTAT,0                                                       
         MVI   BOTSCR,0            INITIALIZE BOTTOM SCREEN FOR TMS             
*                                                                               
         LA    R2,PFTABLE          INITIALIZE PFKEYS TO PFKEY TABLE             
         LA    R3,CLLPFKYH                                                      
PFKEYS   GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         USING PERVALD,R3                                                       
         USING CASRECD,R5                                                       
VK       DS    0H                  *** VALIDATE YEAR ***                        
         TM    CLLYEARH+4,X'80'                                                 
         BZ    EXIT                                                             
         CLI   CLLYEARH+5,0                                                     
         BNE   VK50                                                             
         XC    BIGKEY,BIGKEY                                                    
*        XC    TEMPYEAR,TEMPYEAR                                                
         MVI   TEMPYEAR,X'FF'           - NO INPUT                              
         B     EXIT                                                             
*                                                                               
VK50     LA    R5,BIGKEY                                                        
         LA    R2,CLLYEARH                                                      
*                                                                               
         TM    4(R2),X'08'                           IS VALID NUMERIC?          
         BZ    VKERRX                                INVALID YEAR               
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         LA    R3,BLOCK                                                         
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(BYTE,BLOCK)                           
         CLI   DMCB+4,X'04'        ONLY ONE DATE INPUT                          
         BE    VK60                                                             
         CLI   DMCB+4,X'00'        EVERYTHING OK                                
         BE    VK60                                                             
VKERRX   MVC   GERROR,=AL2(ACEINVYR)                                            
         B     ACCERRX                                                          
*                                                                               
VK60     MVC   TEMPYEAR,PVALPSTA        PUT YEAR IN KEY                         
         XC    BIGKEY,BIGKEY                                                    
         MVI   NEWKYSET,X'FF'           SET NEW FILTER VALUE FOR LR             
VKX      B     EXIT                                                             
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*              LIST - USING 1R RECORDS                                *         
***********************************************************************         
LR       DS    0H                                                               
         MVC   AIO,AIO1            USE AIO1                                     
         LA    R5,BIGKEY                                                        
         CLI   NEWKYSET,X'FF'      NEW FILTER VALUE                             
         BE    LR010               BUILD NEW KEY                                
         OC    CASKEY,CASKEY       1ST TIME IN?                                 
         BNZ   LRHI                                                             
*                                                                               
LR010    DS    0H                                                               
         XC    CASKEY,CASKEY                                                    
         MVI   NEWKYSET,X'00'      RESET INDICATOR                              
         MVI   CASKTYP,CASKTYPQ    X'3E'                                        
         MVI   CASKSUB,CASKSUBQ    X'0B'                                        
         MVC   CASKCPY,CMPY        COMPANY CODE                                 
         CLI   TEMPYEAR,X'FF'                                                   
         BNE   *+12                                                             
         MVI   CASPEDTE,X'00'                                                   
         B     *+10                                                             
         MVC   CASPEDTE(1),TEMPYEAR                                             
         MVC   SAVEKEY,BIGKEY                                                   
*                                                                               
LRHI     GOTO1 HIGH                                                             
         B     LR030                                                            
LRSEQ    GOTO1 SEQ                                                              
LR030    CLI   DMCB+8,0                                                         
         BE    LR050                                                            
         DC    H'0'                                                             
*                                                                               
LR050    CLC   SAVEKEY(CASKCPY+1-CASKEY),BIGKEY   COMPARE UP TO COMPANY         
         BNE   LRX                                                              
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         USING TMRELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,TMRELQ       TIMESHEET PERIOD RULES                       
         BAS   RE,GETEL                                                         
*                                                                               
         LA    R2,LISTAR                                                        
         USING LSTLINED,R2                                                      
         CLI   TMREND+1,X'01'      IS THE LAST PERIOD IN JANUARY?               
         BNE   LR60                NO                                           
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,TMREND),(0,WORK)  SAVE AS EBCDIC                  
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+10,F'-1' GET PREVIOUS YEAR           
         CLI   TEMPYEAR,X'FF'      IS THERE A FILTER VALUE?                     
         BE    *+14                NO - CONTINUE                                
         CLC   TEMPYEAR,WORK+10    YES - COMPARE FILTER WITH REC VALUE          
         BNE   LRX                 NOT EQUAL - CAN EXIT (SEQ ORDER)             
         GOTO1 DATCON,DMCB,(0,WORK+10),(23,LSTYEAR)                             
         B     LR61                                                             
LR60     CLI   TEMPYEAR,X'FF'      IS THERE A FILTER VALUE                      
         BZ    *+14                NO - CONTINUE                                
         CLC   TEMPYEAR,TMREND     YES - COMPARE FILTER WITH REC VALUE          
         BNE   LRX                 NOT EQUAL - CAN EXIT (SEQ ORDER)             
         GOTO1 DATCON,DMCB,(1,TMREND),(23,LSTYEAR)                              
LR61     MVC   LSTSPACE,SPACES                                                  
         GOTO1 DATCON,DMCB,(1,TMREND),(21,LSTPSTRT)                             
         GOTO1 DATCON,DMCB,(1,TMRSTART),(21,LSTPSTRT)                           
         GOTO1 DATCON,DMCB,(1,TMREND),(21,LSTPEND)                              
         MVC   LSTPSPRE,=C' - '                                                 
         MVC   LSTOFFC,CASKOFC                                                  
         GOTO1 LISTMON                                                          
         B     LRSEQ                                                            
         DROP  R2                                                               
*                                                                               
LRX      LA    R2,CLLACTH          FORCE CURSOR TO 1ST LIST ENTRY               
         ST    R2,ACURFORC                                                      
         XC    SAVEKEY,SAVEKEY                                                  
         B     EXIT                                                             
*                                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ROUTL    MVI   DUB,0             SET CC LOW                                     
         B     ROUTCC                                                           
ROUTH    MVI   DUB,2             SET CC HIGH                                    
         B     ROUTCC                                                           
ROUTE    MVI   DUB,1             SET CC EQUAL                                   
ROUTCC   CLI   DUB,1                                                            
EXIT     XIT1                                                                   
*                                                                               
EINVPER  MVC   GERROR,=AL2(ACEIVPER)                                            
         B     ACCERRX                                                          
ENATMS   MVC   GERROR,=AL2(ACENATMS)                                            
         B     ACCERRX                                                          
ERRINV   MVC   GERROR,=AL2(ACEINV)     INVALID INPUT FIELD                      
         B     ACCERRX                                                          
ENOCAL   MVC   GERROR,=AL2(ACENOCAL)     NO CALENDAR                            
         B     ACCERRX                                                          
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ACCERRX                                                          
ERRPLS   MVC   GERROR,=Y(ACIPLSE)                                               
         MVI   GMSGTYPE,C'I'                                                    
         B     ACCERRX                                                          
*                                                                               
ACCERRX  MVI   GMSYS,6             DEFAULT IS ACCOUNTING MSG SYSTEM             
         B     *+12                                                             
ERRX     MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         MVI   GMSGTYPE,C'I'                                                    
         GOTO1 MYERR               DEFAULT IS ACCOUNTING MSG SYSTEM             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* EQUATES/LITERALS                                                    *         
***********************************************************************         
         SPACE 1                                                                
MAXLNS   EQU   15                  MAX NUMBER OF LINES PER SCREEN               
*                                                                               
*              TIME TYPE TABLE WITH ACCUMULATOR                                 
*                                                                               
TIMTABL  DC    AL1(TIMTCB),AL2(AMTBILL-AMTTOTS)                                 
         DC    AL1(TIMTCR),AL2(AMTREAL-AMTTOTS)                                 
         DC    AL1(TIMTCN),AL2(AMTNBILL-AMTTOTS)                                
         DC    AL1(TIMTNC),AL2(AMTNBILL-AMTTOTS)                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY DEFINITIONS                                                   *         
***********************************************************************         
         SPACE 1                                                                
PFTABLE  DS    0C                                                               
         DC    AL1(LPF07X-*,07,PFTLIST,0,0)                                     
         DC    CL3'   '                                                         
         DC    CL8'        '                                                    
         DCDD  AC#LAST,8                                                        
LPF07X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF08X-*,08,PFTLIST,0,0)                                     
         DC    CL3'   '                                                         
         DC    CL8'        '                                                    
         DCDD  AC#NXT,8                                                         
LPF08X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF12X-*,12,PFTLIST,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              LIST SCREEN DSECT                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTLINED DSECT                                                                  
LSTYEAR  DS    CL4                 YEAR                                         
LSTSPACE DS    CL2                                                              
LSTPERD  DS    0CL23               PERIOD                                       
LSTPSTRT DS    CL10                PERIOD START DATE                            
LSTPSPRE DS    CL3                 BLANK - BLANK                                
LSTPEND  DS    CL10                PERIOD END DATE                              
         DS    CL2                                                              
LSTOFFC  DS    CL2                 OFFICE CODE                                  
*                                                                               
LSTBUMP  EQU   L'CLLACTH+L'CLLACT+L'CLLLINEH+L'CLLLINE                          
         EJECT                                                                  
       ++INCLUDE ACCAPWORKD                                                     
         PRINT ON                                                               
***********************************************************************         
* SCREENS                                                             *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPD1D                                                       
         EJECT                                                                  
***********************************************************************         
* APPLICATION SAVED STORAGE                                           *         
***********************************************************************         
         SPACE 1                                                                
STARTWRK DS    0A                  IN TWA                                       
RELO     DS    A                                                                
SAVERC   DS    F                                                                
SVDSKDA  DS    A                   SAVE DISK ADDRESS                            
         DS    3A                  N/D                                          
SAVEKEY  DS    CL56                                                             
LASTKEY  DS    CL56                                                             
TEMPYEAR DS    CL1                                                              
NEWKYSET DS    CL1                                                              
*                                                                               
AMTTOTS  DS    0PL4                                                             
AMTBILL  DS    PL4                 BILLABLE AMOUNT                              
AMTNBILL DS    PL4                 NON-BILLABLE AMOUNT                          
AMTREAL  DS    PL4                 REALIZATION AMOUNT                           
AMTTOTAL DS    PL4                 TOTAL AMOUNT                                 
*                                                                               
SCRBILL  DS    PL4                 BILLABLE AMOUNT  (SCREEN TOTALS)             
SCRNBILL DS    PL4                 NON-BILLABLE AMOUNT                          
SCRREAL  DS    PL4                 REALIZATION AMOUNT                           
SCRTOTAL DS    PL4                 TOTAL AMOUNT                                 
*                                                                               
TOTBILL  DS    PL4                 BILLABLE AMOUNT  (GRAND TOTAL)               
TOTNBILL DS    PL4                 NON-BILLABLE AMOUNT                          
TOTREAL  DS    PL4                 REALIZATION AMOUNT                           
TOTTOTAL DS    PL4                 TOTAL AMOUNT                                 
*                                                                               
FLTSTDTE DS    PL3                 FILTER START DATE                            
FLTENDTE DS    PL3                 FILTER END DATE                              
FLTCLT   DS    CL4                 FILTER CLIENT                                
FLTPRD   DS    CL4                 FILTER PRODUCT                               
FLTJOB   DS    CL6                 FILTER JOB                                   
FLTTSK   DS    CL2                 FILTER TASK                                  
FLTCLTLN DS    XL1                 FILTER CLIENT LENGTH                         
FLTPRDLN DS    XL1                 FILTER PRODUCT LENGTH                        
FLTJOBLN DS    XL1                 FILTER JOB LENGTH                            
FLTTSKLN DS    XL1                 FILTER TASK LENGTH                           
FLDFINP  DS    CL1                 FIELD INPUT                                  
FLDFCLT  EQU   X'80'               CLIENT INPUTTED                              
FLDFPRD  EQU   X'40'               PRODUCT INPUTTED                             
FLDFJOB  EQU   X'20'               JOB INPUTTED                                 
FLDFTSK  EQU   X'10'               TASK INPUTTED                                
FLDALLYR EQU   X'08'               YEAR=ALL                                     
FLD1NCLT EQU   X'04'               1N CLIENT, PRD/JOB/TSK INVALID               
CALDATE  DS    0PL3                CALENDAR DATE                                
CALYEAR  DS    CL1                 CALENDAR YEAR                                
         DS    CL2                                                              
CALSDATE DS    PL3                 CALENDAR START DATE                          
CALEDATE DS    PL3                 CALENDAR END DATE                            
CALKEY   DS    CL56                CALENDAR KEY                                 
*                                                                               
LSTSTAT  DS    XL1                 LIST STATUS                                  
RECFOUND EQU   X'80'               FOUND GOOD RECORD                            
LASTLINE EQU   X'40'               LAST LINE                                    
FRSTSCRN EQU   X'20'               FIRST SCREEN                                 
GOTGRAND EQU   X'10'               GOT GRAND TOTALS                             
LSTPRNTD EQU   X'08'               SOMETHING WAS PRINTED                        
NOTMRECS EQU   X'04'               NO MORE TIME RECORDS                         
LASTOFFC DS    CL2                 LAST OFFICE                                  
TEMPDATE DS    CL3                 TEMPORARY DATE                               
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* ACCAPDSECT                                                                    
* FATIOB                                                                        
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDSCANBLKD                                                                    
* ACGENFILE                                                                     
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACCAP07   12/11/09'                                      
         END                                                                    
