*          DATA SET ACREPR402  AT LEVEL 110 AS OF 03/23/15                      
*PHASE ACR402A,+0                                                               
*INCLUDE CENTER                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE ACLIST                                                                 
         TITLE 'WORK DISTRIBUTION REPORT'                                       
ACR402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACR4**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         USING ACR4D,RC                                                         
         LA    RC,SPACEND                                                       
*********************************************************************           
*        REQUESTED OFF THE SJ LEDGER                                *           
*                                                                   *           
*        JOB READS SJ AND SORTS  BY:                                *           
*        OFFICE GROUP/OFFICE/ORIGIN1/CLIENT/ORIGIN2/PRO/JOB/WC/EMP  *           
*          ORIGIN1 - IS THE OFFICE DEPT OF THE EMPLOYEE THE SAME    *           
*                    AS THE OFFICE GROUP CLIENT                     *           
*          ORIGIN2 - THIS EMPLOYEE WAS FOUND WORKING ON THE CLIENT  *           
*                    OF ANOTHER OG/OFF                              *           
*                                                                   *           
*        PROFILE 1          LEVEL OF DETAIL (SEE BELOW)             *           
*                                   USE OF AN OPTION WILL OVERRIDE  *           
*                                   PROFILE                         *           
*        SPACE 2                                                    *           
*        OPTION 1                                                   *           
*                                                                   *           
*                              BLANK= CLIENT/EMPLOYEE               *           
*                              A    = CLIENT                        *           
*                              B    = JOB                           *           
*                              C    = WORKCODE                      *           
*                              D    = WORKCODE/EMPLOYEE             *           
*                              E    = EMPLOYEE, NO WORKCODE         *           
*                                                                   *           
*-------------------------------------------------------------------*           
*        HISTORY                                                    *           
*        LEVEL    DATE             CHANGE                           *           
*-------------------------------------------------------------------*           
*        89-90  10/09/91           REMOVED OFFICE GROUP/REPORT      *           
*                                  TOTALS AS PER HWEI/IWEX          *           
*        96-107 09/25/96           CHANGED X'4B' CODE TO REFLECT    *           
*                                  NEW X'77' ELEMENTS - JPS         *           
*********************************************************************           
         EJECT                                                                  
*------------------------                                                       
* ROUTINE FOR RUN FIRST *                                                       
*------------------------                                                       
         SPACE 1                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         SPACE 1                                                                
         LA    RE,RELOTAB          RELOCATE MY A TYPES                          
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         SPACE 1                                                                
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         USING BOXD,R2                                                          
         L     R3,=A(BXHOOK)                                                    
         ST    R3,BOXHOOK                                                       
         L     R3,=A(HDHOOK)                                                    
         ST    R3,HEADHOOK                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFC                                       
         SPACE 1                                                                
         MVI   FCSUPOFC,C'Y'       SUPRESS MONACCS OFFICE FILTERING             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*------------------------------------------------+                              
*              ROUTINE FOR REQUEST FIRST         |                              
*------------------------------------------------+                              
         SPACE 1                                                                
REQF     CLI   MODE,REQFRST                                                     
         BNE   FRSTLEVA                                                         
         SPACE 2                                                                
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
*                                                                               
         BAS   RE,GETBUFF                                                       
*                                       -- INIT FOR SORT --                     
         XC    ALSORT,ALSORT                 CLEAR A(LAST SORT)                 
         LA    R1,SRTKYLN                    SORT KEY LENGTH                    
         CVD   R1,DUB                        CONVERT KEY LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         SPACE 1                                                                
         LA    R1,SRTRECL                    SORT RECORD LENGTH                 
         CVD   R1,DUB                        CONVERT REC LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         SPACE 1                                                                
*                                       -- PACK PERIOD DATES --                 
         XC    STRDATE,STRDATE                                                  
         MVC   ENDDATE,=X'FFFFFF'                                               
         CLC   QSTART,SPACES                                                    
         BE    REQF10                                                           
         MVC   WORK(6),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,STRDATE) START=YMD PACKED                
         SPACE 1                                                                
REQF10   CLC   QEND,SPACES                                                      
         BE    REQF20                                                           
         MVC   WORK(6),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,ENDDATE) ENDATE=YMD PACKED               
         SPACE 1                                                                
         USING ACMD,R2                                                          
REQF20   L     R2,AMONACC                                                       
         MVC   MYMEND(2),ACMMEND                                                
         MVC   MYMSTR(2),ACMMSTR    YES, THIS OVERRIDES FDATE                   
*        B     REQF40                                                           
*                                                                               
*EQF30   GOTO1 DATCON,DMCB,(4,RCDATE),(1,WORK) PACK TODAY                       
*        MVC   MYMSTR,WORK                                                      
*        MVI   MYMSTR+1,1          JAN THIS YEAR IS DEFAULT MOS START           
*        USING ACCOMPD,R2                                                       
*        L     R2,ADCMPEL                                                       
*        CLI   ACMPSTM,0           DO THEY HAVE A FISCAL START MON              
*        BE    REQF35              NO                                           
*        ZIC   R1,ACMPSTM          YES, CONVERT IT TO NUMERIC                   
*        N     R1,=F'15'           TURN OFF HI BITS                             
*        TM    ACMPSTM,X'F0'       WAS IT F1-F9                                 
*        BO    *+8                 YES, DONE                                    
*        LA    R1,15(R1)           NO, ADD COMVERSION FACTOR                    
*        STC   R1,MYMSTR+1                                                      
*        SPACE 1                                                                
*EQF35   MVC   MYMEND,WORK         USE TODAYS YYMM AS END MOS                   
*        CLC   MYMEND,MYMSTR       END GREATER THAN START ?                     
*        BNL   REQF40              EQUAL IS OK ALSO                             
*        MVI   WORK+1,X'0F'                                                     
*        SP    WORK(2),=P'10'      SUBTRACT 1 FROM YEAR                         
*        MVC   MYMSTR(1),WORK                                                   
         SPACE 1                                                                
*EQF40   MVC   CURMON,MYMEND                                                    
*        MVC   WORK(L'MYMEND),MYMEND                                            
*        MVI   WORK+2,1                                                         
*        GOTO1 DATCON,DMCB,(1,WORK),(6,CURMONP) FOR HEADER                      
*        MVC   WORK(L'MYMSTR),MYMSTR                                            
*        MVI   WORK+2,1                                                         
*        GOTO1 DATCON,DMCB,(1,WORK),(6,FRMMONP) FOR HEADER                      
*                                                                               
REQF60   L     R2,ACLILST          INIT TABLES                                  
         XC    0(2,R2),0(R2)                                                    
         L     R2,APROLST                                                       
         XC    0(2,R2),0(R2)                                                    
         L     R2,AJOBLST                                                       
         XC    0(2,R2),0(R2)                                                    
         L     R2,AEMPLST                                                       
         XC    0(2,R2),0(R2)                                                    
         L     R2,AOFFLST                                                       
         XC    0(2,R2),0(R2)                                                    
         L     R2,AOFGLST                                                       
         XC    0(2,R2),0(R2)                                                    
         SPACE 1                                                                
         USING TOTTBLD,R2                                                       
         LA    R0,TOTNUM                                                        
         LA    R2,TOTTBL                                                        
REQF70   MVI   TOTWANT,C'N'                                                     
         MVI   TOTDET,C'N'                                                      
         LA    R2,TOTTBLLN(R2)                                                  
         BCT   R0,REQF70                                                        
         SPACE 1                                                                
*        BUILD OFFICE NAME TABLE, OFFICE GROUP TABLE                            
         USING ACKEYD,R7                                                        
         LA    R7,MYKEY                                                         
         USING LISTD,R3                                                         
         LA    R3,WORK                                                          
         MVC   LISTKEY,SPACES                                                   
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOG                                                  
         MVC   ACOGCUL(1),RCCOMPFL                                              
         MVC   ACOGCUL+1(2),=C'SJ'                                              
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
REQF90   L     R2,ACREC                                                         
         CLC   0(ACOGCODE-ACOGKEY,R2),ACOGKEY                                   
         BNE   REQF100                                                          
         LR    R4,R2                                                            
         LA    R3,WORK                                                          
         MVC   LISTKEY(1),ACOGCODE-ACOGKEY(R2)                                  
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         LA    R5,LISTNAME                                                      
         BAS   RE,GETNAME                                                       
         L     R3,AOFGLST                                                       
         BAS   RE,BUILDLST                                                      
         BAS   RE,SEQ                                                           
         B     REQF90                                                           
         SPACE 1                                                                
REQF100  LA    R2,MYKEY                                                         
         MVI   ACOGSREC,ACOGOFF    GO FOR OFFICES NOW                           
         MVC   ACOGOFC,SPACES                                                   
         BAS   RE,HIGH                                                          
REQF110  L     R2,ACREC                                                         
         CLC   0(ACOGCODE-ACOGKEY,R2),ACOGKEY                                   
         BNE   REQF120                                                          
         LR    R4,R2                                                            
         LA    R3,WORK                                                          
         MVC   LISTKEY(2),ACOGOFC-ACOGKEY(R2)                                   
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         LA    R5,LISTNAME                                                      
         BAS   RE,GETNAME                                                       
         L     R3,AOFFLST                                                       
         BAS   RE,BUILDLST                                                      
         BAS   RE,SEQ                                                           
         B     REQF110                                                          
         SPACE 1                                                                
REQF120  EQU   *                                                                
         LA    R1,ORILEV                                                        
         BAS   RE,SETWANT                                                       
         LA    R1,OFFLEV                                                        
         BAS   RE,SETWANT                                                       
         LA    R1,OFGLEV                                                        
         BAS   RE,SETWANT                                                       
         MVC   DETLEVEL,SPACES                                                  
         MVC   DETLEVEL(8),=CL8'CLIENT'                                         
         LA    R1,CLILEV                                                        
         BAS   RE,SETWANT                                                       
         CLI   QOPT1,C'C'          CLIENT REQUESTED                             
         BE    REQF127             GET EMPLOYEE DATA                            
         SPACE 1                                                                
         MVC   DETLEVEL(8),=CL8'PRODUCT'                                        
         LA    R1,PRODLEV                                                       
         BAS   RE,SETWANT                                                       
         CLI   QOPT1,C'P'          CLIENT/PROD REQUESTED                        
         BE    REQF127             GET EMPLOYEE DATA                            
         SPACE 1                                                                
         MVC   DETLEVEL(8),=CL8'JOB'                                            
         LA    R1,JOBLEV                                                        
         BAS   RE,SETWANT                                                       
         SPACE 1                                                                
REQF127  CLI   QOPT2,C'N'          SUPRESS 1R DATA                              
         BE    REQF129             YES                                          
         MVC   DETLEVEL+15(8),=CL8'EMPLOYEE'                                    
         LA    R1,EMPLEV                                                        
         BAS   RE,SETWANT                                                       
         SPACE 1                                                                
REQF129  MVC   DETLEVEL+10(4),=CL4'TASK'                                        
         SPACE 1                                                                
         CLI   QOPT3,C'T'          TASK WANTED?                                 
         BE    REQF130             YES                                          
         MVC   DETLEVEL+10(4),SPACES                                            
         B     REQF140                                                          
         SPACE 1                                                                
REQF130  CLM   R1,1,=AL1(EMPLEV)   EMPLOYEE LEVEL SET                           
         BE    REQF140             YES, LEAVE IT                                
         LA    R1,WCODELEV         NO, SET TO TASK LEVEL                        
         SPACE 1                                                                
REQF140  BAS   RE,SETALOW                                                       
         CLI   QOPT3,C'T'          NO TASK                                      
         BNE   REQF150                                                          
         LA    R1,WCODELEV         NO, SET TO TASK LEVEL                        
         BAS   RE,SETWANT                                                       
         SPACE 1                                                                
REQF150  MVC   DETLEVEL+24(13),=C'LEVEL REQUEST'                                
         GOTO1 ADSQUASH,DMCB,DETLEVEL,L'DETLEVEL                                
         GOTO1 CENTER,DMCB,DETLEVEL,L'DETLEVEL                                  
         SPACE 1                                                                
*----------------------------------------------------------------------         
*        GET POSITION OF OFFICE IN 1R                                           
*----------------------------------------------------------------------         
         USING ACKEYD,R7                                                        
         LA    R7,MYKEY                                                         
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1R'                                             
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
         L     R4,ACREC                                                         
         CLC   ACKEYACC(L'ACKEYACC),0(R4)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACLEDGD,R4                                                       
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   OFFOFF,ACLTOFF                                                   
         NI    OFFOFF,X'FF'-X'40'                                               
         CLI   OFFOFF,0            OFFSET SHOULD BE X'0-12'                     
         BNE   *+8                                                              
         MVI   OFFOFF,1                                                         
         SPACE 1                                                                
         ZIC   R1,OFFOFF                                                        
         BCTR  R1,0                                                             
         STC   R1,OFFOFF                                                        
         SPACE 1                                                                
         MVI   OFFLEN,1            ASSUME NEW OFFICE LENGTH 2                   
         L     RE,ADOFFLST         SAVE VALID TWO BYTE OFFICES                  
         LA    RF,2*OFFMAX                                                      
*                                                                               
         USING ACCOMPD,R4                                                       
         L     R4,ADCMPEL          ARE WE NEW OFFICE?                           
         TM    ACMPSTA4,X'01'                                                   
         BO    REQF200             YES                                          
*                                                                               
         MVI   OFFLEN,0            OLD OFFICES, LENGTH IS 1                     
         L     RE,AOFFCHAR                                                      
         LA    RF,L'OFFCHAR                                                     
*                                                                               
         SPACE 1                                                                
REQF200  EQU   *                                                                
         L     R0,AOFFTAB                                                       
         LA    R1,2*OFFMAX                                                      
         MVCL  R0,RE                                                            
*                                                                               
         USING ACCOMPD,R4                                                       
         L     R4,ADCMPEL          ARE WE NEW OFFICE?                           
         TM    ACMPSTA4,X'01'                                                   
         BO    REQFX               OFFICE LIST BUILT BY MONACC                  
*                                                                               
         CLI   QOFFICE,C' '         IS THERE AN OLD OFFICE FILTER?              
         BE    REQFX               NO                                           
*                                                                               
         L     R2,AOFFTAB                                                       
         MVC   BYTE,QOFFICE                                                     
         LA    R3,BREQUAL                                                       
         TM    BYTE,UPCASE         UPPER CASE BIT ON                            
         BO    *+8                 YES                                          
         LA    R3,BRNEQ            SET TO EXCLUDE THIS OFFICE                   
*                                                                               
         OI    BYTE,UPCASE                                                      
         LA    R0,OFFMAX                                                        
*                                                                               
REQF210  CLC   BYTE,0(R2)                                                       
         EX    R3,REQFBC                                                        
         MVI   0(R2),C' '                                                       
REQF220  LA    R2,2(R2)                                                         
         BCT   R0,REQF210                                                       
REQFX    MVI   OFGSTAT,C'N'        ASSUME NOT USING OFFICE GROUPS               
         CLI   OFFOFF,0            IS OFFICE FIRST BYTE OF 1R KEY               
         BZ    REQFXX              YES, NO ROOM FOR OFFICE GROUP                
         CLI   OFFLEN,0            ARE THESE 1 BYTE OFFICES                     
         BZ    REQFXX              YES, NO OFFICE GROUP                         
         MVI   OFGSTAT,C'Y'                                                     
*                                                                               
REQFXX   B     XIT                                                              
         DROP  R2,R3,R7                                                         
*                                                                               
REQFBC   BC    0,REQF220                                                        
*                                                                               
UPCASE   EQU   X'40'                                                            
BREQUAL  EQU   X'80'                                                            
BRNEQ    EQU   X'70'                                                            
*                                                                               
         EJECT                                                                  
*----------------------------------------                                       
*        SAVE SJ CLIENT NAM                                                     
*----------------------------------------                                       
FRSTLEVA CLI   MODE,LEVAFRST       SJ CLIENT                                    
         BNE   FRSTLEVB            NO                                           
         NI    TRANSTAT,X'FF'-GOTLEVA                                           
         EJECT                                                                  
*--------------------------------------*                                        
*        SAVE SJ DEPARTMENT NAME                                                
*----------------------------------------                                       
FRSTLEVB CLI   MODE,LEVBFRST       SJ PRODUCT                                   
         BNE   FRSTLEVC            NO                                           
         NI    TRANSTAT,X'FF'-GOTLEVB                                           
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------                                       
*        SAVE 1R SUB DEPARTMENT NAME                                            
*----------------------------------------                                       
FRSTLEVC CLI   MODE,LEVCFRST       SJ JOB                                       
         BNE   PROCAC              NO                                           
         SPACE 2                                                                
PROCAC   CLI   MODE,PROCACC                                                     
         BNE   HISTRY                                                           
         NI    TRANSTAT,X'FF'-GOTACC                                            
         B     XIT                                                              
         SPACE 2                                                                
HISTRY   CLI   MODE,PROCSBAC                                                    
         BNE   ACTUAL                                                           
         NI    TRANSTAT,X'FF'-GOTHIST                                           
         NI    TRANSTAT,X'FF'-OTHERS                                            
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*  GET DATA FROM TRANSACTION AND BUILD SORT KEY                      *          
*      HOURS FROM 40 EL                                              *          
*      REVENUE FROM X'44'                                            *          
*      IF TRNSDATE < STRDATE OR > END REJECT                         *          
*      IF OFF/DEP OF 1R CONTRA = THIS OF                             *          
*      IF OFF/DEP OF 1R CONTRA = THIS OF                             *          
**********************************************************************          
         SPACE 1                                                                
ACTUAL   CLI   MODE,PROCTRNS          IS IT A TRANSACTION?                      
         BNE   REQLST                 NO - GET NEXT RECORD                      
*                                                                               
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'        THIS IS A 44 EL?                             
         BNE   XIT                 NO, BAD TRANS                                
         CLI   TRNSTYPE,49            IS TRANS TYPE 49?                         
         BE    ACTU05                 NO - SKIP                                 
         CLI   TRNSTYPE,34            IS IT A TRANSFER                          
         BNE   XIT                    NO - SKIP                                 
         USING ACKEYD,R5                                                        
         LR    R5,R4               GET THER KEY OF THE RECORD                   
         SH    R5,DATADISP                                                      
         CLC   ACKEYCON+1(2),=C'1R'                                             
         BNE   XIT                                                              
*                                                                               
ACTU05   CLC   TRNSDATE,STRDATE    DATE FILTERING                               
         BL    XIT                                                              
         CLC   TRNSDATE,ENDDATE                                                 
         BH    XIT                                                              
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         CLC   ACMMDTE,MYMSTR                                                   
         BL    XIT                                                              
         CLC   ACMMDTE,MYMEND                                                   
         BH    XIT                                                              
         DROP  R2                                                               
*                                                                               
         TM    TRANSTAT,GOTLEVA                                                 
         BO    *+8                                                              
         BAS   RE,NEWLEVA                                                       
         TM    TRANSTAT,GOTLEVB                                                 
         BO    *+8                                                              
         BAS   RE,NEWLEVB                                                       
         TM    TRANSTAT,GOTACC                                                  
         BO    *+8                                                              
         BAS   RE,NEWACC                                                        
         TM    TRANSTAT,GOTHIST                                                 
         BO    *+8                                                              
         BAS   RE,NEWHIST                                                       
*                                                                               
* FILL SORT KEY WITH SJ DATA, BASED ON REQUEST OPTION 1                         
*                                                                               
         USING SRTD,R6                                                          
         LA    R6,SRTREC                                                        
         XC    SRTREC(SRTLNQ),SRTREC                                            
         USING ACKEYD,R5                                                        
         LR    R5,R4               GET THER KEY OF THE RECORD                   
         SH    R5,DATADISP                                                      
         CLI   OFGSTAT,C'Y'        ARE THEY USING OFFICE GROUPS                 
         BNE   *+10                NO                                           
         MVC   SRTOFG,THISOFG      SAVE OFG/OFFICE OF THIS ACCOUNT              
*                                                                               
         MVC   SRTOFC,THISOFC                                                   
         MVC   SRTCLI,ACKEYACC+3   SAVE CLIENT                                  
*                                                                               
         CLI   QOPT1,C'C'          CLIENT REQUESTED                             
         BE    ACTU07              GET EMPLOYEE DATA                            
         MVC   SRTPROD,ACKEYACC+6  PROD                                         
*                                                                               
         CLI   QOPT1,C'P'          CLIENT/PROD REQUESTED                        
         BE    ACTU07              GET EMPLOYEE DATA                            
         MVC   SRTJOB,ACKEYACC+9   JOB                                          
*                                                                               
* FILL SORT KEY WITH 1R DATA, BASED ON REQUEST OPTION 2                         
*                                                                               
ACTU07   CLI   QOPT2,C'N'          SUPRESS 1R DATA                              
         BE    ACTU09              YES, LOOK FOR TASK/WORKCODE                  
         MVC   SRTEMPL,ACKEYCON+3                                               
*                                                                               
* FILL SORT KEY WITH WORKCODE/TASK - REQUEST OPTION 3                           
*                                                                               
ACTU09   XC    SRTWC,SRTWC                                                      
         CLI   QOPT3,C'T'          TASK WANTED                                  
         BNE   ACTU10              NO                                           
*                                                                               
         MVC   SRTWC,TRNSANAL                                                   
         CLI   SRTWC+1,C' '        ONE CHAR W/C                                 
         BH    ACTU10              NO                                           
         MVI   SRTWC+1,0           STORED W/TRAILING X'00'                      
*                                                                               
* SAVE HOURS, PUT TO SORT                                                       
*                                                                               
ACTU10   ZAP   SRTREV,TRNSAMNT                                                  
         ZAP   SRTHOUR,=P'0'                                                    
*                                                                               
         CLI   TRNSTYPE,57         IS THIS A WRITE OFF?                         
         BE    ACTU15              YES, HOURS WRITTEN OFF ARE IN THE 4B         
         CLI   TRNSTYPE,58                                                      
         BE    ACTU15                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   ACTU20                                                           
         USING ACPERSD,R4                                                       
         ZAP   SRTHOUR,ACPSHOUR                                                 
         B     ACTU20                                                           
*                                                                               
         USING ACMD,R2                                                          
ACTU15   L     R2,AMONACC                                                       
         L     R2,ACMAPRO2                                                      
*                                                                               
         USING PTAELD,R2                                                        
         CLI   0(R2),PTAELQ                                                     
         BNE   ACTU20                                                           
*                                                                               
         MVI   ELCODE,PTAELQ                                                    
         B     *+12                                                             
ACTU17   BAS   RE,NEXTEL2                                                       
         BNE   ACTU20                                                           
*                                                                               
         CLI   PTATYPE,PTATWOF     IS THIS A W/O?                               
         BE    *+12                                                             
         CLI   PTATYPE,PTATWOFR    OR W/O RECOVERY                              
         BNE   ACTU17              NO - GET NEXT '77'                           
*                                                                               
         LH    RF,PTAHOURS         GET BILLED HOURS                             
         CVD   RF,DUB              MAKE IT DECIMAL                              
         AP    SRTHOUR,DUB         ADD INTO SORT REC                            
         B     ACTU17                                                           
         DROP  R2                                                               
*                                                                               
ACTU20   TM    TRANSTAT,OTHERS     DOES THIS TRAN CROSS OFFICES                 
         BO    ACTU30              YES                                          
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
*                                                                               
ACTU30   MVI   SRTORG2,OTHERS      PUT OUT EMPLOYEES OF OTHER OFF REC           
         BAS   RE,PUTSORT                                                       
*                                                                               
         XC    SRTORG2,SRTORG2     BUILD RECORD FOR EMPLOYEE OFFICE             
         XC    SRTOFGC(3),SRTOFGC                                               
         MVI   SRTORG1,OTHERS                                                   
         CLI   OFGSTAT,C'Y'        USING OFFICE GROUPS                          
         BNE   ACTU35                                                           
         MVC   SRTOFGC(3),ACKEYCON+3                                            
         B     ACTU40              YES                                          
*                                                                               
ACTU35   ZIC   R2,OFFOFF           OFFSET OF OFFICE IN 1R                       
         LA    R3,ACKEYCON+3                                                    
         LA    R2,0(R2,R3)                                                      
         ZIC   R1,OFFLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTOFC(0),0(R2)                                                  
*                                                                               
ACTU40   BAS   RE,PUTSORT                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LAST REQUEST - GET RECORDS FROM SORTER                                 
*                       PRODUCE REPORT                                          
*------------------------------------------------------------------*            
REQLST   DS    0H                                                               
         CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         OC    ALSORT,ALSORT       ANYTHING PUT TO SORT                         
         BZ    REQL100             NO, LEAVE                                    
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   SRTREC(SRTLNQ),SPACES    INIT FOR FIRST THRU                     
*                                                                               
REQL10   BAS   RE,GETSORT               RETURN A SORT RECORD IN THISREC         
         CLC   THISREC(SRTLNQ),SPACES   ANYTHING RETURNED FROM SORT             
         BE    REQL90                   NO                                      
*                                                                               
         USING SRTD,R6                                                          
         LA    R6,THISREC               A(RECORD TO PROCESS)                    
         BAS   RE,PRTDET                                                        
         MVC   PRTSTA,SRTORG2                                                   
         LA    R3,SRTBUCKS              PROCESS SAVED RECORD                    
         BAS   RE,PRTBUCKS              PROCESS SAVED RECORD                    
         BAS   RE,PRINTEM                                                       
*                                                                               
         USING TOTTBLD,R4          ADD TO LOWEST TOTALS                         
         L     R4,ALOWTAB          FIRST CHECK THE BUCKET                       
         BAS   RE,NEEDTOTS                                                      
         LA    R2,TOTBUCKS                                                      
         BAS   RE,ADDEM                                                         
         DROP  R4                                                               
*                                                                               
* LOOK FOR LAST OF LEVEL                                                        
*                                                                               
         USING TOTTBLD,R3                                                       
         L     R3,ALOWTAB          ADDRESS OF THE LOWEST TABLE I NEED           
         SR    R1,R1                                                            
         OC    ALSORT,ALSORT       IS THIS THE LAST REC TO PROCESS              
         BNZ   REQL50              NO                                           
         MVI   SRTREC,X'FF'        FORCE LAST FOR ALL                           
*                                                                               
REQL50   CLI   TOTLEV,REPLEV       END OF TABLE                                 
         BE    REQL60              THEN DO FIRSTS                               
         IC    R1,TOTLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   THISREC(0),SRTREC   DO I NEED TOTALS AT THIS LEVEL               
         BE    REQL60              SEE IF I NEED FIRSTS                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,TOTLAST       A("LAST FOR" ROUTINE)                         
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
*                                                                               
         LA    R4,TOTTBLLN(R3)     POINT R4 TO NEXT TABLE                       
         BAS   RE,NEEDTOTS         SET NEED TOTS THERE IF NON ZERO              
         LA    R2,TOTBUCKS-TOTTBLD(R4)                                          
         LA    R3,TOTBUCKS                                                      
         BAS   RE,ADDEM                                                         
         LR    R3,R4               RESTORE R3 TO A(NEXT TABLE ENTRY)            
         B     REQL50              SEE IF I NEED TO TS HERE                     
         SPACE 1                                                                
REQL60   CLI   SRTREC,X'FF'        ANY RECORDS LEFT                             
         BE    REQL90              NO                                           
         LA    R1,TOTTBLLN        BACK UP TO THE LEVEL YOU DID LAST             
         SR    R3,R1                                                            
         C     R3,ALOWTAB          DID I DO ANY LASTS                           
         BL    REQL10              NO                                           
         LR    R2,R3               SET R2 FOR PRTFRSTS                          
         BAS   RE,PRTFRST          PRINT ANY FIRSTS YOU MIGHT NEED              
         B     REQL10              GET NEXT SORT REC                            
         SPACE 1                                                                
*                                                                               
REQL90   EQU   *                   REPORT TOTALS                                
         B     REQL100                                                          
*                                                                               
*        NO REPORT TOTALS AS PER HWEI/IWEX 10/9/91                              
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   HDOFG,SPACES                                                     
         MVC   HDOFGNM,SPACES                                                   
         MVC   HDOFF,SPACES                                                     
         MVC   HDOFFNM,SPACES                                                   
         MVC   ORIGPRT,SPACES                                                   
         LA    R3,TOTHIGH                                                       
         MVC   P+1(10),=C'TOTALS FOR'                                           
         MVC   P+12(L'TOTTYPE),TOTTYPE                                          
*                                                                               
         LA    R3,TOTBUCKS                                                      
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,PRINTEM                                                       
         SPACE 1                                                                
REQL100  GOTO1 SORTER,DMCB,=C'END'                                              
         BAS   RE,RELBUFF                                                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*----------------*                                                              
PUTSORT  NTR1                             -- PUT RECORD TO SORT --              
*----------------*                                                              
         USING SRTD,R6                                                          
         LA    R6,SRTREC                                                        
         L     R2,AOFFTAB          TABLE OF VALID OFFICES FOR REQUEST           
         LTR   R2,R2                                                            
         BZ    PUT50                                                            
         CLI   0(R2),0             ANYTHING THERE?                              
         BE    PUT50               NO, LET EM ALL THRU                          
         LA    R0,OFFMAX           MAX OFFICES IN TABLE                         
         SPACE 1                                                                
PUT10    CLC   SRTOFC,0(R2)        IS THIS AN OFFICE ON THE LIST                
         BE    PUT50               YES                                          
         LA    R2,2(R2)                                                         
         BCT   R0,PUT10                                                         
         B     XIT                 NO, REJECT                                   
         SPACE 1                                                                
PUT50    GOTO1 SORTER,DMCB,=C'PUT',(R6)                                         
         MVI   ALSORT,1                      ACTIVITY SWITCH                    
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
GETSORT  NTR1                      RETURN A SORT RECORD IN THISREC              
*                                  SUM SORT RECORDS WITH DUP KEYS               
*                                  WHEN ALSORT=0 THER ARE NO MORE               
*                                  EXPAND TWO BUCKS FROM SORT TO 4              
*------------------------------------------------------------*                  
         SPACE 1                                                                
         OC    ALSORT,ALSORT                                                    
         BZ    XIT                                                              
         MVC   THISREC(SRTLNQ),SRTREC                                           
GETS10   GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         ST    R6,ALSORT                  ADDRESS OF LAST SORT                  
         LTR   R6,R6                      END OF RECORDS FROM SORT              
         BNZ   GETS20                     NO                                    
         CLC   SRTREC(SRTLNQ),SPACES      IS THERE A PREVIOUS SORT              
         BE    XIT                        NO                                    
         B     GETSX                                                            
         SPACE 1                                                                
GETS20   LA    R5,SRTREC           ZAP BUCKETS IN WORK AREA                     
         LA    R2,SRTBUCKS-SRTD(R5)                                             
         LA    R1,NUMBUCKS                                                      
GETS25   ZAP   0(BUCKLN,R2),=P'0'                                               
         LA    R2,BUCKLN(R2)                                                    
         BCT   R1,GETS25                                                        
         SPACE 1                                                                
         MVC   SRTREC(SRTKYLN),0(R6)      SAVE CURRENT SORT KEY                 
         LA    R2,SRTBUCKS-SRTD(R5)                                             
         CLI   SRTORG2-SRTD(R5),OTHERS    WHICH BUCKET DO I USE                 
         BNE   *+8                        THE ONES I JUST ADDRESSED             
         LA    R2,2*BUCKLN(R2)            THE NXT SET                           
         MVI   SRTORG2-SRTD(R5),0         DONT NEED THIS ANYMORE                
         MVC   0(2*BUCKLN,R2),SRTBUCKS    MOVE 2 BUCKS FROM SORTER              
         SPACE 1                                                                
         CLC   THISREC(SRTLNQ),SPACES     DO I HAVE ONE SAVED                   
         BNE   GETS40                     YES - CONTINUE                        
         L     R2,=A(TOTHIGH)             INIT ALL LEVELS                       
         BAS   RE,PRTFRST                                                       
         MVC   THISREC(SRTKYLN),SRTREC    INIT THISREC KEY                      
         LA    R5,THISREC                 ZAP THISREC BUCKETS                   
         LA    R2,SRTBUCKS-SRTD(R5)                                             
         LA    R1,NUMBUCKS                                                      
GETS30   ZAP   0(BUCKLN,R2),=P'0'                                               
         LA    R2,BUCKLN(R2)                                                    
         BCT   R1,GETS30                                                        
         B     GETS50                     AND GET NEXT                          
         SPACE 1                                                                
GETS40   CLC   THISREC(SRTKYLN),SRTREC   SAME KEY                               
         BNE   GETSX                      NO - PROCESS SAVED ONE                
         SPACE 1                                                                
GETS50   LA    R1,NUMBUCKS                                                      
         LA    R5,THISREC                 SAME KEY FROM SORT, ADD UP            
         LA    R2,SRTBUCKS-SRTD(R5)                                             
         LA    R5,SRTREC                                                        
         LA    R3,SRTBUCKS-SRTD(R5)                                             
         SPACE 1                                                                
GETS60   AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R3,BUCKLN(R3)                                                    
         LA    R2,BUCKLN(R2)                                                    
         BCT   R1,GETS60                                                        
         B     GETS10                     AND GET NEXT                          
         SPACE 1                                                                
GETSX    LA    R6,THISREC                                                       
         SPACE 1                   PUT TO BUFFALO                               
         MVC   BUFKEY,SPACES                                                    
         MVC   BUFDATA,SPACES                                                   
         MVC   BUFACCT,SRTCLI                                                   
         MVC   BUFBUCKS(2*BUCKLN),SRTBUCKS                                      
         SPACE 1                                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFC,BUFREC                                
         XC    PRTSTA,PRTSTA                                                    
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
PRTBUCKS NTR1                   -PRINT THE 4 BUCKETS AT 0(R3)                   
*                               -BYTE CONTAINS BINARY MASK                      
*                                TO EITHER PRINT OF IGNORE A BUCKET             
*                               -REPSTAT MUST BE SET TO PRODTYPE OR             
*                                JOBTYPE FOR THE TYPE OF REPORT YOU ARE         
*                                PRODUCING                                      
*------------------------------------------------------------*                  
         ZAP   TOTHRS,=P'0'                                                     
         ZAP   TOTREV,=P'0'                                                     
         LA    R2,OFFSETS          TABLE OF OFSETS FOR THE REPORT               
*                                                                               
         AP    TOTHRS,0(8,R3)                                                   
         BAS   RE,PRTHRS           FIRST BUCKET IS ALWAYS HOURS                 
*                                                                               
         LA    R3,BUCKLN(R3)       NEXT BUCKET                                  
         LA    R2,1(R2)            NEXT OFFSET                                  
         AP    TOTREV,0(8,R3)                                                   
         BAS   RE,PRTAMNT                                                       
*                                                                               
         LA    R3,BUCKLN(R3)       NEXT BUCKET                                  
         LA    R2,1(R2)            NEXT OFFSET                                  
         AP    TOTHRS,0(8,R3)                                                   
         BAS   RE,PRTHRS                                                        
*                                                                               
         LA    R3,BUCKLN(R3)       NEXT BUCKET                                  
         LA    R2,1(R2)            NEXT OFFSET                                  
         AP    TOTREV,0(8,R3)                                                   
         BAS   RE,PRTAMNT                                                       
*                                                                               
         LA    R3,TOTHRS           TOTAL HOURS                                  
         LA    R2,1(R2)            NEXT OFFSET                                  
         BAS   RE,PRTHRS                                                        
*                                                                               
         LA    R3,TOTREV           TOTAL AMOUNT                                 
         LA    R2,1(R2)            NEXT OFFSET                                  
         BAS   RE,PRTAMNT                                                       
*                                                                               
PRTBX    B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------*            
*        PRINT THE DETAIL LINE IN THISREC-0(R6)                                 
*        IN THE TABLE ENRTY AT ALOWTAB: TOTDOFF- OFFEST INTO THE KEY            
*                                                OF DETAIL DATA                 
*                                       TOTDLEN- LENGTH OF DETAIL DATA          
*                                                MINUS 1                        
*                                       TOTDTBL  NAME TABLE FOR THIS            
*                                                A(0) FOR W/C LOOKUP            
*------------------------------------------------------------------*            
PRTDET   NTR1                                                                   
         USING TOTTBLD,R2                                                       
         CLI   QOPT2,C'N'          SUPPRRESSS 1R DATA                           
         BE    XIT                 YES, DETAIL IS IN TABLE                      
         L     R2,ALOWTAB                                                       
         ZIC   R4,TOTDOFF                                                       
         AR    R4,R6               A(FIELD TO LOOK UP)                          
         ZIC   R1,TOTDLEN          LENGTH OF FIELD                              
         USING LISTD,R3                                                         
         LA    R3,WORK                                                          
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTNAME,SPACES                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTKEY(0),0(R4)                                                 
         SPACE 1                                                                
         ICM   R3,3,TOTDTBL        S(TABLE OF NAME)                             
         LTR   R3,R3               IF NOT DEFINED, MUST BE W/C                  
         BZ    PRTD50                                                           
*                                                                               
         MVC   *+8(2),TOTDTBL     S(TABLE OF NAME)                              
         L     R3,FULL             FULL IS A DUMMY FOR THE ASSEMBLER            
*                                                                               
         BAS   RE,GETLIST                                                       
         LTR   R0,R0               ANYTHING RETURNED                            
         BZ    PRTD40                                                           
         LA    R3,WORK             GETLIST RETURNS NAME IN WORK                 
         MVC   P+19(25),LISTNAME                                                
PRTD40   ZIC   R4,TOTDOFF          OFFSET INTO SORT REC OF PRINT FIELS          
         AR    R4,R6               A(FIELD TO LOOK UP)                          
         ZIC   R1,TOTDLEN          LENGTH OF FIELD                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+6(0),0(R4)                                                     
         B     XIT                                                              
         SPACE 1                                                                
         USING ACANALD,R4                                                       
PRTD50   LA    R3,WORK                                                          
         L     R4,ADLEDGER                                                      
         AH    R4,DATADISP                                                      
         MVI   ELCODE,X'12'                                                     
PRTD60   BAS   RE,NEXTEL                                                        
         BNE   PRTD70                                                           
         CLC   ACANCODE,LISTKEY                                                 
         BNE   PRTD60                                                           
         MVC   P+6(2),ACANCODE                                                  
         MVC   P+19(L'ACANDESC),ACANDESC                                        
PRTD70   B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------*            
*        PRINT ANY FIRSTS YOU MIGHT NEED                                        
*        0(R2) POINTS TO THE HIGHEST LEVEL TOTAL PRINTED                        
*        PRINT THE NEW FIRST FOR THIS AND LOWER LEVELS                          
*------------------------------------------------------------------*            
PRTFRST  NTR1                                                                   
         USING SRTD,R6                                                          
         USING TOTTBLD,R2                                                       
         LA    R6,SRTREC           ADDRESS OF NEXT SORT RECORD                  
         SPACE 1                                                                
PRF10    C     R2,ALOWTAB          DO I NEED TOTS HERE                          
         BL    PRFX                NO                                           
         MVC   TOTACCT,SPACES                                                   
         ZIC   R3,TOTKOFF          AM I USING THIS LEVEL?                       
         AR    R3,R6               POINT TO SORT DATA FOR THIS LEVEL            
         ZIC   R1,TOTKLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R3),0(R3)                                                    
         BZ    PRF50               THIS LEVEL NOT DEFINED                       
         SPACE 1                                                                
         SR    RF,RF                                                            
         ICM   RF,3,TOTFIRST      A("FIRST FOR" ROUTINE)                        
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
         CLI   TOTDET,C'Y'         IS THIS A DETAIL LINE                        
         BE    PRF50               YES, DON'T PRINT                             
         CLI   TOTLEV,CLILEV       HIGHER THAT CLIENT                           
         BH    PRF50               YES, JUST HEADER INFO                        
         BAS   RE,PRINTEM                                                       
         SPACE 1                                                                
PRF50    BAS   RE,ZAPNBUMP         CLEAR BUCKS AT R2 AND POINT TO PREV          
         B     PRF10                                                            
         SPACE 1                                                                
PRFX     B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------*                
*        FIRSTS FOR VARIOUS LEVELS                             *                
*        R2 IS POINTING TO THE ADDRESS OF THE TABLE ENTREE FOR *                
*        THAT LEVEL                                            *                
*        R6 POINTS TO THE NEW SORT KEY                         *                
*--------------------------------------------------------------*                
         SPACE 1                                                                
NEWOFG   EQU   *                   NEW OFFICE GROUP                             
         ST    RE,SAVERE                                                        
         USING LISTD,R3                                                         
         LA    R3,WORK                                                          
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTKEY(1),SRTOFG                                                
         MVC   TOTACCT(1),SRTOFG                                                
         MVC   HDOFG,SRTOFG                                                     
         MVC   HDOFGNM,SPACES                                                   
         L     R3,AOFGLST                                                       
         BAS   RE,GETLIST                                                       
         LTR   R0,R0                                                            
         BZ    NEWOG10                                                          
         LA    R3,WORK                                                          
         MVC   HDOFGNM,LISTNAME                                                 
         SPACE 1                                                                
NEWOG10  MVI   FORCEHED,C'Y'                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
NEWOFF   EQU   *                   NEW OFFICE                                   
         ST    RE,SAVERE                                                        
         USING LISTD,R3                                                         
         MVC   HDOFF,SRTOFC                                                     
         MVC   TOTACCT(L'SRTOFC),SRTOFC                                         
         LA    R3,WORK                                                          
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTKEY(2),SRTOFC                                                
         MVC   HDOFFNM,SPACES                                                   
         L     R3,AOFFLST                                                       
         BAS   RE,GETLIST                                                       
         LTR   R0,R0                                                            
         BZ    NEWOF10                                                          
         LA    R3,WORK                                                          
         MVC   HDOFFNM,LISTNAME                                                 
NEWOF10  MVI   FORCEHED,C'Y'                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
NEWORI   EQU   *                   NEW ORIGIN                                   
         ST    RE,SAVERE                                                        
         MVC   ORIGPRT(25),=C'CLIENTS OF CURRENT OFFICE'                        
         CLI   SRTORG1,OTHERS                                                   
         BNE   *+10                                                             
         MVC   ORIGPRT(25),=C'CLIENTS OF OTHER OFFICES '                        
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'       TOF                                          
NEWORIX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
NEWCLI   EQU   *                   NEW CLIENT                                   
         ST    RE,SAVERE                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    *+8                                                              
         BAS   RE,PRINTEM                                                       
         USING LISTD,R3                                                         
         LA    R3,WORK                                                          
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTKEY(3),SRTCLI                                                
         L     R3,ACLILST                                                       
         BAS   RE,GETLIST                                                       
         MVC   P+2(12),TOTTYPE                                                  
         MVC   P+15(3),SRTCLI                                                   
         MVC   TOTACCT(3),SRTCLI                                                
         LA    R3,WORK             GETLIST RETURNS NAME IN WORK                 
         MVC   P+20(29),LISTNAME                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
NEWPRO   EQU   *                   NEW PRODUCT                                  
         ST    RE,SAVERE                                                        
         CLC   SRTPROD,SPACES       PRODUCT DEFINED?                            
         BNH   NEWPROX                                                          
         USING LISTD,R3                                                         
         LA    R3,WORK                                                          
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTKEY(3),SRTCLI                                                
         MVC   LISTKEY+3(3),SRTPROD                                             
         L     R3,APROLST                                                       
         BAS   RE,GETLIST                                                       
         MVC   P+2(12),TOTTYPE                                                  
         MVC   P+15(3),SRTPROD                                                  
         MVC   TOTACCT(3),SRTPROD                                               
         LA    R3,WORK             GETLIST RETURNS NAME IN WORK                 
         MVC   P+20(29),LISTNAME                                                
         MVI   SPACING,2                                                        
NEWPROX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
NEWJOB   EQU   *                   NEW JOB                                      
         ST    RE,SAVERE                                                        
         USING LISTD,R3                                                         
         LA    R3,WORK                                                          
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTKEY(3),SRTCLI                                                
         MVC   LISTKEY+3(3),SRTPROD                                             
         MVC   LISTKEY+6(6),SRTJOB                                              
         MVC   TOTACCT(6),SRTJOB                                                
         L     R3,AJOBLST                                                       
         BAS   RE,GETLIST                                                       
         BAS   RE,PRINTEM          SPACE BEFORE                                 
         MVC   P+3(12),TOTTYPE                                                  
         MVC   P+16(6),SRTJOB                                                   
         LA    R3,WORK             GETLIST RETURNS NAME IN WORK                 
         MVC   P+23(27),LISTNAME                                                
         GOTO1 UNDERLIN,DMCB,(47,P+3),(X'BF',PSECOND+3)                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
NEWWC    EQU   *                   NEW WORK CODE                                
         CLI   QOPT3,C'T'          NO TASK HERE?                                
         BNER  RE                  YUP                                          
         ST    RE,SAVERE                                                        
         USING LISTD,R3                                                         
         L     R4,ADLEDGER                                                      
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DUMP                                                             
         USING ACANALD,R4                                                       
NEWWC10  CLC   ACANCODE,SRTWC                                                   
         BE    NEWWC30                                                          
         BAS   RE,NEXTEL                                                        
         BE    NEWWC10                                                          
         SPACE 1                                                                
         BAS   RE,PRINTEM          WORK CODE NOT FOUND                          
         MVC   P+4(2),SRTWC                                                     
         MVC   P+7(13),=C'---UNKNOWN---'                                        
         MVC   TOTACCT(2),SRTWC                                                 
         B     NEWWCX                                                           
         SPACE 1                                                                
NEWWC30  BAS   RE,PRINTEM          SPACE BEFORE                                 
         MVC   P+4(2),SRTWC                                                     
         MVC   P+7(15),ACANDESC                                                 
         MVC   TOTACCT(2),ACANCODE                                              
         SPACE 1                                                                
NEWWCX   L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
         DROP  R2,R6                                                            
         EJECT                                                                  
*--------------------------------------*                                        
*  PRINT LASTS                         --                                       
*--------------------------------------*                                        
         USING TOTTBLD,R3                                                       
LPRT     NTR1                                                                   
         CLI   TOTLEV,CLILEV       CLIENT LEVEL TOTALS HERE                     
         BNE   LPRT30                                                           
         CLI   QOPT1,C' '          CLIENT/EMPLOYEE REPORT                       
         BE    LPRT40              YES, FORCE TOTALS                            
LPRT30   OC    TOTNEED,TOTNEED                                                  
         BZ    XIT                                                              
         SPACE 1                                                                
LPRT40   CLC   TOTACCT,SPACES      IS THIS LEVEL USED?                          
         BE    XIT                 NO                                           
         BAS   RE,NEEDMID          SEE IF THIS LEVEL NEEDS A BOX                
         MVC   P+2(11),=C'*TOTALS FOR'                                          
         MVI   P+49,C'*'                                                        
         MVC   P+14(12),TOTTYPE                                                 
         MVC   P+27(6),TOTACCT                                                  
         LA    R3,TOTBUCKS                                                      
         BAS   RE,PRTBUCKS                                                      
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
         BAS   RE,NEEDMID          CLOSE THE BOX, IF YOU OPENED IT              
         B     XIT                                                              
         SPACE 2                                                                
LASTORI  NTR1                                                                   
         OC    TOTNEED,TOTNEED                                                  
         BZ    XIT                                                              
         BAS   RE,BOXMID                                                        
         MVC   P+2(10),=C'TOTALS FOR'                                           
         MVC   P+14(25),ORIGPRT                                                 
         LA    R3,TOTBUCKS                                                      
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,PRINTEM                                                       
         BAS   RE,BOXMID                                                        
         B     XIT                                                              
         SPACE 1                                                                
LWC      CLI   QOPT3,C'T'          DO I NEED W/C TOTS?                          
         BNER  RE                  NO, RETURN                                   
         B     LPRT                YES                                          
         DROP  R3                                                               
         EJECT                                                                  
*--------------------------------------*                                        
*  GET ACCOUNT LEVEL DATA,             --                                       
*  SAVE JOB NAME                        --                                      
*  SAVE EFFECTIVE OFFICE GROUP/OFFICE  --                                       
*--------------------------------------*                                        
NEWACC   NTR1                                                                   
         LA    R6,SRTREC                                                        
         USING SRTD,R6                                                          
         BAS   RE,CLERSORT                 CLEAR SORT WORK AREA                 
         SPACE 3                                                                
*                                  *BUILD THE  SORT KEY*                        
         L     R2,ADGOBLOC                                                      
         USING GETOPTD,R2                                                       
         MVC   THISOFG,GOEFFOG                                                  
         MVC   THISOFC,GOEFFOFC                                                 
         USING ACKEYD,R5                                                        
         L     R5,ADACC                                                         
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         MVC   LISTKEY,ACKEYACC+3  SAVE 12 BYTE KEY                             
         L     R4,ADACCNAM                                                      
         LA    R5,LISTNAME                                                      
         BAS   RE,GETNAME          GET NAME                                     
         L     R3,AJOBLST          ADD JOB NAME TO TABLE                        
         BAS   RE,BUILDLST                                                      
         OI    TRANSTAT,GOTACC                                                  
         B     XIT                                                              
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
*--------------------------------------*                                        
*  GET HISTORY (CONTRA ACCOUNT) DATA   --                                       
*  SAVE EMPLOYEE NAME                   --                                      
*  SET IF 1R OFF/DEP DOSENT MATCH SJ'S --                                       
*--------------------------------------*                                        
NEWHIST  NTR1                                                                   
         USING TRSUBHD,R4                                                       
         L     R4,ADSUBAC                                                       
         CLI   0(R4),X'43'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TRANSTAT,GOTHIST                                                 
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         MVC   LISTKEY,TRSBACNT+3  SAVE 12 BYTE KEY                             
         MVC   LISTNAME,SPACES                                                  
         L     R3,AEMPLST          SEE IF EMP IS IN TABLE                       
         BAS   RE,GETLIST                                                       
         CLC   LISTNAME,SPACES     DID I GET A NAME BACK?                       
         BNE   NEWH10              YES                                          
         SPACE 1                                                                
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTNAME(0),TRSBNAME                                             
         SPACE 1                                                                
         L     R3,AEMPLST          ADD JOB NAME TO TABLE                        
         BAS   RE,BUILDLST                                                      
         SPACE 1                                                                
NEWH10   ZIC   R1,OFFOFF           OFFSET INTO ACCOUNT OF OFFICE                
         LA    R3,TRSBACNT+3                                                    
         AR    R3,R1                                                            
         ZIC   R1,OFFLEN           LENGTH OF OFFICE CODE                        
         EX    R1,NEWHCLC                                                       
         BE    NEWHISTX                                                         
         OI    TRANSTAT,OTHERS                                                  
NEWHISTX B     XIT                                                              
         SPACE 1                                                                
NEWHCLC  CLC   0(0,R3),THISOFC                                                  
         DROP  R2,R4                                                            
         EJECT                                                                  
*---------------------------------------------------------------------          
*        SAVE LEVEL A NAME                                                      
*---------------------------------------------------------------------          
         USING LISTD,R2                                                         
         USING ACKEYD,R3                                                        
NEWLEVA  NTR1                                                                   
         LA    R2,WORK                                                          
         L     R3,ADHEIRA                                                       
         MVC   LISTKEY,ACKEYACC+3  SAVE 12 BYTE KEY                             
         L     R4,ADLVANAM                                                      
         LA    R5,LISTNAME                                                      
         BAS   RE,GETNAME          GET NAME                                     
         L     R3,ACLILST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
         OI    TRANSTAT,GOTLEVA                                                 
         B     XIT                                                              
         SPACE 1                                                                
NEWLEVB  NTR1                                                                   
         LA    R2,WORK                                                          
         L     R3,ADHEIRB                                                       
         MVC   LISTKEY,ACKEYACC+3  SAVE 12 BYTE KEY                             
         L     R4,ADLVBNAM                                                      
         LA    R5,LISTNAME                                                      
         BAS   RE,GETNAME          GET NAME                                     
         L     R3,APROLST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
         OI    TRANSTAT,GOTLEVB                                                 
         B     XIT                                                              
         DROP  R2,R3                                                            
*----------------------------------------------------------------------         
*        STORE 1R ACCOUNT NAMES                                                 
*        ADD THE LISTD RECORD IN WORK TO THE TABLE AT 0(R3)                     
*----------------------------------------------------------------------         
         USING LISTD,R2                                                         
BUILDLST NTR1                                                                   
         LA    R2,WORK                                                          
         LH    R5,0(R3)            NUMBER IN TABLE                              
         LH    R0,2(R3)            MAX                                          
         CR    R5,R0               IS THERE ANY ROOM?                           
         BL    BLST10                                                           
         DC    H'0'                NO                                           
         DC    C'TBLISFUL'                                                      
         DS    0H                                                               
BLST10   LA    R4,6(R3)            START OF DATA                                
         LA    R1,LISTDLN          LENGTH OF ONE TABLE RECORD                   
         MR    R0,R5               TIMES THE NUMBER IN THE TABLE                
         AR    R4,R1               ADDED TO START ADDRESS IS OFFSET             
         MVC   0(LISTDLN,R4),LISTREC                                            
         LH    R0,0(R3)            BUMP # IN TABLE                              
         AH    R0,=H'1'                                                         
         STH   R0,0(R3)                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET 1R ACCOUNT NAMES KEY IS FIRST 12 OF WORK                           
*        TABLE IS AT 0(R3)                                                      
*        RETURN THE NAME IN WORK+12(36)                                         
*        RETURN ONLY THE SIGNIFICANT PORTIONS OF THE KEY                        
*        RETURN WITH R0 = 0 IF KEY DATA NOT FOUND                               
*----------------------------------------------------------------------         
         USING LISTD,R3                                                         
GETLIST  NTR1                                                                   
         LH    R0,0(R3)            NUMBER IN TABLE                              
         LTR   R0,R0               ANYTHING THERE                               
         BZ    XITR0               NO                                           
         LH    R2,4(R3)            OFFSET INTO KEY TO RETURN                    
         LA    R3,6(R3)            START OF DATA                                
         SPACE 1                                                                
GETL10   CLC   LISTKEY,WORK                                                     
         BE    GETL20                                                           
         LA    R3,LISTDLN(R3)      NEXT TABLE ENTRY                             
         BCT   R0,GETL10                                                        
         B     XITR0                                                            
GETL20   LA    R1,WORK                                                          
         MVC   LISTNAME-LISTD(L'LISTNAME,R1),LISTNAME                           
         MVC   WORK+50(L'LISTKEY),SPACES                                        
         LA    R4,L'LISTKEY                                                     
         SR    R4,R2               L'TO MOVE IS TOTAL LENGTH - OFFSET           
         BCTR  R4,0                                                             
         AR    R2,R1               OFFSET+ A(KEY) IS DATA TO MOVE               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK+50(0),0(R2)                                                 
         MVC   0(L'LISTKEY,R1),WORK+50                                          
         LA    R0,1                                                             
XITR0    XIT1  REGS=(R0)                                                        
         EJECT                                                                  
*----------------------------------------------------------------------         
*        R4 IS ADDRESS OF THE 20 ELEMENT                                        
*        R5 IS ADDRESS OF 36 BYTE AREA                                          
*----------------------------------------------------------------------         
         USING ACNAMED,R4                                                       
GETNAME  MVC   0(36,R5),SPACES                                                  
         CLI   0(R4),X'20'         IS THIS THE 20 EL                            
         BE    *+6                 IT BETTER BE                                 
         DC    H'0'                                                             
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,GETNMVC                                                       
         BR    RE                                                               
GETNMVC  MVC   0(0,R5),ACNMNAME                                                 
         DROP  R4                                                               
         EJECT                                                                  
*------------------                                                  *          
PRINTEM  NTR1                                                                   
*------------------                                                             
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
SETWANT  NTR1                                                                   
*              SET TOTWANT TO 'Y' FOR THE LEVEL IN R1                           
*------------------------------------------------------------*                  
         USING TOTTBLD,R2                                                       
         LA    R0,TOTNUM                                                        
         LA    R2,TOTTBL                                                        
SETW10   CLM   R1,1,TOTLEV                                                      
         BE    SETW20                                                           
         LA    R2,TOTTBLLN(R2)                                                  
         BCT   R0,SETW10                                                        
         B     SETWX                                                            
         SPACE 1                                                                
SETW20   MVI   TOTWANT,C'Y'                                                     
SETWX    B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
CLERSORT NTR1                    --  CLEAR SORT RECORD AREA --                  
*------------------------------------------------------------*                  
         USING SRTD,R6                                                          
         LA    R6,SRTREC                                                        
         XC    SRTREC(SRTLNQ),SRTREC                                            
         LR    R1,R6                         ADDR OF SORT REC INTO R1           
         LA    R1,SRTBUCKS                                                      
         LA    R0,NUMBUCKS                   NUMBER OF BUCKETS INTO R0          
         SPACE 1                                                                
CLER03   ZAP   0(BUCKLN,R1),=P'0'            CLEAR TO PACKED ZEROS              
         LA    R1,BUCKLN(R1)                 BUMP TO NEXT BUCKET                
         BCT   R0,CLER03                                                        
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
*        ZAP THE BUCKETS OF THE SUBTOTAL RECORD AT 0(R2)                        
*        THEN POINT R2 TO THE PREVIOUS BUCKET                                   
*------------------------------------------------------------*                  
         USING TOTTBLD,R2                                                       
ZAPNBUMP EQU   *                                                                
         LA    R3,TOTBUCKS                                                      
         LA    R1,NUMBUCKS                                                      
ZNB10    ZAP   0(BUCKLN,R3),=P'0'                                               
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,ZNB10                                                         
         SPACE 1                                                                
         LA    R1,TOTTBLLN         POINT TO THE PREVIOUS BUCKET                 
         SR    R2,R1               TO FORCE FIRST FOR LOWER LEVELS              
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
*------------------------------------------------------------*                  
*        CALL DATAMRG , MYKEY HAS BEEN SET                                      
*        RETURN IN R4 THE A(20 ELEMENT)                                         
*------------------------------------------------------------*                  
GETACCT  NTR1                                                                   
         BAS   RE,READ                                                          
         MVI   ELCODE,X'20'                                                     
         L     R4,ACREC                                                         
         BAS   RE,GETEL                                                         
         BE    GETAX                                                            
         DC    H'0'                                                             
GETAX    XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*------------------------------------------------------------*                  
*        1) IF WE DON'T WANT THE TOTALS AT 0(R4), AS PER REQUEST OPTS           
*              BUMP R4 TIL WE FIND A BUCKET WE WANT                             
*        IF THE BUCKET VALUES OF THE TABLE RECORD AT 0(R4)                      
*        ARE NOT ZERO, SET TOTNEED TO 1 SO TOTALS WILL BE PRODUCED              
*------------------------------------------------------------*                  
         USING TOTTBLD,R4                                                       
NEEDTOTS EQU   *                                                                
NEEDT10  CLI   TOTWANT,C'Y'       FIND THE LEVEL I WANT                         
         BE    NEEDT20                                                          
         CLI   TOTLEV,REPLEV       I                                            
         BER   RE                                                               
         LA    R4,TOTTBLLN(R4)                                                  
         B     NEEDT10                                                          
         SPACE 1                                                                
NEEDT20  XC    TOTNEED,TOTNEED                                                  
         LA    R1,TOTBUCKS                                                      
         LA    R0,NUMBUCKS                                                      
NEEDT30  CP    0(BUCKLN,R1),=P'0'                                               
         BNE   NEEDT40                                                          
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,NEEDT30                                                       
         BR    RE                                                               
         SPACE 1                                                                
NEEDT40  MVI   TOTNEED,1                                                        
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
*------------------------------------------------------------*                  
*        PRINT CLIENT LEVEL SUMMARY                                             
*        SAVE TYPE IS SET TO THE TYPE OF SUMMARY YOU WANT TO PRINT              
*------------------------------------------------------------*                  
         USING TOTTBLD,R2                                                       
PRTSUM   NTR1                                                                   
         LA    R2,SUMTOTAL                                                      
         LA    R0,4                                                             
PRTS00   ZAP   0(BUCKLN,R2),=P'0'                                               
         LA    R2,BUCKLN(R2)                                                    
         BCT   R0,PRTS00                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,3          PROD SUMMARY                                 
         MVI   RCSUBPRG,4                                                       
         MVI   PRTSTA,SUMMARY                                                   
         MVC   LASTBUF,SPACES                                                   
         MVC   BUFKEY,SPACES                                                    
         MVC   BUFTYPE,SAVETYPE                                                 
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ABUFC),BUFKEY,1                         
         MVC   LASTBUF,BUFKEY                                                   
PRTS10   TM    DMCB+8,X'80'                                                     
         BO    XIT                 NO RECS AT ALL                               
         CLC   BUFTYPE,SAVETYPE    NONE OF THIS TYPE                            
         BNE   XIT                                                              
         SPACE 1                                                                
PRTS15   LA    R3,BUFBUCKS                                                      
         BAS   RE,PRTBUCKS                                                      
         MVC   P+5(2),BUFACCT                                                   
         SPACE 1                                                                
         SPACE 1                                                                
PRTS40   MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
         SPACE 1                                                                
         LA    R2,SUMTOTAL                                                      
         LA    R3,BUFBUCKS                                                      
         LA    R1,NUMBUCKS                                                      
PRTS50   AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R2,BUCKLN(R2)                                                    
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,PRTS50                                                        
         SPACE 1                                                                
PRTS70   MVC   LASTBUF,BUFKEY                                                   
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFC,BUFKEY,1                              
         TM    DMCB+8,X'80'                                                     
         BO    PRTSX               NO RECS AT ALL                               
         CLC   BUFTYPE,SAVETYPE    NONE OF THIS TYPE                            
         BNE   PRTSX                                                            
         B     PRTS15                                                           
PRTSX    EQU   *                                                                
         LA    R3,SUMTOTAL                                                      
         MVC   P+1(10),=C'TOTALS FOR'                                           
         MVC   P+12(6),=C'CLIENT'                                               
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,PRINTEM                                                       
         IC    R4,SAVETYPE                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',((R4),ABUFC),(X'80',1)                    
         XC    PRTSTA,PRTSTA                                                    
         B     XIT                                                              
         SPACE 3                                                                
         USING LISTD,R3                                                         
         EJECT                                                                  
*------------------------------------------------------------*                  
*        ADD THE NUMBUCKS BUCKETS AT R3 TO THOSE AT R2                          
*------------------------------------------------------------*                  
ADDEM    NTR1                                                                   
         LA    R1,NUMBUCKS                                                      
ADDM20   AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R2,BUCKLN(R2)                                                    
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,ADDM20                                                        
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
*        SET ALOWTAB TO THE ADDRESS OF THE LOWEST TABLE ENTRY WE                
*        WILL NEED                                                              
*------------------------------------------------------------*                  
SETALOW  EQU   *                                                                
         USING TOTTBLD,R2                                                       
         LA    R2,TOTTBL                                                        
         LA    R0,TOTNUM                                                        
SETA20   CLM   R1,1,TOTLEV                                                      
         BE    SETA30                                                           
         LA    R2,TOTTBLLN(R2)                                                  
         BCT   R0,SETA20                                                        
         LA    R2,TOTTBL           IF LEVEL NOT FOUND, SET TO LOWEST            
         B     *+8                 AND DON'T SET TOTDET                         
         SPACE 1                                                                
SETA30   MVI   TOTDET,C'Y'                                                      
         ST    R2,ALOWTAB                                                       
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET SPACE FOR THE NAME TABLES                                          
*----------------------------------------------------------------------         
GETBUFF  NTR1                                                                   
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         GETMAIN R,LV=(0)                                                       
         LA    R0,MAINNUM                                                       
         LR    R5,R1               R5 IS BUFFER POINTER                         
         ST    R1,ABUFF            SAVE BUFF START                              
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         STCM  R5,15,MCUSRDMP                                                   
         LR    RF,R5                                                            
         A     RF,=A(BUFSIZE)                                                   
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         USING HEADD,R5                                                         
         L     R2,=A(MAINTAB)                                                   
         USING MAIND,R2                                                         
*                                                                               
GETB10   MVC   *+8(2),MAINAST     SCON OF WHERE TO STORE BUFF LOCATION          
         ST    R5,FULL             FULL IS A DUMMY FOR THE ASSEMBLER            
         XC    HEADER,HEADER       CLEAR TABLE HEADER                           
         MVC   HEADSIG,MAINSIG                                                  
*                                                                               
         OC    MAINMAX(2),MAINMAX  WILL MAINMAX FIT IN A HALF                   
         BZ    *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
         MVC   HEADMAX,MAINMAX+2   SAVE FULLWORD IF HALFWORD                    
*                                                                               
         A     R5,MAINSIZE         BUMP R5                                      
*                                                                               
         LA    R2,MAINLEN(R2)                                                   
         BCT   R0,GETB10                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        RELEASE GETMAINED SPACE                                                
*----------------------------------------------------------------------         
RELBUFF  NTR1                                                                   
         L     R1,ABUFF                                                         
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         XC    MCUSRDMP,MCUSRDMP   CLEAR XTRA DUMP ADDRESS                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
*              DATAMGR INTERFACE                                                
*------------------------------------------------------------*                  
HIGH     MVC   COMMAND,=C'DMRDHI'            READ HIGH                          
*        MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
         SPACE 1                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'            READ SEQUENTIAL                    
*        MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
         SPACE 1                                                                
READ     MVC   COMMAND,=C'DMREAD'            A SPECIFIC READ                    
         SPACE 1                                                                
GTREC    NTR1                                                                   
         L     R7,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R7)                      
         CLI   DMCB+8,0                      TEST FOR ERRORS                    
         BE    *+6                                                              
         DC    H'0'                          DIE IF ERRORS FOUND                
         B     XIT                                                              
         SPACE 4                                                                
BRRE     BR    RE                                                               
*                                                                               
DUMP     DC    H'0'                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* GETEL # 2                                                          *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R2,DATADISP,ELCODE,2                                            
         EJECT                                                                  
**********************************************************************          
* EDIT ROUTINES - EDIT THE BUCKET AT 0(R3)                           *          
*      0(R2) IS THE OFFSET INTO 'P' TO PRINT                         *          
**********************************************************************          
         SPACE 1                                                                
PRTHRS   LA    R4,P                                                             
         ZIC   R1,0(R2)                                                         
         AR    R4,R1                                                            
         EDIT  (P8,(R3)),(10,(R4)),2,MINUS=YES                                  
         BR    RE                                                               
         SPACE 2                                                                
PRTAMNT  CLI   QOPT10,C'N'         QOPT10 HAS SECURITY FLAG                     
         BER   RE                  NOT ALLOWED TO SEE REV                       
         LA    R4,P                                                             
         ZIC   R1,0(R2)                                                         
         AR    R4,R1                                                            
         EDIT  (P8,(R3)),(11,(R4)),2,MINUS=YES                                  
         BR    RE                                                               
*                                                                               
PRTRAT   LA    R4,P                                                             
         ZIC   R1,0(R2)                                                         
         AR    R4,R1                                                            
         EDIT  (P8,(R3)),(7,(R4)),2,MINUS=YES                                   
         BR    RE                                                               
         EJECT                                                                  
*------------------------------------------------------------*                  
*        PRINT A MIDLINE, WHEREVER YOU ARE                                      
*                                                                               
*------------------------------------------------------------*                  
BOXMID   NTR1                                                                   
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         ZIC   R2,LINE                                                          
         CH    R2,=H'52'                                                        
         BL    BOXM20                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     BOXM50                                                           
BOXM20   ZIC   R2,LINE                                                          
         LA    R3,BOXROWS(R2)                                                   
         BCTR  R3,0                                                             
         MVI   0(R3),C'M'                                                       
         MVI   BOXINIT,0                                                        
BOXM50   BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*------------------------------------------------------------*                  
*        CALL BOXMID, FOR SELECTED LEVELS                                       
*------------------------------------------------------------*                  
NEEDMID  NTR1                                                                   
         USING TOTTBLD,R2                                                       
         CLI   TOTLEV,OFFLEV                                                    
         BE    NEED20                                                           
         CLI   TOTLEV,OFGLEV                                                    
         BE    NEED20                                                           
         CLI   TOTLEV,REPLEV                                                    
         BE    NEED20                                                           
         B     XIT                                                              
NEED20   BAS   RE,BOXMID                                                        
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
*              CONSTANTS                                                        
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    A(RECORD)                                                        
         DC    A(BUFFALOC)                                                      
         DC    A(OFFTAB)                                                        
         DC    A(OFFCHAR)                                                       
         DC    V(SORTER)                                                        
         DC    V(UNDERLIN)                                                      
         DC    V(ACLIST)                                                        
         DC    V(CENTER)                                                        
         DC    X'FF'                                                            
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
         SPACE 2                                                                
         EJECT                                                                  
*TOTLEV   DS    AL1                 SUB TOTAL LEVEL                             
*TOTLEN   DS    AL1                 LENGTH OF THIS LEVEL IN SORT                
*TOTDISP  DS    AL1                 LENGTH OF THIS LEVEL IN SORT                
*TOTTYPE  DS    CL12                SUB TOTAL TYPE                              
*TOTBUCKS DS    0C                  A(FIRST BUCKET)                             
*         DS    (BUCKLN)P                                                       
*         DS    (BUCKLN)P                                                       
*         DS    (BUCKLN)P                                                       
*         DS    (BUCKLN)P                                                       
*TOTFIRST DS    AL2                 ADDRESS OF FIRST FOR ROUTINE                
*TOTLAST  DS    AL2                 ADDRESS OF LAST FOR ROUTINE                 
*TOTWANT  DS    AL1                                                             
*TOTNEED  DS    AL1                 DO I NEED TO PRINT SUBTOTS                  
*TOTDATA  DS    CL7                 DATA TO PRINT IN TOTAL FOR L                
*TOTTBLLN EQU   *-TOTTBLD                                                       
*--------------------------------------------------------------------*          
*        TABLE OF SUBTOTAL VALUES                                               
*        LEVEL, LEVEL LENGTH, TITLE,NAME, 4 (BUCKLN)P ACCUMS                    
*        TABLE IS COVERED BY TOTTBLD                                            
*--------------------------------------------------------------------*          
TOTTBL   DS    0H                                                               
         SPACE 1                                                                
         DC    AL1(WCODELEV),AL1(SRTWCLN),AL1(0)                                
         DC    CL12'WORKCODE  ',(4*8)C' '                                       
         DC    AL2(NEWWC-ACR402),AL2(LWC-ACR402),AL1(0),AL2(0)                  
         DC    CL12' '                                                          
         DC    AL1(SRTEMPLN-L'SRTEMPL)                                          
         DC    AL1(L'SRTEMPL-1)                                                 
         DC    SL2(AEMPLST)                                                     
         DC    AL1(SRTWCLN-L'SRTWC)                                             
         DC    AL1(L'SRTWC)                                                     
         SPACE 1                                                                
         DC    AL1(JOBLEV),AL1(SRTJOBLN),AL1(0)                                 
         DC    CL12'JOB       ',(4*8)C' '                                       
         DC    AL2(NEWJOB-ACR402),AL2(LPRT-ACR402),AL1(0),AL2(0)                
         DC    CL12' '                                                          
         DC    AL1(SRTWCLN-L'SRTWC)                                             
         DC    AL1(L'SRTWC-1)                                                   
         DC    SL2(0)                                                           
         DC    AL1(SRTJOBLN-L'SRTJOB)                                           
         DC    AL1(L'SRTJOB)                                                    
         SPACE 1                                                                
         DC    AL1(PRODLEV),AL1(SRTPROLN),AL1(0)                                
         DC    CL12'PRODUCT   ',(4*8)C' '                                       
         DC    AL2(NEWPRO-ACR402),AL2(LPRT-ACR402),AL1(0),AL2(0)                
         DC    CL12' '                                                          
         DC    AL1(SRTJOBLN-L'SRTJOB)                                           
         DC    AL1(L'SRTJOB-1)                                                  
         DC    SL2(AJOBLST)                                                     
         DC    AL1(SRTPROLN-L'SRTPROD)                                          
         DC    AL1(SRTPROLN)                                                    
         SPACE 1                                                                
         DC    AL1(CLILEV),AL1(SRTCLILN),AL1(0)                                 
         DC    CL12'CLIENT    ',(4*8)C' '                                       
         DC    AL2(NEWCLI-ACR402),AL2(LPRT-ACR402),AL1(0),AL2(0)                
         DC    CL12' '                                                          
         DC    AL1(0)              THERE IS NO PROD LEVEL DETAIL FORMAT         
         DC    AL1(0)                                                           
         DC    SL2(0)                                                           
         DC    AL1(SRTCLILN-L'SRTCLI)                                           
         DC    AL1(SRTCLILN)                                                    
         SPACE 1                                                                
         DC    AL1(ORILEV),AL1(SRTORILN),AL1(0)                                 
         DC    CL12'ORIGIN    ',(4*8)C' '                                       
         DC    AL2(NEWORI-ACR402),AL2(LASTORI-ACR402),AL1(0),AL2(0)             
         DC    CL12' '                                                          
         DC    AL1(SRTCLILN-L'SRTCLI)                                           
         DC    AL1(L'SRTCLI-1)                                                  
         DC    SL2(ACLILST)                                                     
         DC    AL1(SRTORILN-L'SRTORG1)                                          
         DC    AL1(SRTORILN)                                                    
         SPACE 1                                                                
         DC    AL1(OFFLEV),AL1(SRTOFCLN),AL1(0)                                 
         DC    CL12'OFFICE    ',(4*8)C' '                                       
         DC    AL2(NEWOFF-ACR402),AL2(LPRT-ACR402),AL1(0),AL2(0)                
         DC    CL12' '                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    SL2(0)                                                           
         DC    AL1(SRTOFCLN-L'SRTOFC)                                           
         DC    AL1(SRTOFCLN)                                                    
         SPACE 1                                                                
         DC    AL1(OFGLEV),AL1(SRTOFGLN),AL1(0)                                 
         DC    CL12'OFFICE GROUP',(4*8)C' '                                     
*        DC    AL2(NEWOFG-ACR402),AL2(LPRT-ACR402),AL1(0),AL2(0)                
*        NO OFFICE GROUP TOTALS                                                 
         DC    AL2(NEWOFG-ACR402),AL2(BRRE-ACR402),AL1(0),AL2(0)                
         DC    CL12' '                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    SL2(0)                                                           
         DC    AL1(SRTOFGLN-L'SRTOFG)                                           
         DC    AL1(SRTOFGLN)                                                    
         SPACE 1                                                                
TOTHIGH  DC    AL1(REPLEV),XL1'01',AL1(0)                                       
         DC    CL12'REPORT    ',(4*8)C' '                                       
         DC    AL2(BRRE-ACR402),AL2(BRRE-ACR402),AL1(0),AL2(0)                  
         DC    CL12' '                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    SL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
TOTNUM   EQU   (*-TOTTBL)/TOTTBLLN     NUMBER OF LEVELS IN THE TABLE            
NUMBUCKS EQU   4                                                                
EMPLEV   EQU   0                                                                
WCODELEV EQU   1                                                                
JOBLEV   EQU   2                                                                
PRODLEV  EQU   3                                                                
CLILEV   EQU   4                                                                
ORILEV   EQU   5                                                                
OFFLEV   EQU   6                                                                
OFGLEV   EQU   7                                                                
REPLEV   EQU   8                                                                
         SPACE 1                                                                
*--------------------------------------------------------------------*          
*        TABLE OF BUCKET OFFSETS                                                
*                                                                               
OFFSETS  DC    AL1(51),AL1(66),AL1(78),AL1(93),AL1(105),AL1(120)                
         EJECT                                                                  
*--------------------------------------------------------------------*          
*                    LITERALS                                                   
*--------------------------------------------------------------------*          
         LTORG                                                                  
*--------------------------------------------------------------------*          
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              HEADHOOK - CALLED FROM ACPRINT                                   
*--------------------------------------------------------------------*          
HDHOOK   NMOD1 0,*HEADHK                                                        
         L     R2,=A(BOXRC)                                                     
         ICM   RC,15,0(R2)                                                      
         SPACE 1                                                                
         MVI   HEAD9,X'FA'         GET RID OF THAT PESKY MIDLINE                
         XC    HEAD9+1(49),HEAD7+1                                              
         MVI   HEAD9+50,X'EB'                                                   
         MVC   HEAD4+14(1),HDOFG                                                
         MVC   HEAD4+18(35),HDOFGNM                                             
         MVC   HEAD5+14(2),HDOFF                                                
         MVC   HEAD5+18(36),HDOFFNM                                             
         MVC   HEAD4+46(37),DETLEVEL                                            
         MVC   HEAD8+2(25),ORIGPRT CLIENTS OF ...                               
         MVC   HEAD5+52(25),ORIGPRT                                             
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         SPACE 1                                                                
         MVI   BOXROWS+6,C'T'      SET ROWS                                     
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+56,C'B'                                                  
         SPACE 1                                                                
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+50,C'C'                                                  
         MVI   BOXCOLS+77,C'C'                                                  
         MVI   BOXCOLS+104,C'C'                                                 
         MVI   BOXCOLS+131,C'R'                                                 
         SPACE 1                                                                
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              BOXHOOK - CALLED FROM DDPRINT                                    
*--------------------------------------------------------------------*          
BXHOOK   NMOD1 0,*BHOOK                                                         
         L     R2,=A(BOXRC)                                                     
         ICM   RC,15,0(R2)                                                      
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         SPACE 1                                                                
         CLI   4(R1),8             START ROW 8 AT 50                            
         BNE   BOX20                                                            
         MVI   BOXCOLS,C'V'                                                     
         MVI   BOXCOLS+50,C'L'                                                  
         SPACE 1                                                                
BOX20    CLI   4(R1),9                                                          
         BNE   BOXX                                                             
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+50,C'C'                                                  
         MVI   BOXCOLS+61,C'D'     SUB BOXES GET A DASH                         
         MVI   BOXCOLS+88,C'D'                                                  
         MVI   BOXCOLS+115,C'D'                                                 
         SPACE 1                                                                
BOXX     XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*--------------------------------------------------------------------           
*        BUFFALO CSECT                                                          
*--------------------------------------------------------------------           
         BUFF  LINES=140,ROWS=1,COLUMNS=2,FLAVOR=PACKED,KEYLIST=(3,A),CX        
               OMMENT=5                                                         
*--------------------------------------------------------------------           
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        TABLES OF 1R NAME DATA, KEPT AT EACH LEVEL                             
*        TABLE IS-H(NUMBER IN TABLE), Y(MAX IT TABLE), C(TABLE DATA)            
*        TABLE DATA IS COVERED BY LISTD DSECT, BUILT BY BUILDLST                
*--------------------------------------------------------------------*          
*--------------------------------------------------------------------*          
*        MAINTAB IS TABLE OF HOW GETMAIN CORE SHOULD BE SPLIT UP                
*        COVERED BY MAIND                                                       
*--------------------------------------------------------------------*          
MAINTAB  DS    0F                                                               
         DC    S(AEMPLST)                                                       
         DC    Y(5)                                                             
         DC    A(EMPMAX)                                                        
         DC    A(EMPSIZE)                                                       
         SPACE 1                                                                
         DC    S(ACLILST)                                                       
         DC    Y(1)                                                             
         DC    A(CLIMAX)                                                        
         DC    A(CLISIZE)                                                       
         SPACE 1                                                                
         DC    S(APROLST)                                                       
         DC    H'0'                                                             
         DC    A(PROMAX)                                                        
         DC    A(PROSIZE)                                                       
         SPACE 1                                                                
         DC    S(AJOBLST)                                                       
         DC    H'0'                                                             
         DC    A(JOBMAX)                                                        
         DC    A(JOBSIZE)                                                       
         SPACE 1                                                                
         DC    S(AOFFLST)                                                       
         DC    H'0'                                                             
         DC    A(OFFMAX)                                                        
         DC    A(OFFSIZE)                                                       
         SPACE 1                                                                
         DC    S(AOFGLST)                                                       
         DC    H'0'                                                             
         DC    A(OFGMAX)                                                        
         DC    A(OFGSIZE)                                                       
MAINNUM  EQU   (*-MAINTAB)/MAINLEN                                              
         SPACE 1                                                                
EMPSIZE  EQU   (EMPMAX*LISTDLN)+HEADLN                                          
EMPMAX   EQU   4500                                                             
         SPACE 1                                                                
CLISIZE  EQU   (CLIMAX*LISTDLN)+HEADLN                                          
CLIMAX   EQU   2200                                                             
         SPACE 1                                                                
PROSIZE  EQU   (PROMAX*LISTDLN)+HEADLN                                          
PROMAX   EQU   4000                                                             
         SPACE 1                                                                
JOBSIZE  EQU   (JOBMAX*LISTDLN)+HEADLN                                          
JOBMAX   EQU   20000                                                            
         SPACE 1                                                                
OFFSIZE  EQU   (OFFMAX*LISTDLN)+HEADLN                                          
OFFMAX   EQU   255                                                              
         SPACE 1                                                                
OFGSIZE  EQU   (OFGMAX*LISTDLN)+HEADLN                                          
OFGMAX   EQU   40                                                               
         SPACE 4                                                                
BUFSIZE  EQU   EMPSIZE+CLISIZE+PROSIZE+JOBSIZE+OFFSIZE+OFGSIZE                  
         EJECT                                                                  
*        TABLE OF VALID OFICES FOR THIS REQUEST                                 
OFFTAB   DS    (OFFMAX*2)C                                                      
*                                                                               
*        ALL VALID 1 CHARACTER OFFICES                                          
OFFCHAR  DC    CL72'A B C D E F G H I J K L M N O P Q R S T U V W X Y ZX        
                1 2 3 4 5 6 7 8 9 0 '                                           
         EJECT                                                                  
*              DSECT FOR STORAGE AREA                                           
         SPACE 1                                                                
ACR4D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
ACREC    DS    A                                                                
ABUFC    DS    A                                                                
AOFFTAB  DS    A                                                                
AOFFCHAR DS    A                                                                
SORTER   DS    V                                                                
UNDERLIN DS    V                                                                
ACLIST   DS    V                                                                
CENTER   DS    V                                                                
*                                                                               
AEMPLST  DS    A                                                                
ACLILST  DS    A                                                                
APROLST  DS    A                                                                
AJOBLST  DS    A                                                                
AOFFLST  DS    A                                                                
AOFGLST  DS    A                                                                
         SPACE 1                                                                
ADBOX    DS    A                                                                
SAVERE   DS    A                                                                
ALOWTAB  DS    A                   ADDRESS OF LOWEST TABLE I NEED               
ABUFF    DS    A                   ADDRESS OF GETMAINED BUFFER                  
NUMNON   DS    CL1                                                              
COMMAND  DS    CL6                                                              
ELCODE   DS    CL1                                                              
OFFOFF   DS    CL1                 OFFSET OF OFFICE                             
OFFLEN   DS    CL1                 LENGTH OF OFFICE                             
OFGSTAT  DS    CL1                 Y, USING OFFICE GROUPS                       
DETLEVEL DS    CL37                HEAD DETAIL LEVEL PRINT                      
ORIGPRT  DS    CL25                                                             
DETMASK  DS    CL1                 PRINT MASK FOR DETAILS                       
LISTSW   DS    CL1                 IF WE HAVE A CLIENT LIST, CHECK IT           
MYMEND   DS    CL3                 MOS START                                    
MYMSTR   DS    CL3                 MOS END                                      
BILLED   DS    CL1                 FULLY BILLED SWITCH                          
STRDATE  DS    CL3                 START DATE PACKED YMD                        
ENDDATE  DS    CL3                 END DATE PACKED YMD                          
BILDATE  DS    CL3                 BILLING DATE PACKED YMD                      
MOS      DS    CL2                 MONTH OF SERVICE FROM SJ TRANS               
M1START  DS    PL2                 MOS START DATE PACKED                        
M1END    DS    PL2                 MOS END DATE PACKED                          
CURMON   DS    CL3                 PACKED CURRENT MONTH                         
CURMONP  DS    CL6                 PRINT CURRENT MONTH                          
FRMMONP  DS    CL6                 PRINT "FROM" MONTH                           
TRANSTAT DS    CL1                                                              
GOTACC   EQU   1                   I HAVE SAVED ACCOUNT DATA                    
GOTHIST  EQU   2                   I SAVED HISTORY DATA                         
OTHERS   EQU   4                   THIS TRAN IS ACROSS OFFICES                  
GOTLEVA  EQU   8                                                                
GOTLEVB  EQU   16                                                               
         SPACE 1                                                                
HDOFG    DS    CL1                 SJ OFFICE CODE FOR HEADERS                   
HDOFGNM  DS    CL36                SJ OFFICE NAME                               
HDOFF    DS    CL2                 SJ OFFICE CODE FOR HEADERS                   
HDOFFNM  DS    CL36                SJ OFFICE NAME                               
         SPACE 1                                                                
SAVEDATE DS    CL2                 MDATE                                        
SAVETRDT DS    CL3                 TRNSDATE                                     
SAVEHOUR DS    PL(BUCKLN)                                                       
SAVERATE DS    PL(BUCKLN)                                                       
SUMTOTAL DS    4PL(BUCKLN)                                                      
         SPACE 1                                                                
PL16     DS    PL16                FOR MULTIPLYING RATE                         
TOTHRS   DS    PL8                 ALL EMPLOYEES TOTALS                         
TOTREV   DS    PL8                                                              
         SPACE 1                                                                
ALSORT   DS    A                   A(LAST SORT RECORD)                          
*                                  WORK AREA FOR SORT RECORD                    
SRTREC   DS    0C                  NOTE ONLY 2 BUCKETS ARE USED WHEN            
*                                  PUT TO SORT, EXPANDED TO 4 WHEN GOT          
         DS    (SRTKYLN)C          KEY                                          
         DS    4PL(BUCKLN)         BUCKETS                                      
SRTLNQ   EQU   *-SRTREC                                                         
THISREC  DS    (SRTLNQ)C           WORK AREA FOR LAST RECORD                    
THISOFGC DS    0CL3                                                             
THISOFG  DS    CL1                                                              
THISOFC  DS    CL2                                                              
SAVETYPE DS    CL1                                                              
PRTSTA   DS    CL1                                                              
SUMMARY  EQU   1                                                                
BUFREC   DS    0C                                                               
BUFKEY   DS    0CL3                                                             
BUFTYPE  DS    CL1                                                              
SUMTYPLN EQU   *-BUFKEY                                                         
BUFACCT  DS    CL2                 SUB DEPARTMENT                               
BUFDATA  DS    CL5                 COMMENT DATA (KEY TO LOOK UP NAME            
BUFBUCKS DS    4PL8                                                             
BUFRCLN  EQU   *-BUFREC                                                         
         SPACE 1                                                                
LASTBUF  DS    CL(L'BUFKEY)        PREVIOUS BUFFALO KEY                         
REPSTAT  DS    CL1                                                              
MYKEY    DS    0CL49                                                            
MYCUL    DS    CL3                 COMPANY UNIT LEDGER                          
MYACCT   DS    CL12                ACCOUNT NUMBER                               
         ORG   MYKEY                                                            
         DS    CL49                KEY AREA                                     
         EJECT                                                                  
*              DSECT FOR SORT RECORD                                            
         SPACE 1                                                                
SRTD     DSECT                                                                  
SRTKEY   DS    0C                  ---- FORM A SORT ----*                       
SRTOFGC  DS    0CL3                                                             
SRTOFG   DS    CL1                 OFFICE GROUP OR 1R OFFICE                    
SRTOFGLN EQU   *-SRTKEY                                                         
SRTOFC   DS    CL2                 OFFICE OR 1R DEPT                            
SRTOFCLN EQU   *-SRTKEY                                                         
SRTORG1  DS    CL1                 X'00'- CLIENT IS FROM THIS OFFICE            
*                                  X'01'- CLIENT IS FROM OTHER OFFICES          
SRTORILN EQU   *-SRTKEY                                                         
SRTCLI   DS    CL3                 CLIENT                                       
SRTCLILN EQU   *-SRTKEY                                                         
*                                  X'01' EMP IS FROM OTHER OFFICE               
SRTPROD  DS    CL3                          -PRODUCT                            
SRTPROLN EQU   *-SRTKEY                                                         
SRTJOB   DS    CL6                          -JOB                                
SRTJOBLN EQU   *-SRTKEY                                                         
SRTWC    DS    CL2                 WORKCODE/TASK BREAKDOWN                      
SRTWCLN  EQU   *-SRTKEY                                                         
SRTORG2  DS    CL1                 X'00' EMPLOYEE IS FROM THIS OFFICE           
SRTEMPL  DS    CL12                EMPLOYEE'S  1R ACCOUNT                       
SRTEMPLN EQU   *-SRTKEY                                                         
SRTKYLN  EQU   *-SRTKEY            SORT KEY LENGTH                              
SRTBUCKS DS    0C                  LOCATION OF BUCKETS                          
BUCKLN   EQU   8                   LENGTH OF BUCKETS                            
SRTHOUR  DS    PL(BUCKLN)          HOUR                                         
SRTREV   DS    PL(BUCKLN)          REVENUE                                      
SBUKCONT EQU   (*-SRTBUCKS)/(BUCKLN) NUMBER OF BUCKETS                          
SRTRECL  EQU   *-SRTD                                                           
         EJECT                                                                  
*------------------------*                                                      
* DSECT FOR LIST TABLE                                                          
*------------------------*                                                      
LISTD    DSECT                                                                  
LISTREC  DS    0C                                                               
LISTKEY  DS    CL12                                                             
LISTNAME DS    CL36                                                             
LISTDLN  EQU   *-LISTD                                                          
         SPACE 2                                                                
*------------------------*                                                      
* DSECT FOR SUBTOTAL TABLE                                                      
*------------------------*                                                      
TOTTBLD  DSECT                                                                  
TOTLEV   DS    AL1                 SUB TOTAL LEVEL                              
TOTLEN   DS    AL1                 LENGTH OF THIS LEVEL IN SORT KEY             
TOTDISP  DS    AL1                 DISPLA OF THIS LEVEL IN SORT KEY             
TOTTYPE  DS    CL12                SUB TOTAL TYPE                               
TOTBUCKS DS    0C                  A(FIRST BUCKET)                              
         DS    (BUCKLN)P                                                        
         DS    (BUCKLN)P                                                        
         DS    (BUCKLN)P                                                        
         DS    (BUCKLN)P                                                        
TOTFIRST DS    AL2                 ADDRESS OF FIRST FOR ROUTINE                 
TOTLAST  DS    AL2                 ADDRESS OF LAST FOR ROUTINE                  
TOTWANT  DS    AL1                 SET AS PER REQUEST OPTIONS                   
TOTNEED  DS    AL1                 DO I NEED TO PRINT SUBTOTS                   
TOTDET   DS    AL1                 THIS LEVEL IS DETAIL ON THE REPORT           
TOTACCT  DS    CL12                ACCOUNT THIS TOTAL IS FOR                    
TOTDOFF  DS    AL1                 IF THIS IS THE LOWEST LEVEL OF TOTAL         
TOTDLEN  DS    AL1                 THE TOTDXXX FIELDS DEFINE WHERE TO           
TOTDTBL  DS    SL2                 GET THE DETAIL DATA                          
TOTKOFF  DS    AL1                 WHEN DOING FIRSTS FOR THIS LEVEL             
TOTKLEN  DS    AL1                 THESE HELP YOU EXTRACT THE DATA              
TOTTBLLN EQU   *-TOTTBLD                                                        
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DSECT FOR MAIN TAB, A TABLE WHICH LOOPS THRU THE STORAGE GETMAIN              
*        GETS                                                                   
*-------------------------------------------------------------------*           
MAIND    DSECT                                                                  
MAINAST  DS    S                   ADDRESS TO STORE A(TABLE)                    
MAINSIG  DS    H                   DISPLACEMENT INTO KEY OF DATA                
MAINMAX  DS    A                                                                
MAINSIZE DS    A                                                                
MAINLEN  EQU   *-MAIND                                                          
*                                                                               
*                                                                               
HEADD    DSECT                     HEADER BEFORE EACH NAME TABLE                
         DS    0H                                                               
HEADER   DS    0CL6                                                             
         DS    H                                                                
HEADMAX  DS    H                                                                
HEADSIG  DS    H                                                                
HEADLN   EQU   *-HEADER                                                         
*                                                                               
         EJECT                                                                  
ACR402   CSECT                                                                  
         SPACE 1                                                                
         ENTRY RECORD                                                           
RECORD   DS    0D                  DATAMRG AREA                                 
         DS    CL42                KEY                                          
         DS    CL2000              DATA                                         
         SPACE 1                                                                
*        DDLOGOD                                                                
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACMASTD                                                                
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
*        DDCNTRL                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDBOXEQUS                                                              
*        DDREMOTED                                                              
GETOPTD  DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110ACREPR402 03/23/15'                                      
         END                                                                    
