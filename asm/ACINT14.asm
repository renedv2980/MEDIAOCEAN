*          DATA SET ACINT14    AT LEVEL 018 AS OF 05/05/08                      
*PHASE T61914A                                                                  
         TITLE 'T61914 - ESTIMATE POSTING ADJUSTMENT'                           
T61914   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDEND-MYD),T61914**,R7                                         
         LR    R4,RC                                                            
         USING MYD,R4                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE KEY MODE?                           
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     OKEXIT                                                           
         SPACE 1                                                                
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC             VALIDATE RECORD                              
         BAS   RE,PROCPF           CHECK PF KEYS                                
         B     RESETKEY                                                         
         SPACE 1                                                                
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY             DISPLAY KEY                                  
         BAS   RE,VKEY                                                          
         B     RESETKEY                                                         
         SPACE 1                                                                
MODE6    CLI   MODE,DISPREC                                                     
         BNE   OKEXIT                                                           
         BAS   RE,DREC             DISPLAY RECORD                               
         SPACE 1                                                                
RESETKEY MVC   KEY,SAVEKEY                                                      
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                          DISPLAY KEY                                *         
***********************************************************************         
         SPACE 1                                                                
DKEY     NTR1                                                                   
         L     R6,AIO                                                           
         USING ACINKEY,R6                                                       
         CLI   ACINCOD,ACINEQU     INTERAGNECY RECORDS X'2D'                    
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    ADJRCVH+6,X'80'                                                  
         MVC   ADJRCV,ACINACC      A.O.R AGENCY                                 
         OI    ADJCLTH+6,X'80'                                                  
         MVC   ADJCLT,ACINCLT      OUR CLIENT                                   
         OI    ADJPRDH+6,X'80'                                                  
         MVC   ADJPRD,ACINPRD      AND PRODUCT                                  
         OI    ADJMEDH+6,X'80'                                                  
         MVC   ADJMED(L'ACINMED),ACINMED                                        
         OI    ADJESTH+6,X'80'                                                  
         MVC   ADJEST,ACINEST      SIX DIGIT ESTIMATE NUMBER                    
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           VALIDATE KEY                              *         
***********************************************************************         
         SPACE 1                                                                
VKEY     NTR1                                                                   
         L     R5,AIO4                                                          
         USING LOCALD,R5                                                        
         LA    RE,LOCALD           RECEIVING FIELD                              
         LA    RF,LOCALLN          RECEIVING FIELD LENGTH                       
         SR    R1,R1               BITS 0-7 PAD FACTOR,8-32 LENGTH SEND         
         ICM   R1,8,=X'40'         INSERT 40 AS PADDING FACTOR                  
         MVCL  RE,R0                                                            
         SPACE 1                                                                
         LA    R2,ADJRCVH          VALIDATE ACCOUNT                             
         MVC   CUL+1(2),RECVLEDG                                                
         OI    ADJRCVH+6,X'80'                                                  
         MVI   OPTION,C'Y'         PASS NAME                                    
         GOTO1 VALACCT                                                          
         MVC   RECVACT(L'CUL),CUL                                               
         MVC   RECVACT+L'CUL(L'RECVACT-L'CUL),RECCODE                           
         MVC   RECVNAM,RCVNAME                                                  
         SPACE 1                                                                
         LA    R2,ADJCLTH          CLIENT                                       
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C' '          DOES CLIENT START WITH A BLANK ?             
         BE    ERREXIT             YES, THIS IS AN ERROR                        
         OI    ADJCLTH+6,X'80'                                                  
         SPACE 1                                                                
         MVC   CUL+1(2),PRODLEDG                                                
         GOTO1 VALCLI              VALIDATE CLIENT                              
         MVC   CLI,CLICODE         SAVE CLIENT CODE FOR POSTING                 
         MVC   DUEDAYS,EFFDUEDA    EFFECTIVE + DUE DAYS                         
         SPACE 1                                                                
         LA    R2,ADJPRDH          PRODUCT                                      
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C' '          DOES PRODUCT START WITH A BLANK ?            
         BE    ERREXIT             YES, THIS IS AN ERROR                        
         OI    ADJPRDH+6,X'80'                                                  
         SPACE 1                                                                
         GOTO1 VALPROD             VALIDATE PRODUCT                             
         MVC   PRD,PRODCODE        SAVE PRODUCT CODE FOR POSTING                
         MVC   PRODNAM,PRONAME     SAVE PRODUCT NAME                            
         MVC   OFFCODE,EFFOFFC     SAVE CLI/PRD OFFICE CODE                     
         SPACE 1                                                                
         TM    COMPSTA1,X'10'      IS COMPANY ON COSTING                        
         BZ    *+16                NO - SKIP 1C STUFF                           
         MVC   ACCT1C,COSTCODE     SAVE 1C COSTING CODE FOR POSTING             
         MVC   NAM1C,COSTNAME      SAVE 1C NAME                                 
         SPACE 1                                                                
         LA    R2,ADJMEDH          MEDIA                                        
         OI    ADJMEDH+6,X'80'                                                  
         GOTO1 VALMED              VALIDATE MEDIA                               
         MVC   INCMACT,MEDIACOD    SAVE INCOME ACCOUNT FOR POSTING              
         MVC   INCMNAM,MEDNAME                                                  
         MVC   MEDCODE,MEDIA                                                    
         SPACE 1                                                                
         TM    COMPSTA1,X'10'      IS COMPANY ON COSTING                        
         BZ    VKEY03              NO - SKIP 12 STUFF                           
         MVC   ACCT12,A12CODE      SAVE 12 COSTING CODE FOR POSTING             
         MVC   NAM12,A12NAME       SAVE 12 NAME                                 
         MVC   ACCT11,A11CODE      SAVE 11 COSTING CODE FOR POSTING             
         MVC   NAM11,A11NAME       SAVE 11 NAME                                 
         SPACE 1                                                                
VKEY03   LA    R2,ADJESTH          ESTIMATE NUMBER                              
         OI    ADJESTH+6,X'80'                                                  
         GOTO1 VALEST                                                           
         MVC   ESTNUMB,ESTIMATE    SAVE ESTIMATE FOR POSTING                    
         MVC   ESTDESC,ESTNAME                                                  
         SPACE 1                                                                
         MVC   KEY,SPACES          CLEAR KEY                                    
         MVC   CUL+1(L'RECVLEDG),RECVLEDG                                       
         LA    R6,KEY                                                           
         USING ACINKEY,R6                                                       
         MVI   ACINCOD,ACINEQU                                                  
         MVI   ACINSREC,ACINSEQU                                                
         MVC   ACINCUL,CUL                                                      
         MVC   ACINACC,RECCODE                                                  
         MVC   ACINCLT,CLICODE                                                  
         MVC   ACINPRD,PRODCODE                                                 
         MVC   ACINMED,MEDIA                                                    
         MVC   ACINEST,ESTIMATE                                                 
         MVC   SAVEKEY,KEY                                                      
         B     OKEXIT                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                        DISPLAY RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
DREC     NTR1                                                                   
         L     R6,AIO                                                           
         OI    ADJMEDH+6,X'80'               TRANSMIT                           
         MVC   ADJMED,SPACES                                                    
         MVC   ADJMED(L'ACINMED),(ACINMED-ACINKEY)(R6)                          
         SPACE 1                                                                
         TM    (ACSTATUS-ACKEYD)(R6),X'02'   IS MEDIA AN MI RECORD?             
         BZ    DREC01                                                           
         SPACE 1                                                                
         MVC   ADJMED(3),=C'MI='                                                
         MVC   ADJMED+3(L'ACINMED),(ACINMED-ACINKEY)(R6)                        
         MVI   ADJMEDH+5,5                   MAKE SURE LENGTH IS 5              
         MVC   SAVEKEY,0(R6)                 SAVE KEY INCLUDING STATUS          
         SPACE 1                                                                
DREC01   MVI   ELCODE,ACIPFEQU               GET PROFILE ELEMENT                
         MVI   ERROR,NOEL                                                       
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
         SPACE 1                                                                
         USING ACINPRFD,R6                                                      
         SPACE 1                                                                
*                                            ESTIMATE PERIOD                    
         OI    ADJPERTH+6,X'80'              TRANSMIT                           
         MVC   ADJPERT,=C'EST. PERIOD'                                          
         SPACE 1                                                                
         OI    ADJPERH+6,X'80'                                                  
         SPACE 1                                                                
         MVC   WORK(2),ACIPFPRS                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,ADJPER)                                  
         MVI   ADJPER+6,C'-'                                                    
         MVC   WORK(2),ACIPFPRE                                                 
         MVI   WORK+2,1                                                         
         GOTO1 (RF),(R1),(1,WORK),(6,ADJPER+7)                                  
         SPACE 1                                                                
         MVI   ELCODE,ACIESEQU               GET ESTIMATE ELEMENT               
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
         SPACE 1                                                                
         LA    R0,TOTNUMB                    CLEAR SCREEN TOTAL ACCUMS          
         LA    R1,SCRENTOT                                                      
         ZAP   0(L'SCRENTOT,R1),=P'0'                                           
         LA    R1,L'SCRENTOT(R1)                                                
         BCT   R0,*-10                                                          
         SPACE 1                                                                
*                                          ALL MTHLY EST SCREEN LINES           
*                                          - VALIDIDATED                        
*                                          - MODIFIED                           
*                                          - PROTECTED                          
         SPACE 1                                                                
         LA    R1,ADJMONH                  1ST MONTHLY FIELD                    
         LA    R0,ADJENDH                  LAST SCREEN FIELD                    
         SR    RF,RF                                                            
         SPACE 1                                                                
DREC01A  OI    4(R1),X'20'                 MARK VALIDATED                       
         NI    1(R1),X'FF'-X'08'-X'01'     TURN OFF HI INTENS/MODIFIED          
         OI    1(R1),X'20'                 PROTECTED                            
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         CR    R1,R0                                                            
         BNH   DREC01A                                                          
         SPACE 1                                                                
         USING ACINESTD,R6                                                      
         USING SCREEND,R3                                                       
         LA    R3,ADJMONH          1ST MONTHLY SCREEN LINE                      
         SPACE 1                                                                
*                                 ALWAYS LEAVE THE FIRST RECORD FIELD           
*                                 UNPROTECTED OR GENCON WON'T RECOGNIZE         
*                                 A KEY FIELD CHANGE NEXT TIME IN.              
*                                 HE FAILS TO PASS YOU VALKEY MODE              
         SPACE 1                                                                
         NI    EPOSTDH+1,X'FF'-X'20'  UNPROTECT 1ST RECORD FIELD                
         MVI   NOPOST,C'N'            SET NOTHING POSTED SWITCH TO NO           
         SPACE 1                                                                
         GOTO1 VCLEARF,DMCB,ADJMONH,ADJENDH                                     
         GOTO1 (RF),(R1),(1,ADJMONH),ADJENDH                                    
         SPACE 1                                                                
*                                  *ADVERTISING MONTH*                          
*                                  HIGHLIGHT IF THE LINE IS ELIGIBLE            
*                                  TO BE ADJUSTED                               
*                                  UNPROTEST ELIGIBLE LINES AS WELL             
         SPACE 1                                                                
DREC02   DS    0H                                                               
         TM    ACIESTAT,X'80'                        HAS EST POSTED             
         BZ    DREC02A                               NO- LEAVE IT OFF           
         MVI   NOPOST,C'Y'                           SOMTHING FOR EST           
         OI    EMTHH+1,X'08'                         HI INTENSITY               
         NI    EPOSTDH+1,X'FF'-X'20'                 UNPROTECT                  
         NI    EGROSSH+1,X'FF'-X'20'                                            
         NI    EAORPH+1,X'FF'-X'20'                                             
         NI    ERCVPH+1,X'FF'-X'20'                                             
         NI    ERECVH+1,X'FF'-X'20'                                             
         SPACE 1                                                                
DREC02A  MVC   WORK(L'ACIESMTH),ACIESMTH                                        
         MVI   WORK+(L'ACIESMTH),1                                              
         GOTO1 DATCON,DMCB,(1,WORK),(6,EMTH)         EST ADV MONTH              
         SPACE 2                                                                
*                                  *POSTING DATE*                               
*                                  POSTING MMMDDYY IF EST NOT POSTED            
*                                  TODAY MMMDDYY FOR EST ADJUSTMENTS            
         SPACE 1                                                                
         OC    ACIESDTO,ACIESDTO                     POSTING DATES?             
         BZ    DREC03                                NO- NO DATCON              
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(2,ACIESDTO),(6,EPOSTD)   POSTING G/L MTH            
         TM    ACIESTAT,X'80'                        HAS EST POSTED             
         BZ    DREC03                                NO -                       
         GOTO1 DATCON,DMCB,(1,TODAYP),(6,EPOSTD)     YES- USE PRESENT           
*                                                    MTH/YY                     
         SPACE 2                                                                
*                                  *GROSS BILLING*                              
         SPACE 1                                                                
DREC03   CURED ACIESGRS,(L'EGROSS,EGROSS),2,FLOAT=-                             
         AP    GROSSTOT,ACIESGRS                                                
         SPACE 1                                                                
*                                  *A.O.R AND REC PCT*                          
         CURED ACIESFEE,(L'EAORP,EAORP),2,FLOAT=-                               
         CURED ACIESRCV,(L'ERCVP,ERCVP),2,FLOAT=-                               
         SPACE 1                                                                
*                                  *RECEIVABLE ESTIMATE *                       
         CURED ACIESREC,(L'ERECV,ERECV),2,FLOAT=-                               
         AP    RECVETOT,ACIESREC                                                
         SPACE 2                                                                
         TM   ACIESTAT,X'80'       HAS ESTIMATE BEEN POSTED?                    
         BZ   DREC03A              NO SKIP POSTED COL                           
         SPACE 2                                                                
*                                  *POSTED AMOUNT*                              
         SPACE 1                                                                
         CURED ACIESREC,(L'EPOST,EPOST),2,FLOAT=-                               
         AP    POSTTOT,ACIESREC                                                 
         SPACE 1                                                                
*                                  *PAID AMOUNT*                                
DREC03A  DS    0H                                                               
         CURED ACIESPD,(L'EPAID,EPAID),2,FLOAT=-                                
         AP    PAIDTOT,ACIESPD                                                  
         SPACE 2                                                                
DREC04   LA    R3,ELINE(R3)        NEXT SCREEN LINE                             
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),ACIESEQU      NEXT EST ELEMENT                             
         BE    DREC02                                                           
         SPACE 1                                                                
*                                  TOTAL AND/OR PFKEY LINES                     
         SPACE 1                                                                
         LA    R1,ADJLLINH         HI INTENS                                    
         LA    R0,ADJENDH                                                       
         SR    RF,RF                                                            
         OI    1(R1),X'08'                                                      
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         CR    R1,R0                                                            
         BNH   *-12                                                             
         SPACE 1                                                                
         LA    R1,ADJLLINH LAST SCREEN LINE                                     
         MVC   EMTH-SCREEND(L'EMTH,R1),=C'PFKEYS'                               
         MVC   EPOSTD-SCREEND(L'EPOSTD,R1),=C'2=ES DIS'                         
         MVC   EGROSS-SCREEND(L'EGROSS,R1),=C'3=ES CHA   '                      
         OI    EPAIDH+6-SCREEND(R1),X'80'                                       
         MVC   EPAID-SCREEND(L'EPAID,R1),=C'12=RETURN '                         
          SPACE 1                                                               
         CR    R3,R1                                 LAST SCREEN LINE           
         BE    DREC05                                YES-CLEAR PF KEYS          
         BL    *+6                                   NO - SKIP A LINE           
         DC    H'0'                                                             
         SPACE 1                                                                
         LA    R3,ELINE(R3)                          SKIP A LINE                
         CR    R3,R1                                 LAST LINE NOW?             
         BNE   DREC06                                                           
         SPACE 1                                                                
DREC05   GOTO1 VCLEARF,DMCB,EMTHH,EPAIDH                                        
         GOTO1 (RF),(R1),(1,EMTHH),EPAIDH                                       
         SPACE 1                                                                
DREC06   OI    EMTHH+1,X'08'                         HI INTENS TOTALS           
         MVC   EMTH,=C'TOTALS'                                                  
         OI    EGROSSH+1,X'08'                                                  
         CURED GROSSTOT,(L'EGROSS,EGROSS),2,FLOAT=-    GROSS BILLING            
         OI    ERECVH+1,X'08'                                                   
         CURED RECVETOT,(L'ERECV,ERECV),2,FLOAT=-      RECEIVABLE EST           
         OI    EPAIDH+1,X'08'                                                   
         CURED PAIDTOT,(L'EPAID,EPAID),2,FLOAT=-                                
         OI    EPOSTH+1,X'08'                                                   
         CURED POSTTOT,(L'EPOST,EPOST),2,FLOAT=-                                
         SPACE 1                                                                
         LA    R2,ADJRCVH                                                       
         OI    GENSTAT2,USMYOK      TELL GENCON TO USE MY MESSAGE               
         MVC   CONHEAD(L'ADJUSMSG),ADJUSMSG                                     
         CLI   ADJUSTED,C'Y'        ANYTHING BEEN ADJUSTED?                     
         BE    DREC09                                                           
         SPACE 1                                                                
         MVC   CONHEAD(L'ADJOKMSG),ADJOKMSG                                     
         CLI   NOPOST,C'Y'          ANYTHING POSTED ?                           
         BE    *+10                                                             
         MVC   CONHEAD(L'NPOSTMSG),NPOSTMSG                                     
DREC09   B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
*                                                                     *         
*   **NOTE** CHANGES ARE ONLY ALLOWED TO ESTIMATES ALREADY POSTED     *         
***********************************************************************         
         SPACE 1                                                                
VREC     NTR1                                                                   
         L     R5,AIO4                                                          
         USING LOCALD,R5                                                        
         SPACE 1                                                                
*                                  RE VALIDATE THE MEDIA                        
         LA    R2,ADJMEDH          MEDIA                                        
         MVC   AIO,AIO2                                                         
         GOTO1 VALMED              RE -VALIDATE MEDIA                           
         MVC   INCMACT,MEDIACOD    SAVE INCOME ACCOUNT FOR POSTING              
         MVC   INCMNAM,MEDNAME                                                  
         MVC   ACCT12,A12CODE      SAVE 12 COSTING CODE FOR POSTING             
         MVC   NAM12,A12NAME       SAVE 12 NAME                                 
         MVC   ACCT11,A11CODE      SAVE 11 COSTING CODE FOR POSTING             
         MVC   NAM11,A11NAME       SAVE 11 NAME                                 
         MVC   AIO,AIO1            RESTORE AIO                                  
         SPACE 1                                                                
         XC    MEDSWITC,MEDSWITC                                                
         L     R1,AIO                                                           
         TM    (ACSTATUS-ACKEYD)(R1),X'02'                                      
         BZ    *+8                                                              
         MVI   MEDSWITC,C'Y'                                                    
         MVI   ELCODE,ACIPFEQU     ESTIMATE PROFILE ELEMENT                     
         MVI   ERROR,NOEL          NO ESTIMATE PROFILE ON RECORD                
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
         SPACE 1                                                                
         USING ACINPRFD,R6                                                      
*        MVC   SVTWAID,ACIPFTWA    SAVE USER ID FOR POSSIBLE POSTING            
         MVC   SVTWAID,TWAORIG     CURRENT USER ID SET BY BASE                  
         MVC   ESTDESC,ACIPFDES    SAVE ESTIMATE DESCRIPTION                    
         SPACE 1                                                                
         LA    R0,TOTNUMB                  CLEAR SCREEN TOTAL ACCUMS            
         LA    R1,SCRENTOT                                                      
         ZAP   0(L'SCRENTOT,R1),=P'0'                                           
         LA    R1,L'SCRENTOT(R1)                                                
         BCT   R0,*-10                                                          
         SPACE 1                                                                
         LA    R1,ADJMONH                  TRANSMIT ALL SCREEN LINES            
         LA    R0,ADJENDH                                                       
         SR    RF,RF                                                            
         SPACE 1                                                                
         OI    6(R1),X'80'                                                      
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         CR    R1,R0                                                            
         BNH   *-12                                                             
         SPACE 1                                                                
*                                  ***********************************          
*                                  * CHECK/EDIT ESTIMATE INFO CHANGES*          
*                                  *                                 *          
*                                  * R6 - MONTHLY ESTIMATE ELEMENT   *          
*                                  * R3 - CORRESPONDING SCREEN LINE  *          
*                                  *                                 *          
*                                  ***********************************          
         USING ACINESTD,R6                                                      
         USING SCREEND,R3                                                       
         MVI   ELCODE,ACIESEQU     CHECK FOR MONTHLY ESTIMATE ELEMENTS          
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
         SPACE 1                                                                
         LA    R3,ADJMONH          1ST SCREEN LINE                              
         MVI   ADJUSTED,C'N'                                                    
         SPACE 2                                                                
*                                  G/L MTH IS THE MMM/YY OF THE POSTING         
*                                  DATE FOR ESTIMATES POSTED BY AC08.           
*                                  FOR ESTIMATES ADJUSTED THRU THIS             
*                                  PROGRAM, THE CURRENT MTH IS USED.            
*                                  THE CURRENT MTH MAY BE OVERIDDEN.            
         SPACE 2                                                                
VREC03   TM   EGROSSH+1,X'20'      IS THE LINE PROTECTED?                       
         BZ   VREC04               NO- THEN VALIDATE THE FIELDS                 
         AP   GROSSTOT,ACIESGRS    YES- ADD THIS LINES GRS AND RECV             
         AP   RECVETOT,ACIESREC    INTO SCREEN TOTALS                           
         B    VREC12               GET NEXT EST ELEMENT/SCREEN LINE             
         SPACE 1                                                                
VREC04   LA    R2,EPOSTDH                                                       
         GOTO1 ANY                 IS THERE A POSTING GL MTH MMM/YY             
         SPACE 1                                                                
*                                                                               
*                                                                               
         USING BMONVALD,R1                                                      
         XC    BMONWRK,BMONWRK                                                  
         GOTO1 VBMONVAL,DMCB,EPOSTDH,(58,ACOMFACS),(0,BMONWRK),        X        
               (COMPANY,0)                                                      
         LA    R1,BMONWRK                                                       
         MVI   ERROR,MOALOCK                                                    
         TM    BMOERR,BMOELOKQ            LOCKED MOA ?                          
         BO    ERREXIT                    YES                                   
         MVI   ERROR,INVDATE                                                    
         TM    BMOERR,BMOEINVQ+BMOERNGQ   OUT OF RANGE OR INVALID DATE          
         BNZ   ERREXIT                    YES                                   
         MVC   MOSP(2),BMOMOSP                                                  
         MVI   MOSP+2,X'01'                                                     
*        GOTO1 DATVAL,DMCB,(0,EPOSTD),WORK                                      
*        GOTO1 DATCON,DMCB,(0,WORK),(1,MOSP)                                    
         DROP  R1                                                               
*&&DO                                                                           
         SR    R0,R0                                                            
         IC    R0,5(R2)            GET INPUT LENGTH                             
         MVI   ERROR,INVDATE                                                    
         GOTO1 PERVAL,DMCB,((R0),WORK),ELEMENT                                  
         TM    4(R1),X'01'                                                      
         BO    ERREXIT                                                          
         LA    R1,ELEMENT                                                       
         USING PERVALD,R1                                                       
         CLC   PVALNMNS,=H'1'      TEST SINGLE MONTH ONLY                       
         BNE   ERREXIT                                                          
         TM    PVALASSM,PVALASY    TEST YEAR WAS ASSUMED                        
         BO    ERREXIT                                                          
         MVC   MOSP,PVALPSTA                                                    
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,TODAYP),(0,DUB)   CHAR FOR ADDAY                  
         GOTO1 ADDAY,DMCB,(C'M',DUB),WORK,F'-2'      ALLOW 2 MTHS BACK          
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         CLC   MOSP(2),WORK+6                                                   
         BL    ERREXIT                                                          
         GOTO1 ADDAY,DMCB,(C'M',DUB),WORK,F'1'       ALLOW 1 MTH AHEAD          
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         CLC   MOSP(2),WORK+6                                                   
         BH    ERREXIT                                                          
*&&                                                                             
         MVI   ERROR,NOCHANGE                                                   
         TM    EPOSTDH+4,X'20'     HAS POSTING G/L MTH CHANGED?                 
         BO    VREC04A                                                          
         TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BZ    ERREXIT             NO- CHANGE OF G/L MTH NOT ALLOWED            
*        MVI   ERROR,MOALOCK                                                    
*        CLC   CMOSLOCK,MOSP       TO LATEST LOCKED MTH                         
*        BNL   ERREXIT                                                          
         OI    EPOSTDH+4,X'20'     MARK VALIDATED                               
         SR    R1,R1                                                            
         IC    R1,0(,R2)           GET INPUT LENGTH                             
         SHI   R1,8                                                             
         TM    1(R2),X'02'         EXTENDED FH                                  
         BZ    *+8                                                              
         SHI   R1,8                                                             
         BCTR  R1,0                                                             
         EXMVC R1,8(R2),SPACES                                                  
         OI    6(R2),X'80'         XMIT IT BACK                                 
         GOTO1 DATCON,DMCB,(1,MOSP),(9,8(R2))                                   
         SPACE 2                                                                
*                                  DECIDE WHICH FORMULA TO USE                  
*                                                                               
*                                  FORMULA 1  - GROSS X PCT = REC AMT           
*                                  (DEFAULT)  USED IF GROSS HAS CHANGED         
*                                                                               
*                                  FORMULA 2  - REC AMT / PCT = GROSS           
*                                             USED IF RECV AMT CHANGES          
         SPACE 1                                                                
VREC04A  MVI   FORMULA,1                                                        
         MVI   ERROR,NOCHANGE                                                   
         LA    R2,EGROSSH                                                       
         TM    EGROSSH+4,X'20'     HAS GROSS FIGURE BEEN CHANGED                
         BZ    VREC04B             YES-USE FORMULA 1                            
         SPACE 1                                                                
         LA    R2,ERECVH                                                        
         TM    ERECVH+4,X'20'      CHECK IF REC EST CHANGED                     
         BNZ   VREC06              NO  - USE DEFAULT FORMULA 1                  
         MVI   FORMULA,2           YES - USE FORMULA 2                          
         SPACE 1                                                                
VREC04B  TM    ACIESTAT,X'80'       HAS EST BEEN POSTED                         
         BZ    ERREXIT              NO - CHANGE NOT ALLOWED                     
         SPACE 1                                                                
         NI    EMTHH+4,X'FF'-X'20'  YES -TURN OFF LINE VALIDATION               
*                                   THIS SIGNIFIES A LINE CHANGE                
         SPACE 2                                                                
*                                  ** GROSS  ESTIMATE **                        
         SPACE 1                                                                
*                                  2 DECIMAL PLACES                             
VREC06   LA    R2,EGROSSH                                                       
         GOTO1 ANY                 VALIDATE DECIMAL                             
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R0)                                  
         CLI   DMCB,0                                                           
         BE    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     ERREXIT                                                          
         ZAP   DUB,DMCB+4(8)                                                    
         OI    EGROSSH+4,X'20'     MARK FIELD VALIDATED                         
         ZAP   GROSS,DUB           GROSS ESTIMATE                               
         SPACE 2                                                                
*                                  ** PERCENTS **                               
         ZAP   PCTTOTS,=P'0'                                                    
         MVI   ERROR,NOCHANGE                                                   
         LA    R2,EAORPH                                                        
         TM    EAORPH+4,X'20'      HAS AOR PCT BEEN CHANGED?                    
         BNZ   *+16                NO- CONTINUE                                 
         TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BZ    ERREXIT             NO-CHA NOT ALLOWED FROM THIS SCREEN          
         NI    EMTHH+4,X'FF'-X'20'  YES -TURN OFF LINE VALIDATION               
*                                   THIS SIGNIFIES A LINE CHANGE                
         BAS   RE,PERCENTS                                                      
         SPACE 1                                                                
         MVI   ERROR,NOCHANGE                                                   
         LA    R2,ERCVPH                                                        
         TM    ERCVPH+4,X'20'      HAS REC PCT BEEN CHANGED?                    
         BNZ   *+16                NO- CONTINUE                                 
         TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BZ    ERREXIT             NO- CHA NOT ALLOWED FROM THIS SCREEN         
         NI    EMTHH+4,X'FF'-X'20'  YES -TURN OFF LINE VALIDATION               
*                                   THIS SIGNIFIES A LINE CHANGE                
         BAS   RE,PERCENTS                                                      
         ZAP   PCT,DUB             RECEIVABLE PCT                               
         SPACE 2                                                                
*                                  ** RECEIVABLE ESTIMATE **                    
         LA    R2,ERECVH                                                        
         MVI   MAX,2               2 DECIMAL PLACES                             
         GOTO1 VALIDEC             VALIDATE DECIMAL                             
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         OI    ERECVH+4,X'20'      MARK AS VALIDATED                            
         ZAP   RECEIV,DUB          RECEIVABLE  ESTIMATE                         
         SPACE 1                                                                
         CLI   FORMULA,1                                                        
         BNE   VREC08                                                           
         ZAP   PK16,GROSS          GROSS X PCT = REC ESTIMATE                   
         MP    PK16,PCT                                                         
         SRP   PK16,64-4,5                                                      
         ZAP   RECEIV,PK16                                                      
         B     VREC10                                                           
         SPACE 1                                                                
VREC08   ZAP   PK16,RECEIV         RECEIVABLE / PCT = GROSS ESTIMATE            
         MVI   ERROR,INVALID                                                    
         LA    R2,ERCVPH                                                        
         CP    PCT,=P'0'                                                        
         BE    ERREXIT                                                          
         ZAP   DUB,PCT                                                          
         MP    PK16,=P'10000'                                                   
         DP    PK16,DUB                                                         
         ZAP   GROSS,PK16(8)                                                    
         SPACE 1                                                                
VREC10   DS    0H                                                               
         CURED GROSS,(L'EGROSS,EGROSS),2,FLOAT=-                                
         AP    GROSSTOT,GROSS                                                   
         CURED RECEIV,(L'ERECV,ERECV),2,FLOAT=-                                 
         AP    RECVETOT,RECEIV                                                  
         SPACE 1                                                                
VREC12   LA    R3,ELINE(R3)            NEXT SCREEN LINE                         
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                   NEXT ELEMENT                             
         CLI   0(R6),ACIESEQU                                                   
         BE    VREC03                                                           
         SPACE 1                                                                
*                                      TOTAL OR PFKEY LINES                     
         SPACE 1                                                                
         LA    R1,ADJLLINH         HI INTENS                                    
         LA    R0,ADJENDH                                                       
         SR    RF,RF                                                            
         OI    1(R1),X'08'                                                      
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         CR    R1,R0                                                            
         BNH   *-12                                                             
         SPACE 1                                                                
         LA    R1,ADJLLINH LAST SCREEN LINE                                     
         MVC   EMTH-SCREEND(L'EMTH,R1),=C'PFKEYS'                               
         MVC   EPOSTD-SCREEND(L'EPOSTD,R1),=C'2=ES DIS'                         
         MVC   EGROSS-SCREEND(L'EGROSS,R1),=C'3=ES CHA   '                      
         OI    EPAIDH+6-SCREEND(R1),X'80'                                       
         MVC   EPAID-SCREEND(L'EPAID,R1),=C'12=RETURN '                         
          SPACE 1                                                               
         CR    R3,R1                                 LAST SCREEN LINE           
         BE    VREC16                                YES-CLEAR PF KEYS          
         BL    *+6                                   NO - SKIP A LINE           
         DC    H'0'                                                             
         SPACE 1                                                                
         LA    R3,ELINE(R3)                          SKIP A LINE                
         CR    R3,R1                                 LAST LINE NOW?             
         BNE   VREC18                                                           
         SPACE 1                                                                
VREC16   GOTO1 VCLEARF,DMCB,EMTHH,EPAIDH                                        
         GOTO1 (RF),(R1),(1,EMTHH),EPAIDH                                       
         SPACE 1                                                                
VREC18   MVI   EGROSSH+5,L'EGROSS      MODIFIED                                 
         MVI   ERECVH+5,L'ERECV                                                 
         OI    EGROSSH+6,X'80'                                                  
         OI    ERECVH+6,X'80'                                                   
         CURED GROSSTOT,(L'EGROSS,EGROSS),2,FLOAT=-    GROSS BILLING            
         CURED RECVETOT,(L'ERECV,ERECV),2,FLOAT=-      RECEIVABLE EST           
         SPACE 1                                                                
         CLI   PFKEY,PF4               PF4 FOR TRANSACTION UPDATE               
         BE    VUPDAT                                                           
         SPACE 1                                                                
         LA    R2,ADJRCVH                                                       
         OI    GENSTAT2,USMYOK        TELL GENCON TO USE MY OK MESSAGE          
         MVC   CONHEAD(L'POSTMSG),POSTMSG                                       
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                      UPDATE EST ELEMENTS AND TRANSACTION           *          
***********************************************************************         
*                                                                               
VUPDAT   DS    0H                                                               
         MVI   ELCODE,ACIESEQU     MONTHLY ESTIMATE ELEMENTS                    
         MVI   ERROR,NOEL                                                       
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
         SPACE 1                                                                
*                                  ***********************************          
*                                  * R6 - MONTHLY ESTIMATE ELEMENT   *          
*                                  * R3 - CORRESPONDING SCREEN LINE  *          
*                                  ***********************************          
         LA    R2,UPDBLOCK                                                      
         LA    R3,UPDBLLNQ         CLEAR UPDBLOCK                               
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
         USING TRNBLKD,RF          R3=A(ADDTRN BLOCK)                           
         LA    RF,UPDBLOCK                                                      
         MVC   TRNCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   TRNCPYS1(4),COMPSTA1                                             
         MVC   TRNCPYS5,COMPSTA5                                                
         MVC   TRNCPYS6,COMPSTA6                                                
         MVC   TRNCPYS7,COMPSTA7                                                
         MVC   TRNCPYS8,COMPSTA8                                                
         MVC   TRNCPYS9,COMPSTA9                                                
         MVC   TRNCPYSA,COMPSTAA                                                
         MVC   TRNGLMOA,COMPGMOA                                                
*                                                                               
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
         DROP  RF                                                               
*                                  ** DR TO RECEIVABLES **                      
         USING ACINESTD,R6                                                      
         USING SCREEND,R3                                                       
         LA    R3,ADJMONH          SCREEN LINE BEGINS WITH ADV MONTH            
         SPACE 1                                                                
*                                  MAKE SURE SCREEN AND X'C7' JIVE              
         SPACE 1                                                                
VUPD02   GOTO1 DATVAL,DMCB,(2,EMTH),WORK                                        
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         CLC   ACIESMTH,WORK+6     MATCH ON ADVERTISING MONTH                   
         BNE   ERREXIT                                                          
         SPACE 1                                                                
         TM    EGROSSH+1,X'20'     IF LINE PROTECTED NOT INTERESTED             
         BNZ   VUPD90                                                           
         TM    ACIESTAT,X'80'      IF EST NOT POSTED NOT INTERESTED             
         BZ    VUPD90                                                           
         TM    EMTHH+4,X'20'       HAS LINE BEEN MODIFIED                       
         BNZ   VUPD90              NO - CHECK NEXT ELEMENT                      
         SPACE 1                                                                
         MVC   ADVMTH,ACIESMTH     SAVE THE ADV MTH FOR POSTING LATER           
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(1,TODAYP),(2,ACIESDAT)   NEW POSTING DATE           
         GOTO1 (RF),(R1),(2,ACIESDTO),(1,TDATEP)     TRANSACTION DATE           
*                                                    FROM ORIG POST DTE         
*        G/L MONTH OF SERVICE                                                   
*                                                                               
         GOTO1 DATVAL,DMCB,(2,EPOSTD),WORK                                      
         GOTO1 DATCON,DMCB,(0,WORK),(1,MOSP)  M.O.S.PACKED FOR ADDTRN           
         MVC   MOS(1),WORK+1                                                    
         MVC   MOS+1(1),WORK+3                                                  
         CLI   WORK+2,C'1'                                                      
         BNE   VUPD04                                                           
         MVI   MOS+1,C'A'                                                       
         CLI   WORK+3,C'0'                                                      
         BE    VUPD04                                                           
         MVI   MOS+1,C'B'                                                       
         CLI   WORK+3,C'1'                                                      
         BE    VUPD04                                                           
         MVI   MOS+1,C'C'                                                       
         SPACE 2                                                                
*                                  FIGURE OUT ADJUSTED GROSS                    
VUPD04   MVI   MAX,2                                                            
         LA    R2,EGROSSH                                                       
         GOTO1 ANY                 VALIDATE DECIMAL                             
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R0)                                  
         CLI   DMCB,0                                                           
         BE    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     ERREXIT                                                          
         ZAP   DUB,DMCB+4(8)                                                    
         ZAP   GROSS,DUB           NEW GRS - PREV POSTED GRS = ADJ GRS          
         SP    GROSS,ACIESGRS                                                   
         ZAP   ACIESGRS,DUB        SAVE NEW GROSS IN X'C7'                      
         SPACE 2                                                                
*                                  FIGURE OUT ADJUSTED RECEIVABLE AMT           
         LA    R2,ERECVH                                                        
         GOTO1 VALIDEC                                                          
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         ZAP   RECEIV,DUB          NEW REC AMT - PREV POSTED = ADJ RECV         
         SP    RECEIV,ACIESREC                                                  
         ZAP   ACIESREC,DUB        SAVE NEW RECEIVABLE AMT IN X'C7'             
         SPACE 1                                                                
         ZAP   PCTTOTS,=P'0'                                                    
         LA    R2,EAORPH           UPDATE A.O.R PCT IN X'C7'                    
         BAS   RE,PERCENTS                                                      
         ZAP   ACIESFEE,DUB                                                     
         SPACE 1                                                                
         LA    R2,ERCVPH           UPDATE RECV PCT IN X'C7'                     
         BAS   RE,PERCENTS                                                      
         ZAP   ACIESRCV,DUB                                                     
         SPACE 1                                                                
*                                  REMOVED 8/30/94 BECAUSE AGENCY WANTS         
*                                  TO ADJUST THE GROSS ON ZERO PSTING           
*****    CP    RECEIV,=P'0'        IF NO CHANGE IN RECEIVABLE AMNT              
*****    BZ    VUPD90              DON' T BOTHER TO ADJUST                      
*                                                                               
         MVC   POSTACT,RECVACT                                                  
         MVC   POSTCON,SPACES      CONTRA IS NAME OF SI OR MI RECORD            
         MVC   CONTNAM,SPACES      USE ALSO AS NAME OF CONTRA                   
         LA    R2,POSTCON+3                                                     
         GOTO1 CHOPPER,DMCB,(36,INCMNAM),(12,(R2)),(0,1)                        
         MVC   CONTNAM(L'POSTCON),POSTCON                                       
         MVI   STATIS,X'80'        DEBIT TO RECEIVABLES                         
         ZAP   AMOUNTP,RECEIV      POSTING AMOUNT                               
         BAS   RE,ADJUST           GO DO THE POSTINGS                           
         MVI   ADJUSTED,C'Y'       SET ADJUSTED SWITCH FOR USER MESSAGE         
         SPACE 1                                                                
*                                  ** CR TO INCOME **                           
         MVC   POSTACT,INCMACT                                                  
         MVC   POSTCON,SPACES                                                   
         MVC   POSTCON(L'COMPANY),COMPANY                                       
         MVC   POSTCON+L'COMPANY(L'PRODLEDG),PRODLEDG                           
         MVC   POSTCON+L'COMPANY+L'PRODLEDG(L'CLIPRD),CLIPRD                    
         MVC   CONTNAM,PRODNAM     PROD AS CONTRA NAME                          
         MVI   STATIS,X'00'        CREDIT TO INCOME                             
         ZAP   AMOUNTP,RECEIV      POSTING AMOUNT                               
         BAS   RE,ADJUST           GO DO THE POSTINGS                           
         SPACE 1                                                                
         BAS   RE,BHEAD            SETUP/UPDATE BATCHEADER RECORD               
         SPACE 1                                                                
         TM    COMPSTA1,X'10'      AGENCY ON COSTING?                           
         BZ    VUPD90              NO SKIP COST POSTINGS                        
         SPACE 1                                                                
         MVC   POSTACT,ACCT1C      ACCOUNT 1C                                   
         MVC   POSTCON,ACCT12      CONTRA 12                                    
         MVC   CONTNAM,NAM12       12 CONTRA NAME                               
         MVI   STATIS,X'80'        DEBIT TO 1C COSTING ACCOUNT                  
         ZAP   AMOUNTP,RECEIV      POSTING AMOUNT                               
         BAS   RE,ADJUST           GO DO THE POSTINGS                           
         MVC   POSTACT,ACCT12      ACCOUNT 12                                   
         MVC   POSTCON,ACCT1C      CONTRA 1C                                    
         MVC   CONTNAM,NAM1C       1C CONTRA NAME                               
         MVI   STATIS,X'00'        DEBIT TO 12 INCOME ANALYSIS                  
         BAS   RE,ADJUST           GO DO THE POSTINGS                           
         SPACE 1                                                                
         MVC   POSTACT,ACCT1C      ACCOUNT 1C                                   
         MVC   POSTCON,ACCT11      CONTRA 11                                    
         MVC   CONTNAM,NAM11       11 CONTRA NAME                               
         MVI   STATIS,X'80'        DEBIT TO 1C COSTING ACCOUNT                  
         ZAP   AMOUNTP,GROSS       POSTING AMOUNT                               
         BAS   RE,ADJUST           GO DO THE POSTINGS                           
         MVC   POSTACT,ACCT11      ACCOUNT 11                                   
         MVC   POSTCON,ACCT1C      CONTRA 1C                                    
         MVC   CONTNAM,NAM1C       1C CONTRA NAME                               
         MVI   STATIS,X'00'        CREDIT TO 11 BILLING ANALYSIS                
         BAS   RE,ADJUST           GO DO THE POSTINGS                           
         SPACE 1                                                                
VUPD90   MVC   AIO,AIO1            RESTORE AIO                                  
         LA    R3,ELINE(R3)        NEXT SCREEN LINE                             
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               NEXT ELEMENT                                 
         CLI   0(R6),ACIESEQU                                                   
         BE    VUPD02                                                           
*                                                                               
         USING TRNBLKD,RF          R3=A(ADDTRN BLOCK)                           
         LA    RF,UPDBLOCK                                                      
         MVI   TRNINDS2,TRNIUPDG   UPDATE GL POSTING                            
         OI    TRNINDS,TRNILAST    LAST TIME CALL TO ADDTRN                     
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,DREC             DISPLAY NEW RECORD                           
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                      ADD THE TRANSACTIONS WITH A D D T R N         *          
***********************************************************************         
*                                                                               
ADJUST   NTR1                                                                   
         MVC   AIO,AIO2            USE AIO2 TO ADD TRANSACTIONS                 
         L     R2,AIO                                                           
         LA    R3,2000             CLEAR AIO2                                   
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
         L     R6,AIO                                                           
         USING ACKEYD,R6                                                        
         XC    ACKEYACC(ACRECORD-ACKEYACC),ACKEYACC                             
         MVC   ACKEYACC(ACKEYREF-ACKEYACC),SPACES                               
         MVC   ACKEYACC,POSTACT                                                 
         MVC   ACKEYCON,POSTCON                                                 
         MVC   ACKEYDTE,TDATEP                                                  
         MVC   ACKEYREF,ESTNUMB                                                 
         MVC   ACLENGTH,=H'50'                                                  
*                                                                               
*                                  *** BUILD TRANSACTION ELEMENT ***            
         USING TRANSD,R6                                                        
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRNSEL,TRNSELQ      X'44'                                        
         MVI   TRNSLEN,TRANSLEN                                                 
         MVC   TRNSDATE,TDATEP     TRANSACTION DATE --DAY OF ORIG POST          
         MVC   TRNSREF,ESTNUMB     REFERENCE #      --ESTIMATE NUMBER           
         MVI   TRNSTYPE,58         TRANS TYPE       --58 INTERAGENCY            
         MVC   TRNSSTAT,STATIS     DEBIT OR CREDIT                              
*                                                                               
*                                  TRNSBTCH = YM'AJ'DD                          
         MVC   TRNSBTCH(L'MOS),MOS                                              
         MVC   TRNSBTCH+L'MOS(L'ADJ),ADJ                                        
         MVC   TRNSBTCH+L'MOS+L'ADJ(L'TRNSBTCH-L'MOS-L'ADJ),RCDATE+3            
         CLC   OFFCODE,SPACES                                                   
         BNL   *+10                                                             
         MVC   OFFCODE,SPACES                                                   
         MVC   TRNSOFFC,OFFCODE    OFFICE CODE                                  
         ZAP   TRNSAMNT,AMOUNTP    ADJUSTMENT AMOUNT (OR GROSS FOR 11)          
         GOTO1 DATCON,DMCB,(1,TODAYP),(5,ADJNARRA+23)                           
         MVC   WORK(L'ADVMTH),ADVMTH   ADV MTH INTO NARRATIVE                   
         MVI   WORK+(L'ADVMTH),0                                                
         GOTO1 DATCON,DMCB,(1,WORK),(6,ADJADMTH)                                
         MVC   TRNSNARR(L'ADJNARRA),ADJNARRA                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         CLI   POSTACT+1,C'1'      IS THIS AN ANALYSIS POSTING (UNIT 1)         
         BE    ADJ04               YES - SKIP X'50' X'61' X'1A'                 
*                                                                               
         USING TRCASHD,R6          GROSS MEMO X'50'                             
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'G'                                                    
         ZAP   TRCSAMNT,GROSS                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
         USING TRDUED,R6           DUE DATE X'61'                               
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRDUEL,TRDUELQ                                                   
         MVI   TRDUEN,TRDULNQ                                                   
         ZIC   R3,DUEDAYS         NUMBER OF DAYS TO ADD TO TRNS DATE            
         GOTO1 DATCON,DMCB,(1,TDATEP),(0,DUB)   TRANS DT AS YYMMDD              
         GOTO1 ADDAY,DMCB,DUB,WORK,(R3)         + DUE DAYS                      
         GOTO1 DATCON,DMCB,(0,WORK),(2,TRDUDATE) DUE DATE COMPRESSED            
         GOTO1 ADDELEM                                                          
*                                                                               
         USING ACMTD,R6            MEDIA TRANSFER ELEMENT X'1A'                 
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACMTEL,ACMTELQ                                                   
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVC   ACMTMED,SPACES                                                   
         MVC   ACMTCLI,CLI                                                      
         MVC   ACMTPRD,PRD                                                      
         MVC   ACMTMOS,ADVMTH      ADVERTISING MONTH                            
         MVC   ACMTEST,ESTNUMB                                                  
         MVC   ACMTMED2,MEDCODE    INTERAGENCY MEDIA CODE                       
         CLI   MEDSWITC,C'Y'                                                    
         BNE   *+8                                                              
         OI    ACMTSTAT,X'02'      MARK AS MI=                                  
         MVI   ACMTSYS,C'I'        INTERAGENCY                                  
         MVC   ACMTDSCP,ESTDESC    ESTIMATE DESCRIPTION                         
         ZAP   DUB,GROSS           GROSS AMOUNT                                 
**                                                                              
         CP    DUB,=P'2100000000'  IF OVER $21,000,000.00                       
         BNH   *+10                                                             
         ZAP   DUB,=P'0'           THE CVB INSTRUCTION IS BROKEN                
         CP    DUB,=P'-2100000000'   - $21,000,000.00                           
         BNL   *+10                                                             
         ZAP   DUB,=P'0'                                                        
*                                                                               
         CVB   R1,DUB                                                           
         STCM  R1,15,ACMTGRS                                                    
         ZAP   DUB,RECEIV          RECEIVABLE AMOUNT IS ALSO INCOME AMT         
         CVB   R1,DUB                                                           
         STCM  R1,15,ACMTCOM                                                    
         STCM  R1,15,ACMTRECV                                                   
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEMENT,(0,=C'ADD=END')             
         CLI   12(R1),0                                                         
         BE    ADJ04                                                            
         MVI   ERROR,TOOLONG       DID RECORD GET TOO LONG                      
         CLI   12(R1),5                                                         
         BE    ERREXIT                                                          
         DC    H'0'                                                             
*                                                                               
         USING ACAPD,R6                                                         
ADJ04    LA    R6,ELEMENT          ANALYSIS POINTER X'C0'                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACAPEL,ACAPELQ      ELEMENT CODE                                 
         LA    R1,ACAPMLEN-ACAPD                                                
         STC   R1,ACAPLEN          ELEMENT LENGTH                               
*                                                                               
         LA    R3,RECVACT          ADD THE SR ACCOUNT DR                        
         MVI   STATIS,X'80'                                                     
         BAS   RE,ADDC0                                                         
*                                                                               
         LA    R3,INCMACT          ADD THE SI ACCOUNT CR                        
         MVI   STATIS,X'00'                                                     
         BAS   RE,ADDC0                                                         
*                                                                               
         TM    COMPSTA1,X'10'      AGENCY ON COSTING?                           
         BZ    ADJ08               NO  - SKIP ADDING THOSE POSTINGS             
*                                                                               
         LA    R3,ACCT1C           ADD THE 1C ACCOUNT DR                        
         MVI   STATIS,X'80'                                                     
         BAS   RE,ADDC0                                                         
         LA    R3,ACCT12           ADD 12 ACCOUNT     CR                        
         MVI   STATIS,X'00'                                                     
         BAS   RE,ADDC0                                                         
         LA    R3,ACCT11           ADD 11 ACCOUNT     CR                        
         MVI   STATIS,X'00'                                                     
         BAS   RE,ADDC0                                                         
*                                                                               
ADJ08    GOTO1 ADDELEM                                                          
*                                                                               
         USING TRNBLKD,RF                                                       
         LA    RF,UPDBLOCK                                                      
         MVC   TRNREC,AIO2         A(TRANS RECORD)                              
         MVC   TRNPUSER,SVTWAID    SET USER-ID                                  
         MVC   TRNBSEQN,=X'0001'   MIGHT BE A BUG BUT HOW IT WORKED             
*        ICM   RE,3,TRNBSEQN       INCREMENT TRANSACTION SEQUENCE #             
*        LA    RE,1(RE)                                                         
*        STCM  RE,3,TRNBSEQN                                                    
         MVC   TRNCACNM,CONTNAM    SET CONTRA-ACCOUNT NAME                      
         MVC   TRNBMOS,MOSP        SET (PWOS) BATCH MONTH                       
         OI    TRNINDS2,TRNIADDG   ADD GL POSTING                               
*                                                                               
         GOTO1 ADDTRN,TRNBLKD      **A D D T R N **                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        ADDING THE X'C0' ANALYSIS POINTER ELEMENT              *               
*****************************************************************               
         SPACE 2                                                                
         USING ACAPD,R6                                                         
ADDC0    CLC   POSTACT,0(R3)       DON'T ADD IF ACCOUNT                         
         BER   RE                                                               
         CLC   POSTCON,0(R3)       OR CONTRA ACCOUNT                            
         BER   RE                                                               
         ZIC   R1,ACAPLEN          CURRENT LENGTH                               
         LA    R2,0(R1,R6)         R2 TO NEXT AVAILABLE MINI                    
         USING ACAPMLEN,R2         R2 TO MINI ELEMENT                           
         MVC   ACAPSTAT,STATIS     STATUS                                       
         LA    RF,14(R3)           RF TO LAST BYTE OF KEY                       
         LA    R1,13               KEY LESS 1 FOR COMPANY AND 1 FOR EX.         
         CLI   0(RF),X'41'         FIND LAST SIGNIFICANT BYTE                   
         BH    *+12                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         DC    H'0'                INVALID KEY                                  
         CH    R1,=H'2'                                                         
         BNL   *+6                 NEED AT LEAST U/L/A                          
         DC    H'0'                                                             
         EX    R1,*+8              MOVE ANALYSIS POINTER TO ELEMENT             
         B     *+10                                                             
         MVC   ACAPACCT(0),1(R3)   KEY(MINUS COMPANY) TO ELEMENT                
         LA    R1,ACAPACCT-ACAPMLEN+1(R1) LENGTH OF MINI                        
         STC   R1,ACAPMLEN                                                      
*                                  CHECK FOR DUPLICATE MINI ELEMENT             
         BCTR  R1,0                FIX CURRENT LENGTH FOR EX INSTR.             
         ZIC   RF,ACAPMIN          NUMBER OF MINI'S SO FAR                      
         LTR   RF,RF                                                            
         BZ    ADDC02                     FIRST TIME                            
         LA    R3,ACAPMLEN-ACAPD(R6)      R3 TO FIRST MINI                      
ADDC01   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R2)        PREVIOUS VS CURRENT                         
         BER   RE                   IF THE SAME NO NEED TO ADD                  
         ZIC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         BCT   RF,ADDC01                                                        
*                                                                               
ADDC02   AH    R1,=H'1'            RESTORE LENGTH OF CURRENT MINI               
         ZIC   R0,ACAPLEN          ADD OLD LENGTH                               
         AR    R1,R0               TO LENGTH OF NEW MINI                        
         STC   R1,ACAPLEN          NEW ELEMENT LENGTH                           
         ZIC   R0,ACAPMIN                                                       
         AH    R0,=H'1'            NUMBER OF MINI'S                             
         STC   R0,ACAPMIN                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*                      BATCH HEADER DUMMY                             *         
***********************************************************************         
*                                                                               
BHEAD    NTR1                                                                   
*                                 IF BATCH HEADER EXISTS UPDATE                 
*                                 - ITEM COUNT AND CASH TOTAL                   
         SPACE 1                                                                
         MVC   AIO,AIO2                     USE AIO2                            
         MVC   KEY,SPACES                                                       
         LA    R6,KEY                                                           
         USING ACBKEYD,R6                                                       
         XC    ACBKEYD((ACBKREF+L'ACBKREF)-ACBKEYD),ACBKEYD                     
         MVI   ACBKCODE,X'0B'                                                   
         MVC   ACBKCOMP,COMPANY                                                 
         MVC   ACBKOFF,SVTWAID                                                  
         MVI   ACBKGRUP,C'G'                GENERAL ACCOUNTING                  
         MVI   ACBKTYPE,58                  BATCH TYPE 58                       
         MVC   ACBKDATE,TODAYP              DATE                                
         MVC   BATCHREF(L'MOS),MOS                                              
         MVC   BATCHREF+L'MOS(L'ADJ),ADJ                                        
*        MVC   BATCHREF+L'MOS+L'ADJ(L'BATCHREF-L'MOS-L'ADJ),RCDATE+3            
         MVC   ACBKREF,BATCHREF                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY              DOES BATCH HEADER EXIST             
         BNE   BHED04                       NO  - GO ADD A NEW ONE              
         SPACE 1                                                                
         L     R6,AIO                       YES - UPDATE ITEM/$$ COUNTS         
         LA    RF,ACBKEYD+(ACRECORD-ACKEYD)                                     
         USING ACBTCHD,RF                                                       
         AP    ACBHCASH,RECEIV                                                  
         AP    ACBHITEM,=P'1'                                                   
         MVC   FUNCTION,=CL8'DMWRT '                                            
         B     BHED09                       WRITE RECORD BACK                   
         SPACE 2                                                                
*                                 SET UP A NEW BATCH HEADER RECORD              
         SPACE 1                                                                
BHED04   L     R2,AIO                       CLEAR AIO AREA                      
         LA    R3,2000                                                          
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
         L     R6,AIO                                                           
         SPACE 1                                                                
         MVC   ACBKEYD(42),KEYSAVE          SAME KEY AS ABOVE                   
         SPACE 1                                                                
*                                           BATCH STATUS                        
*                                           X'20' MARK AS BEU                   
         MVI   ACBKEYD+(ACSTATUS-ACKEYD),X'20'                                  
         LA    RF,ACBKEYD+(ACRECORD-ACKEYD)                                     
         USING ACBTCHD,RF                                                       
         MVI   ACBHEL,X'06'                                                     
         MVI   ACBHLEN,(ACBHITEM+L'ACBHITEM)-ACBTCHD                            
         MVC   ACBHNAME(L'BATCHNAM),BATCHNAM                                    
         ZAP   ACBHCASH,RECEIV                                                  
         ZAP   ACBHITEM,=P'1'                                                   
         LA    RF,(ACBHITEM+L'ACBHITEM)-ACBTCHD+1(RF)                           
         DROP  RF                                                               
         SR    RF,R6                                                            
         STCM  RF,3,ACBKEYD+(ACLENGTH-ACKEYD)                                   
         MVC   FUNCTION,=CL8'DMADD'                                             
         SPACE 1                                                                
BHED09   DS    0H                                                               
         BAS   RE,MANAGER                                                       
         MVC   AIO,AIO1                     RESTORE AIO                         
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                      PROCESS PF KEYS                                *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         LA    R2,ADJRCVH                                                       
         LA    R6,SAVEKEY                                                       
         USING ACINKEY,R6                                                       
         XC    DUB,DUB                                                          
         MVI   DUB,L'ACINMED                                                    
         MVC   DUB+1(L'ACINMED),ACINMED                                         
         TM    (ACSTATUS-ACKEYD)(R6),X'02'   MI= RECORD?                        
         BZ    PROCPFA                                                          
         MVI   DUB,X'05'                                                        
         MVC   DUB+1,=C'MI='                                                    
         MVC   DUB+4(L'ACINMED),ACINMED                                         
PROCPFA  CLI   PFKEY,PF3           PF3 FOR PROFILE ACTION CHANGE                
         BE    PROCPF3                                                          
         CLI   PFKEY,PF2           PF2 FOR PROFILE ACTION DISPLAY               
         BNE   PROCPFX                                                          
         SPACE 1                                                                
         LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         SPACE 1                                                                
PROCPF2  MVI   PFKEY,0                                                          
         GOTO1 VTRANSF,WORK,=C'ESTIMATE',,(12,ACINACC),(3,ACINCLT),(3,AX        
               CINPRD),(DUB,DUB+1),(6,ACINEST),0                                
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF3  LA    R1,=C'CHA'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         B     PROCPF2                                                          
         SPACE 1                                                                
PROCPFX  B     OKEXIT                                                           
         EJECT                                                                  
*****************************************************************               
*        VALIDATE THE INPUT PROFILE/ESTIMATE  PERCENTS          *               
*****************************************************************               
         SPACE 1                                                                
PERCENTS NTR1                                                                   
         MVI   MAX,2               2 DECIMAL PLACES                             
         GOTO1 VALIDEC             VALIDATE THE PCT                             
         MVI   ERROR,BADFORM                                                    
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         CP    DUB,=P'0'           EDIT RETURNS INPUT PACKED IN DUB             
         BL    ERREXIT             NO MINUS AMOUNTS                             
         CP    DUB,=P'10000'       PCT GREATER THAN 100                         
         BH    ERREXIT                                                          
         OI    6(R2),X'80'         SET TRANSMIT                                 
         CURED DUB,(5,8(R2)),2,FLOAT=-                                          
         AP    PCTTOTS,DUB                                                      
         CP    PCTTOTS,=P'10000'   TOTAL GREATER THAN 100 PERCENT               
         BNH   OKEXIT                                                           
         B     ERREXIT                                                          
         EJECT                                                                  
***********************************************************************         
*             ADD OR UPDATE RECORD, DEPENDING ON FUNCTION             *         
***********************************************************************         
         SPACE 1                                                                
MANAGER  NTR1                                                                   
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,FUNCTION,=C'ACCOUNT',(R6),(R6),DMWORK               
         CLI   DMCB+8,0                                                         
         BE    OKEXIT                                                           
         DC    H'0'                                                             
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
OKEXIT   CR    R8,R8                                                            
EXIT     XIT1                                                                   
         SPACE 3                                                                
ERREXIT  GOTO1 VERRCUR                                                          
         EJECT                                                                  
ADJ      DC    CL4'AJUS'                                                        
BATCHNAM DC    CL15'INTERAGENCY ADJ'                                            
POSTMSG  DC    CL50'CHANGES DISPLAYED - HIT PF4 TO UPDATE'                      
NPOSTMSG DC    CL50'***ESTIMATE NOT POSTED - NO ADJUSTMENT ALLOWED*** '         
ADJOKMSG DC    CL50'***ADJUSTMENTS PERMITTED FOR HIGHLIGHTED MONTHS***'         
ADJUSMSG DC    CL50'***POSTED AMT HAS BEEN ADJUSTED -- FILE UPDATED***'         
ADJNARRA DC    CL47'INTERAGENCY ADJUSTMENT MMMDD/YY FOR ADV MMM/YY '            
         ORG   *-7                                                              
ADJADMTH DS    CL7'MMM/YY '                                                     
         SPACE 1                                                                
TRANSLEN EQU   TRNSLNQ+(L'ADJNARRA)                                             
         EJECT                                                                  
*        LTORG FOR THIS PHASE                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MYD      DSECT                                                                  
FUNCTION DS    CL8                 SAVE DATAMGR COMMAND                         
SCRENTOT DS    0PL8                SCREEN TOTALS                                
GROSSTOT DS    PL8                                                              
RECVETOT DS    PL8                                                              
POSTTOT  DS    PL8                                                              
PAIDTOT  DS    PL8                                                              
TOTNUMB  EQU   (*-SCRENTOT)/L'SCRENTOT                                          
GROSS    DS    PL8                                                              
RECEIV   DS    PL8                                                              
AMOUNTP  DS    PL8                                                              
PCT      DS    PL3                                                              
PK16     DS    PL16                                                             
FORMULA  DS    CL1                 FORMULA TO USE IN CALC                       
NOPOST   DS    CL1                 ANYTHING TO BE ADJUSTED?                     
ADJUSTED DS    CL1                 ANYTHING ADJUSTED?                           
         SPACE 1                                                                
PCTTOTS  DS    PL3                                                              
SAVERE   DS    A                   SAVE AREA FOR RE                             
MYDEND   EQU   *                                                                
         EJECT                                                                  
SCREEND  DSECT                                                                  
EMTHH    DS    CL8                 ESTIMATE MONTH                               
EMTH     DS    CL(L'ADJMON)                                                     
EPOSTDH  DS    CL8                 POSTING/ADJUSTMENT GL MTH/YY                 
EPOSTD   DS    CL(L'ADJPST)                                                     
EGROSSH  DS    CL8                 GROSS ESTIMATE                               
EGROSS   DS    CL(L'ADJGRS)                                                     
EAORPH   DS    CL8                 A.O.R. PCT                                   
EAORP    DS    CL(L'ADJAPC)                                                     
ERCVPH   DS    CL8                 RECV PCT                                     
ERCVP    DS    CL(L'ADJRPC)                                                     
ERECVH   DS    CL8                 RECEIVABLE ESTIMATE                          
ERECV    DS    CL(L'ADJREC)                                                     
EPOSTH   DS    CL8                 POSTED AMOUNT                                
EPOST    DS    CL(L'ADJPOS)                                                     
EPAIDH   DS    CL8                 PAID AMOUNT                                  
EPAID    DS    CL(L'ADJPRE)                                                     
ELINE    EQU   *-SCREEND                                                        
         EJECT                                                                  
*  DDSPOOLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*  DDSPLWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*  ACGENBOTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*  ACBMONVALD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
         PRINT ON                                                               
*  DDPERVALD                                                                    
*        PRINT OFF                                                              
*        INCLUDE DDPERVALD                                                      
*        PRINT ON                                                               
*  ACINTWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACINTWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACINTF4D                                                       
         DS    0F                                                               
SAVEKEY  DS    CL48                HOLD KEY FOR EXIT                            
UPDBLOCK DS    XL(TRNBLKL)         ADDTRN BLOCK                                 
UPDBLLNQ EQU   *-UPDBLOCK                                                       
PALAREA  DS    XL20                P&L BUCKET AREA                              
         EJECT                                                                  
*                                                                               
*        DSECT TO COVER LOCAL WORKING STORAGE                                   
*                                                                               
LOCALD   DSECT                                                                  
RECVACT  DS    CL15                RECEIVABLE ACCT                              
RECVNAM  DS    CL36                RECV NAME                                    
INCMACT  DS    CL15                INCOME ACCOUNT                               
INCMNAM  DS    CL36                INCOME NAME                                  
PRODNAM  DS    CL36                PRODUCT NAME                                 
ESTNUMB  DS    CL6                 EST NUMBER                                   
ESTDESC  DS    CL36                EST DESCRIPTION                              
CLIPRD   DS    0CL6                                                             
CLI      DS    CL3                 CLIENT                                       
PRD      DS    CL3                 PROD                                         
MEDCODE  DS    CL2                 2 BYTE MEDIA                                 
MEDSWITC DS    CL1                 Y = USING MEDIA RECORDS                      
ACCT1C   DS    CL15                1C COSTING ACCOUNT                           
NAM1C    DS    CL36                COSTING ACCOUNT NAME                         
ACCT12   DS    CL15                12 INCOME ANALYSIS ACCOUNT                   
NAM12    DS    CL36                12 NAME                                      
ACCT11   DS    CL15                11 INCOME ANALYSIS ACCOUNT                   
NAM11    DS    CL36                11 NAME                                      
OFFCODE  DS    CL2                 OFFICE CODE                                  
MOS      DS    CL2                 ACCOUNTING MONTH OF SERVICE                  
MOSP     DS    CL3                 MOS PACKED FOR ADDTRN                        
TDATEP   DS    CL3                 TRANS DATE PACKED FOR ADDTRN                 
ADVMTH   DS    CL2                 ADVERTISING MTH FOR POSTING                  
SVTWAID  DS    XL2                 TWA USER ID FOR POSTING                      
POSTACT  DS    CL15                POSTING ACCOUNT FOR ADDTRN                   
POSTCON  DS    CL15                POSTING CONTRA FOR ADDTRN                    
CONTNAM  DS    CL36                CONTRA NAME                                  
STATIS   DS    XL1                 TRANSTAT                                     
BATCHREF DS    CL6                 BATCH REFERENCE                              
DUEDAYS  DS    XL1                 NUMBER OF +DUE DAYS TO TRANS DATE            
BMONWRK  DS    XL20                OUTPUT BLOCK FOR BMONVALD                    
LOCALLN  EQU   *-LOCALD                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACINT14   05/05/08'                                      
         END                                                                    
