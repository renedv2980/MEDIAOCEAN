*          DATA SET ACINT11    AT LEVEL 022 AS OF 05/01/02                      
*PHASE T61911A,*                                                                
         TITLE 'T61911 - ESTIMATE RECORD MAINTENENCE'                           
T61911   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61911**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLEN                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE KEY MODE?                           
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
*        BAS   RE,PROCPF           CHECK PF KEYS                                
         B     OKEXIT                                                           
         SPACE 1                                                                
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC             VALIDATE RECORD                              
         BAS   RE,DREC             DISPLAY NEW RECORD                           
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
         BAS   RE,PROCPF           CHECK PF KEYS                                
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
         OI    ETMRCVH+6,X'80'                                                  
         MVC   ETMRCV,ACINACC      A.O.R AGENCY                                 
         OI    ETMCLTH+6,X'80'                                                  
         MVC   ETMCLT,ACINCLT      OUR CLIENT                                   
         OI    ETMPRDH+6,X'80'                                                  
         MVC   ETMPRD,ACINPRD      AND PRODUCT                                  
         OI    ETMMEDH+6,X'80'                                                  
         MVC   ETMMED(L'ACINMED),ACINMED                                        
         OI    ETMESTH+6,X'80'                                                  
         MVC   ETMEST,ACINEST      SIX DIGIT ESTIMATE NUMBER                    
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           VALIDATE KEY                              *         
***********************************************************************         
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,ETMRCVH          VALIDATE ACCOUNT                             
         MVC   CUL+1(2),RECVLEDG                                                
         OI    ETMRCVH+6,X'80'         SET TO TRANSMIT                          
         MVI   OPTION,C'Y'         PASS NAME                                    
         GOTO1 VALACCT                                                          
         SPACE 1                                                                
         LA    R2,ETMCLTH          CLIENT                                       
         MVI   ERROR,INVALID                                                    
         SR    R1,R1                                                            
         ICM   R1,1,ETMCLTH+5                                                   
         CH    R1,=H'1'                                                         
         BNH   ERREXIT                                                          
         CLI   8(R2),C' '          DOES CLIENT START WITH A BLANK ?             
         BE    ERREXIT             YES, THIS IS AN ERROR                        
         OI    ETMCLT+6,X'80'      SET TRANSMIT                                 
         SPACE 1                                                                
         MVC   CUL+1(2),PRODLEDG                                                
         GOTO1 VALCLI              VALIDATE CLIENT                              
         CLC   ETMCLTH+5(1),LCLI   MAKE SURE INPUT IS NOT TOO LONG              
         MVI   ERROR,TOOLONG                                                    
         BH    ERREXIT                                                          
         SPACE 1                                                                
         LA    R2,ETMPRDH          PRODUCT                                      
         MVI   ERROR,INVALID                                                    
         SR    R1,R1                                                            
         ICM   R1,1,ETMPRDH+5                                                   
         CH    R1,=H'1'                                                         
         BNH   ERREXIT                                                          
         CLI   8(R2),C' '          DOES PRODUCT START WITH A BLANK ?            
         BE    ERREXIT             YES, THIS IS AN ERROR                        
         OI    ETMPRDH+6,X'80'     SET TRANSMIT                                 
         SPACE 1                                                                
         GOTO1 VALPROD             VALIDATE PRODUCT                             
         CLC   ETMPRDH+5(1),LPRO   MAKE SURE INPUT IS NOT TOO LONG              
         MVI   ERROR,TOOLONG                                                    
         BH    ERREXIT                                                          
         SPACE 1                                                                
         LA    R2,ETMMEDH          MEDIA                                        
         OI    ETMMEDH+6,X'80'     SET TRANSMIT                                 
         GOTO1 VALMED              VALIDATE MEDIA                               
         SPACE 1                                                                
         LA    R2,ETMESTH          ESTIMATE NUMBER                              
         OI    ETMESTH+6,X'80'     SET TRANSMIT                                 
         GOTO1 VALEST                                                           
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
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                        DISPLAY RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
DREC     NTR1                                                                   
         L     R6,AIO                                                           
         OI    ETMMEDH+6,X'80'               TRANSMIT                           
         MVC   ETMMED,SPACES                                                    
         MVC   ETMMED(L'ACINMED),(ACINMED-ACINKEY)(R6)                          
         SPACE 1                                                                
         TM    (ACSTATUS-ACKEYD)(R6),X'02'   IS MEDIA AN MI RECORD?             
         BZ    DREC01                                                           
         MVC   ETMMED(3),=C'MI='                                                
         MVC   ETMMED+3(L'ACINMED),(ACINMED-ACINKEY)(R6)                        
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
         OI    ETMPERTH+6,X'80'              TRANSMIT                           
         MVC   ETMPERT,=C'EST. PERIOD'                                          
         SPACE 1                                                                
         OI    ETMPERH+6,X'80'                                                  
         SPACE 1                                                                
         MVC   WORK(2),ACIPFPRS                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,ETMPER)                                  
         MVI   ETMPER+6,C'-'                                                    
         MVC   WORK(2),ACIPFPRE                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,ETMPER+7)                                
         MVC   OPERST,ACIPFPRS                                                  
         MVC   OPERED,ACIPFPRE               SAVE START/END OF PERIOD           
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
         LA    R1,ETMMONH                  1ST MONTHLY FIELD                    
         LA    R0,ETMENDH                  LAST SCREEN FIELD                    
         SR    RF,RF                                                            
         SPACE 1                                                                
DREC01A  OI    4(R1),X'20'                 MARK VALIDATED                       
         NI    1(R1),X'FF'-X'08'-X'01'     TURN OFF HI INTENS/MODIFIED          
         OI    1(R1),X'20'                 PROTECTED                            
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         CR    R1,R0                                                            
         BNH   DREC01A                                                          
         EJECT                                                                  
*                                ***********************************            
*                                * DISPLAY MONTHLY ESTIMATE SCREEN *            
*                                * LINES AND BUILD A TABLE OF THE  *            
*                                * CURRENT MONTHLY POSTING DATES   *            
*                                * FOR THIS ESTIMATE.              *            
*                                ***********************************            
         USING ACINESTD,R6                                                      
         USING SCREEND,R3                                                       
         LA    R3,ETMMONH          1ST MONTHLY SCREEN LINE                      
         SPACE 1                                                                
         GOTO1 VCLEARF,DMCB,ETMMONH,ETMENDH                                     
         SPACE 1                                                                
         GOTO1 (RF),(R1),(1,ETMMONH),ETMENDH                                    
         SPACE 1                                                                
         LA    R4,PDATETAB         TABLE OF POSTING DATES                       
         SR    R2,R2               USE R2 TO COUNT SCREEN LINES                 
         SPACE 1                                                                
DREC02   DS    0H                                                               
         MVC   WORK(L'ACIESMTH),ACIESMTH                                        
         MVI   WORK+(L'ACIESMTH),1                                              
         GOTO1 DATCON,DMCB,(1,WORK),(6,EMTH)         EST MONTH                  
         MVC   0(3,R4),SPACES                        CLEAR TAB ENTRY            
         NI    EPOSTDH+1,X'FF'-X'20'                 UNPROTECT                  
         OC    ACIESDTO,ACIESDTO                     POSTING DATES?             
         BZ    DREC03                                NO- NO DATCON              
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(2,ACIESDTO),(5,EPOSTD)   POSTING DATE               
         TM    ACIESTAT,X'80'                        HAS EST BEEN POST?         
         BNZ   DREC03                                YES - SKIP TABL            
         GOTO1 DATCON,DMCB,(2,ACIESDTO),(1,0(R4))    POST DATE TABLE            
         SPACE 1                                                                
DREC03   CURED ACIESGRS,(L'EGROSS,EGROSS),2,FLOAT=-    GROSS BILLING            
         AP    GROSSTOT,ACIESGRS                                                
         CURED ACIESFEE,(L'EAORP,EAORP),2,FLOAT=-      A.O.R. FEE PCT           
         CURED ACIESRCV,(L'ERCVP,ERCVP),2,FLOAT=-      CREATIVE PCT             
         CURED ACIESREC,(L'ERECV,ERECV),2,FLOAT=-      RECEIVABLE EST           
         AP    RECVETOT,ACIESREC                                                
         NI    EGROSSH+1,X'FF'-X'20'                 UNPROTECT                  
         NI    EAORPH+1,X'FF'-X'20'                                             
         NI    ERCVPH+1,X'FF'-X'20'                                             
         NI    ERECVH+1,X'FF'-X'20'                                             
         TM    ACIESTAT,X'80'                        HAS EST BEEN POST?         
         BZ    DREC03A                               NO SKIP POST COL           
         SPACE 1                                                                
         CURED ACIESREC,(L'EPOST,EPOST),2,FLOAT=-    POSTED AMOUNT              
         AP    POSTTOT,ACIESREC                                                 
         SPACE 1                                                                
DREC03A  CURED ACIESPD,(L'EPAID,EPAID),2,FLOAT=-     PAID AMOUNT                
         AP    PAIDTOT,ACIESPD                                                  
         SPACE 1                                                                
         LA    R3,ELINE(R3)                          NEXT SCREEN LINE           
         AH    R2,=H'1'                              BUMP LINE COUNT 1          
         LA    R4,3(R4)                              BUMP POST DAT TAB          
         MVI   0(R4),X'FF'                           MARK END                   
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),ACIESEQU                        NEXT EST ELEMENT           
         BE    DREC02                                                           
         SPACE 1                                                                
         LA    R1,ETMLLINH                           HI INTENS PFKEYS           
         LA    R0,ETMENDH                                                       
         SR    RF,RF                                                            
         OI    1(R1),X'08'                                                      
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         CR    R1,R0                                                            
         BNH   *-12                                                             
         SPACE 1                                                                
         LA    R5,ETMLLINH                           PFKEYS                     
         MVC   EMTH-SCREEND(L'EMTH,R5),=C'PFKEYS'                               
         MVC   EPOSTD-SCREEND(L'EPOSTD,R5),=C'2=PR DIS'                         
         MVC   EGROSS-SCREEND(L'EGROSS,R5),=C'3=PR CHA   '                      
         MVC   EAORP-SCREEND(L'EAORP,R5),=C'4=EST'                              
         MVC   ERCVP-SCREEND(L'ERCVP,R5),=C'ADJ  '                              
         OI    EPAIDH+6-SCREEND(R5),X'80'                                       
         MVC   EPAID-SCREEND(L'EPAID,R5),=C'12=RETURN '                         
          SPACE 1                                                               
         CH    R2,=H'12'                             LAST SCREEN LINE           
         BE    DREC05                                YES-CLEAR PF KEYS          
         BL    *+6                                   NO - SKIP A LINE           
         DC    H'0'                                                             
         SPACE 1                                                                
         LA    R3,ELINE(R3)                          SKIP A LINE                
         CR    R3,R5                                 LAST LINE NOW?             
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
         CURED PAIDTOT,(L'EPAID,EPAID),2,FLOAT=-       PAID                     
         OI    EPOSTH+1,X'08'                                                   
         CURED POSTTOT,(L'EPOST,EPOST),2,FLOAT=-       POST                     
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
VREC     NTR1                                                                   
         CLI   EMULATE,C'Y'        TEST EMULATING ACCOUNT FILE                  
         BNE   *+8                 NO                                           
         BAS   RE,GETDA            GET ESTIMATE RECORD DISK ADDRESS             
         SPACE 1                                                                
         MVI   ELCODE,ACIPFEQU     ESTIMATE PROFILE ELEMENT                     
         BAS   RE,GETELIO                                                       
         BE    *+12                                                             
         MVI   ERROR,NOEL          NO ESTIMATE PROFILE ON RECORD                
         B     ERREXIT                                                          
         SPACE 1                                                                
         MVI   ELCODE,ACIESEQU     MONTHLY ESTIMATE ELEMENTS                    
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
         SPACE 1                                                                
         USING ACINESTD,R6                                                      
         USING SCREEND,R3                                                       
         LA    R3,ETMMONH          SCREEN LINE BEGINS WITH ADV MONTH            
         SPACE 1                                                                
*                                  ***********************************          
*                                  *  BUILD REVISED POST DATE TABLE  *          
*                                  ***********************************          
         SPACE 1                                                                
         MVC   PDATENEW,SPACES                                                  
         LA    R4,PDATENEW         NEW POSTING DATE TABLE                       
         MVI   0(R4),X'FF'         MARK END                                     
         MVC   NPERST,OPERST       SAVE START AND END OF THE EST PERIOD         
         MVC   NPERED,OPERED                                                    
         MVI   DELETONE,C'N'       RESET DELETE ONE SWITCH TO NO                
         SPACE 1                                                                
VREC03   CLI   EPOSTDH+5,0          INPUT TO POSTING DATE?                      
         BNE   VREC03D              YES- CHECK IT                               
         TM    ACIESTAT,X'80'       POSTING DATE REQUIRED IF ALREADY            
         BNZ   VREC03D              POSTED                                      
                                                                                
         LA    R2,EGROSSH                                                       
         MVI   MAX,X'82'                                                        
         GOTO1 VALIDEC                                                          
         CP    DMCB+4(8),=P'0'                                                  
         BNZ   VREC03D                                                          
                                                                                
         MVI   MAX,X'82'                                                        
         LA    R2,ERECVH                                                        
         GOTO1 VALIDEC                                                          
         CP    DMCB+4(8),=P'0'                                                  
         BNZ   VREC03D                                                          
                                                                                
         XC    ACIESDTO,ACIESDTO    CLEAR ORIGINAL POSTING DATE                 
         MVC   ACIESDAT,ACIESDTO    MAKE REVISED THE SAME                       
         MVC   0(3,R4),SPACES       SET POST DATE TABLE TO IGNORE               
         B     VREC04A              CONTINUE                                    
         SPACE 1                                                                
VREC03D  LA    R2,EPOSTDH                                                       
         GOTO1 ANY                 IS THERE A POSTING DATE                      
         EJECT                                                                  
*                                  ***********************************          
*                                  *  DELETING MTHS FROM EST PERIOD  *          
*                                  ***********************************          
*                                  * ALLOW THE DELETION OF ONE MONTH *          
*                                  * AT A TIME FROM THE FRONT OR BACK*          
*                                  * END OF THE ESTIMATE PERIOD ONLY *          
*                                  * IF NOTHING HAS BEEN POSTED FOR  *          
*                                  * THAT MONTH.                     *          
*                                  ***********************************          
         CLC   EPOSTD(6),=C'DELETE'                                             
         BNE   VREC04                                                           
         MVI   ERROR,CANTDEL                                                    
         CLI   DELETONE,C'Y'       HAVE WE DELETED ONE ALREADY                  
         BE    ERREXIT             YES -ERROR DELETE ONLY ONE AT A TIME         
         CLC   OPERST,OPERED       CAN'T DLETE IF ONLY 1 MTH IN PERIOD          
         BE    ERREXIT                                                          
         CLC   ACIESMTH,OPERST     EITHER THE 1ST OF LAST MTH                   
         BE    *+14                                                             
         CLC   ACIESMTH,OPERED                                                  
         BNE   ERREXIT                                                          
         TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BNZ   ERREXIT             YES NO CHANGE OF DATE ALLOWED                
         SPACE 1                                                                
         MVC   0(3,R4),SPACES      SET POST DATE TAB TO DEL                     
         MVI   ACIESEL,X'FF'       MARK EL FOR DELETION                         
         MVI   DELETONE,C'Y'       SET DELETE ONE SWITCH                        
         B     VREC04A                                                          
         SPACE 1                                                                
VREC04   MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,(0,EPOSTD),WORK                                      
         OC    DMCB,DMCB                                                        
         BZ    ERREXIT                                                          
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,ACIESDTO)                                
         GOTO1 (RF),(R1),(2,ACIESDTO),(1,0(R4))      POST DATE TABLE II         
         MVC   MOSP(L'MOSP),0(R4)                    M.O.S. PACKED              
         TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BZ    *+10                                                             
         MVC   0(3,R4),SPACES      CLEAR OUT TABL ENTRY                         
         SPACE 1                                                                
         MVI   ERROR,NOCHANGE                                                   
         TM    EPOSTDH+4,X'20'     HAS POST DATE CHANGED                        
         BNZ   VREC04A             NO - DONT BOTHER CHECK DATE FURTHER          
         TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BNZ   ERREXIT             YES NO CHANGE OF DATE ALLOWED                
         MVC   ACIESDAT,ACIESDTO   NO-SAVE ORIG POSTDATE IN REVISED TOO         
         SPACE 1                                                                
*                                  COMPARE M.O.S. FROM POSTING DATE             
*                                  TO LATEST LOCKED MONTH                       
         USING BMONVALD,R1                                                      
         MVI   ERROR,MOALOCK                                                    
         MVC   WORK(L'EPOSTDH+L'EPOSTD),EPOSTDH                                 
         MVI   WORK+5,0                                                         
         XC    BMONWRK,BMONWRK                                                  
         GOTO1 VBMONVAL,DMCB,WORK,(58,ACOMFACS),(0,BMONWRK),           X        
               (COMPANY,0)                                                      
         LA    R1,BMONWRK                                                       
         CLC   MOSP(L'CMOSLOCK),BMOLCKP                                         
         BNH   ERREXIT                                                          
         MVI   ERROR,ONEYEAR                                                    
         CLC   MOSP(L'NEXTP),NEXTP  CAN'T BE MORE THAN A YEAR AHEAD             
         BH    ERREXIT                                                          
         DROP  R1                                                               
         SPACE 1                                                                
VREC04A  MVI   FORMULA,1           SET FORMULA TO DEFAULT GROSSXPCT=REC         
         LA    R2,EGROSSH                                                       
         TM    EGROSSH+4,X'20'     HAS GROSS FIGURE BEEN CHANGED                
         BZ    VREC04B             YES-USE FORMULA 1                            
         SPACE 1                                                                
         LA    R2,ERECVH                                                        
         TM    ERECVH+4,X'20'      CHECK IF REC EST CHANGED                     
         BNZ   VREC06              NO- USE FORMULA 1                            
         MVI   FORMULA,2           SET FORMULA TO REC/PCT=GROSS                 
         MVI   ERROR,NOCHANGE                                                   
VREC04B  TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BNZ   ERREXIT             YES NO CHANGE OF GROSS OR RECV               
         SPACE 1                                                                
*                                  ** GROSS  ESTIMATE **                        
         SPACE 1                                                                
VREC06   DS    0H                                                               
         MVI   MAX,X'82'           2 DECIMAL PLACES                             
         LA    R2,EGROSSH                                                       
         GOTO1 VALIDEC             VALIDATE DECIMAL                             
         ZAP   DUB,DMCB+4(8)                                                    
         OI    EGROSSH+4,X'20'     MARK FIELD VALIDATED                         
         ZAP   GROSS,DUB           GROSS ESTIMATE                               
         SPACE 1                                                                
*                                  ** PERCENTS **                               
         ZAP   PCTTOTS,=P'0'                                                    
         MVI   ERROR,NOCHANGE                                                   
         LA    R2,EAORPH                                                        
         TM    EAORPH+4,X'20'      HAS AOR PCT BEEN CHANGED?                    
         BNZ   *+12                NO- CONTINUE                                 
         TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BNZ   ERREXIT             YES NO CHANGE OF PCT                         
         BAS   RE,PERCENTS                                                      
         TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BNZ   VREC06A             YES SKIP UPDATING THE ELEMENT                
         ZAP   ACIESFEE,DUB        A.O.R. FEE PCT TO X'C7' EL                   
         ZAP   ACIESFEO,DUB        A.O.R. FEE PCT TO X'C7' EL (ORIG)            
VREC06A  MVI   ERROR,NOCHANGE                                                   
         LA    R2,ERCVPH                                                        
         TM    ERCVPH+4,X'20'      HAS REC PCT BEEN CHANGED?                    
         BNZ   *+12                NO- CONTINUE                                 
         TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BNZ   ERREXIT             YES NO CHANGE OF PCT                         
         BAS   RE,PERCENTS                                                      
         TM    ACIESTAT,X'80'      HAS EST BEEN POSTED                          
         BNZ   VREC06B             YES SKIP UPDATING THE ELEMENT                
         ZAP   ACIESRCV,DUB        RECV PCT TO X'C7' EL                         
         ZAP   ACIESRCO,DUB        RECV PCT TO X'C7' EL (ORIG)                  
VREC06B  ZAP   PCT,DUB             RECEIVABLE PCT                               
         SPACE 1                                                                
         LA    R2,ERECVH                                                        
         MVI   MAX,X'82'           2 DECIMAL PLACES                             
         GOTO1 VALIDEC             VALIDATE DECIMAL                             
         ZAP   DUB,DMCB+4(8)                                                    
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
*        SRP   PK16(8),64-1,5                                                   
         ZAP   GROSS,PK16(8)                                                    
         SPACE 1                                                                
VREC10   OI    EGROSSH+6,X'80'         TRANSMIT                                 
         CURED GROSS,(L'EGROSS,EGROSS),2,FLOAT=-                                
         OI    ERECVH+6,X'80'                                                   
         CURED RECEIV,(L'ERECV,ERECV),2,FLOAT=-                                 
         TM    ACIESTAT,X'80'          HAS EST BEEN POSTED                      
         BNZ   VREC10A                 YES SKIP UPDATING THE ELEMENT            
         ZAP   ACIESGRS,GROSS          GROSS EST TO X'C7' EL                    
         ZAP   ACIESGRO,GROSS          GROSS EST TO X'C7' EL (ORIG)             
         ZAP   ACIESREC,RECEIV         RECEIVABLE EST TO X'C7' EL               
         ZAP   ACIESRAO,RECEIV         RECEIVABLE EST TO X'C7' EL(ORIG)         
         SPACE 1                                                                
VREC10A  LA    R3,ELINE(R3)            NEXT SCREEN LINE                         
         LA    R4,3(R4)                BUMP NEW POST DAT TABLE                  
         MVI   0(R4),X'FF'             MARK END                                 
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                   NEXT ELEMENT                             
         CLI   0(R6),ACIESEQU                                                   
         BE    VREC03                                                           
         EJECT                                                                  
*                                  ***********************************          
*                                  * ADD/REVISE AC08 POSTING POINTERS*          
*                                  ***********************************          
*                                  * COMPARE POSTING DATE TABLES     *          
*                                  * 1- DELETE "2D04" AC08 POINTERS  *          
*                                  *    THAT WERE "REVISED".         *          
*                                  * 2- ADD NEW AC08 POINTER RECORDS *          
*                                  *    WITH THE REVISED DATE.       *          
*                                  ***********************************          
         MVC   AIO,AIO2            USE 2ND AIO                                  
         SPACE 1                                                                
*                                  ** DELETE OLD DATE POINTER **                
         SPACE 1                                                                
         LA    R5,PDATETAB         EXISTING POINTER RECORD DATES                
VREC12   CLC   0(3,R5),SPACES      IS THERE EXISTING DATE POINTER               
         BE    VREC14              NO-JUST SEARCH FOR NEXT                      
         MVC   KEY,SPACES          CLEAR KEY                                    
         LA    R6,KEY                                                           
         USING ACDAKEY,R6                                                       
         MVI   ACDACOD,ACDAEQU                                                  
         MVI   ACDASREC,ACDASEQU                                                
         MVC   ACDADATE,0(R5)                                                   
         MVC   ACDACUL(ACINSUBL),SAVEKEY+(ACINCUL-ACINKEY)                      
         OI    DMINBTS,X'08'                                                    
         CLI   EMULATE,C'Y'        TEST EMULATING ACCOUNT FILE                  
         BE    VREC13              YES                                          
*                                                                               
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(42),KEY                                                  
         BNE   VREC14                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         OI    (ACSTATUS-ACKEYD)(R6),X'80'    DELETE THE RECORD                 
         MVC   FUNCTION,DMWRT                                                   
         BAS   RE,MANAGER                                                       
         B     VREC14                                                           
         SPACE 1                                                                
VREC13   MVC   KEYSAVE,KEY         SAVE KEY BEFORE READ                         
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),ACCDIR,KEY,AIO,0                   
         NI    DMINBTS,X'FF'-X'08'                                              
         L     R6,AIO                                                           
         USING IDJRECD,R6                                                       
         CLC   IDJKEY,KEYSAVE      TEST IF KEY FOUND                            
         BNE   VREC14              NO                                           
         OI    IDJKSTA,X'80'       DELETE THE DIRECTORY POINTER                 
         MVC   FUNCTION,DMWRT                                                   
         BAS   RE,MANAGERD                                                      
         SPACE 1                                                                
VREC14   LA    R5,3(R5)            BUMP THE OLD DATE TABLE                      
         CLI   0(R5),X'FF'         END OF TABLE                                 
         BNE   VREC12                                                           
*                                  ** ADD NEW DATE POINTER **                   
         SPACE 1                                                                
         LA    R5,PDATENEW         POSSIBLE DATE CHANGES                        
VREC16   CLC   0(3,R5),SPACES      IF TAB ENTRY SET TO SPACE DONT ADD           
         BE    VREC20                                                           
         MVC   KEY,SPACES          CLEAR KEY                                    
         LA    R6,KEY                                                           
         USING ACDAKEY,R6                                                       
         MVI   ACDACOD,ACDAEQU                                                  
         MVI   ACDASREC,ACDASEQU                                                
         MVC   ACDADATE,0(R5)                                                   
         MVC   ACDACUL(ACINSUBL),SAVEKEY+(ACINCUL-ACINKEY)                      
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         CLI   EMULATE,C'Y'        TEST EMULATING ACCOUNT FILE                  
         BE    VREC18              YES                                          
         SPACE 1                                                                
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(42),KEY                RECORD ALREADY THERE              
         BNE   VREC17                         NO- GO ADD IT                     
         L     R6,AIO                                                           
         NI    (ACSTATUS-ACKEYD)(R6),X'7F'    YES-UNDELETE THE RECORD           
         MVC   FUNCTION,DMWRT                                                   
         BAS   RE,MANAGER                                                       
         B     VREC20                         BUMP TABLE                        
         SPACE 1                                                                
VREC17   L     R2,AIO                         CLEAR AIO                         
         LA    R3,2000                                                          
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
         L     R6,AIO                                                           
         MVC   ACDAKEY,SPACES                                                   
         MVC   ACDAKEY,KEYSAVE                                                  
         MVC   ACLENGTH-ACKEYD(L'ACLENGTH,R6),=H'50'   LENGTH                   
         MVC   FUNCTION,DMADD                                                   
         BAS   RE,MANAGER                                                       
         B     VREC20                                                           
         SPACE 1                                                                
VREC18   MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),ACCDIR,KEY,AIO,0                   
         NI    DMINBTS,X'FF'-X'08'                                              
         L     R6,AIO                                                           
         USING IDJRECD,R6                                                       
         CLC   IDJKEY,KEYSAVE      TEST IF POINTER FOUND                        
         BNE   VREC19              NO-ADD ONE                                   
*                                                                               
         NI    IDJKSTA,X'FF'-X'80' UNDELETE POINTER                             
         MVC   FUNCTION,DMWRT                                                   
         BAS   RE,MANAGERD                                                      
         B     VREC20                                                           
*                                                                               
VREC19   MVC   IDJKEY,KEYSAVE      INITIALIZE KEY                               
         XC    IDJKSTA,IDJKSTA     CLEAR STATUS AREA                            
         MVC   IDJKDA,SAVEDA       SET ESTIMATE DISK ADDRESS                    
         MVC   FUNCTION,DMADD                                                   
         BAS   RE,MANAGERD                                                      
*                                                                               
VREC20   LA    R5,3(R5)            BUMP TABLES                                  
         CLI   0(R5),X'FF'         END OF TABLE                                 
         BNE   VREC16                                                           
         SPACE 1                                                                
         MVC   AIO,AIO1            RESTORE AIO TO ORIGINAL                      
         CLI   DELETONE,C'Y'                                                    
         BNE   VREC25                                                           
         MVI   ELCODE,ACIESEQU     LOOK FOR EST ELS                             
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACINESTD,R6                                                      
         MVC   NPERST,ACIESMTH     NEW START OF EST PERIOD                      
         MVC   NPERED,ACIESMTH     NEW END OF EST PERIOD                        
VREC21   ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),ACIESEQU                                                   
         BNE   VREC22                                                           
         MVC   NPERED,ACIESMTH     NEW END OF EST PERIOD                        
         B     VREC21                                                           
VREC22   L     R6,AIO                                                           
         MVI   ELCODE,ACIPFEQU     GET PROFILE EL                               
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACINPRFD,R6                                                      
         MVC   ACIPFPRS,NPERST     SAVE NEW EST PERIOD IN X'C6'                 
         MVC   ACIPFPRE,NPERED                                                  
         MVI   ELCODE,X'FF'        DELETE ANY MARKED X'C7' EST EL'S             
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
VREC25   B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                      PROCESS PF KEYS                                *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         LA    R2,ETMRCVH                                                       
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
         SPACE 1                                                                
PROCPFA  CLI   PFKEY,PF3           PF3 FOR PROFILE ACTION CHANGE                
         BE    PROCPF3                                                          
         CLI   PFKEY,PF4           PF4 FOR ESTIMATE ADJUSTMENT                  
         BE    PROCPF4                                                          
         CLI   PFKEY,PF2           PF2 FOR PROFILE ACTION DISPLAY               
         BNE   PROCPFX                                                          
         SPACE 1                                                                
         LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         SPACE 1                                                                
PROCPF2  MVI   PFKEY,0                                                          
         GOTO1 VTRANSF,WORK,=C'PROFILE',,(12,ACINACC),(3,ACINCLT),(3,ACX        
               INPRD),(DUB,DUB+1),(6,ACINEST),0                                 
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF3  LA    R1,=C'CHA'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         B     PROCPF2                                                          
         SPACE 1                                                                
PROCPF4  MVI   PFKEY,0                                                          
         GOTO1 VTRANSF,WORK,=C'ESTIMATE',=C'ADJ',(12,ACINACC),(3,ACINCLX        
               T),(3,ACINPRD),(DUB,DUB+1),(6,ACINEST),0                         
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
         SPACE 2                                                                
***********************************************************************         
*             ADD OR WRITE BACK DIRECTORY POINTER                     *         
***********************************************************************         
         SPACE 1                                                                
MANAGERD NTR1  ,                                                                
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,FUNCTION,ACCDIR,(R6),(R6),DMWORK                    
         CLI   DMCB+8,0                                                         
         BE    OKEXIT                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
*             GET ESTIMATE RECORD DISK ADDRESS                        *         
***********************************************************************         
         SPACE 1                                                                
GETDA    ST    RE,SAVERE                                                        
         L     R6,AIO2                                                          
         USING INTRECD,R6                                                       
         MVC   INTKEY,SAVEKEY      SET ESTIMATE KEY                             
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,(R6),(R6),0                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEDA,INTKDA       EXTRACT DISK ADDRESS                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
OKEXIT   CR    R8,R8                                                            
EXIT     XIT1                                                                   
         SPACE 3                                                                
ERREXIT  GOTO1 VERRCUR                                                          
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
SCREEND  DSECT                                                                  
EMTHH    DS    CL8                 ESTIMATE MONTH                               
EMTH     DS    CL(L'ETMMON)                                                     
EPOSTDH  DS    CL8                 MONTHLY POSTING DATE                         
EPOSTD   DS    CL(L'ETMPST)                                                     
EGROSSH  DS    CL8                 GROSS ESTIMATE                               
EGROSS   DS    CL(L'ETMGRS)                                                     
EAORPH   DS    CL8                 A.O.R. PCT                                   
EAORP    DS    CL(L'ETMAPC)                                                     
ERCVPH   DS    CL8                 RECV PCT                                     
ERCVP    DS    CL(L'ETMRPC)                                                     
ERECVH   DS    CL8                 RECEIVABLE ESTIMATE                          
ERECV    DS    CL(L'ETMREC)                                                     
EPOSTH   DS    CL8                 POSTED AMOUNT                                
EPOST    DS    CL(L'ETMPOS)                                                     
EPAIDH   DS    CL8                 PAID AMOUNT                                  
EPAID    DS    CL(L'ETMPRE)                                                     
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
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACBMONVALD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
         PRINT ON                                                               
*  ACINTWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACINTWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACINTF1D                                                       
         DS    0F                                                               
DELETONE DS    XL1                 DELETE ONE X'C7' EL ONLY SWITCH              
OPERST   DS    CL2                 'OLD' START OF ESTIMATE PERIOD               
OPERED   DS    CL2                 'OLD' END OF ESTIMATE PERIOD                 
NPERST   DS    CL2                 'NEW' START OF ESTIMATE PERIOD               
NPERED   DS    CL2                 'NEW' END OF ESTIMATE PERIOD                 
SAVEKEY  DS    CL48                HOLD SJ KEY FOR EXIT                         
PDATETAB DS    CL39                POSTING DATE TABLE YYMMDD 3BYTES             
*                                  12 MONTH MAX                                 
PDATENEW DS    CL39                *REVISED* POSTING DATE TABLE                 
*                                  12 MONTH MAX                                 
         EJECT                                                                  
* LOCAL WORKING STORAGE                                                         
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   LOCAL                                                            
FUNCTION DS    CL8                 SAVE DATAMGR COMMAND                         
SCRENTOT DS    0PL8                SCREEN TOTALS                                
GROSSTOT DS    PL8                                                              
RECVETOT DS    PL8                                                              
POSTTOT  DS    PL8                                                              
PAIDTOT  DS    PL8                                                              
TOTNUMB  EQU   (*-SCRENTOT)/L'SCRENTOT                                          
GROSS    DS    PL8                                                              
RECEIV   DS    PL8                                                              
PCT      DS    PL3                                                              
MOSP     DS    PL3                 M.O.S.                                       
PK16     DS    PL16                                                             
FORMULA  DS    CL1                                                              
BMONWRK  DS    CL20                                                             
         SPACE 1                                                                
PCTTOTS  DS    PL3                                                              
SAVERE   DS    A                   SAVE AREA FOR RE                             
SAVEDA   DS    XL4                 ESTIMATE RECORD DISK ADDRESS                 
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACINT11   05/01/02'                                      
         END                                                                    
