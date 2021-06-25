*          DATA SET RERIS07    AT LEVEL 184 AS OF 11/20/14                      
*PHASE T80D07A                                                                  
         TITLE 'RIS - T80D07 - READ AND FOUT MAKEGOOD LIST'                     
***********************************************************************         
*                                                                     *         
*        RERIS07 --- RIS HEADLINE AND DETAIL LINE BUILDER             *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* AUG15/13 (BOB) --- BIG BANG FOR MAKEGOOD OFFER LISTS                *         
*                                                                     *         
*                    **  END TOMBSTONE  **                            *         
***********************************************************************         
*                                                                               
T80D07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0D07**,RR=RE                                                 
         LA    R9,2048(RB)         INIT BASE REGISTERS                          
         LA    R9,2048(R9)                                                      
         LA    R7,2048(R9)                                                      
         LA    R7,2048(R7)                                                      
         USING T80D07,RB,R9,R7                                                  
*                                                                               
         L     RC,0(R1)                                                         
         USING T80DFFD,RA                                                       
         USING GENOLD,RC                                                        
*                                                                               
         ST    RE,RELO2            SAVE RELOCATION FACTOR                       
*                                                                               
         XC    KEYSAVE,KEYSAVE                                                  
         ZAP   PGTOT,=P'0'         CLEAR PAGE TOTAL                             
         MVI   LISTBYTE,LISTMKG    =0 IF IN MIDDLE OF READ, SO NEED TO          
*                                  SET FOR RIS READ MODULE                      
         OC    LISTWOPS,LISTWOPS   IF NO OPTIONS                                
         BNZ   *+10                                                             
         MVC   LISTWOPS,WOPSSAVE      USE SAVED OPTIONS                         
*                                                                               
         OC    LISTOOPS,LISTOOPS   IF NO OPTIONS                                
         BNZ   *+10                                                             
         MVC   LISTOOPS,OOPSSAVE      USE SAVED OPTIONS                         
*                                                                               
         OC    LISTDOPS,LISTDOPS   IF NO OPTIONS                                
         BNZ   *+10                                                             
         MVC   LISTDOPS,DOPSSAVE      USE SAVED OPTIONS                         
*                                                                               
         XC    ADVNMSV,ADVNMSV     INIT ADVERTISER NAME AREA                    
*                                                                               
         XC    WOPSSAVE,WOPSSAVE   INIT FILTER SAVE AREA                        
         XC    OOPSSAVE,OOPSSAVE   INIT FILTER SAVE AREA                        
         XC    DOPSSAVE,DOPSSAVE   INIT FILTER SAVE AREA                        
*                                                                               
         LA    R2,RISTITLH         POINT TO TITLE LINE                          
*                                                                               
         EJECT                                                                  
*                                                                               
         CLI   PRNT,1              ARE WE PRINTING REPORT?                      
         BNE   TT00                                                             
*                                                                               
         BRAS  RE,INITP            YES/INITIALIZE                               
*                                                                               
         BRAS  RE,DOHEADS             PRINT HEADLINES                           
*                                                                               
         B     FL12                                                             
                                                                                
XIT      XIT1                                                                   
                                                                                
*                                  TEST NEXT OPTION THIS TIME                   
* TEST NEXT OPTION THIS TIME                                                    
*                                                                               
TT00     CLI   NEXTBYTE,0                                                       
         BE    FOUTITLE                                                         
         B     FIRSTLIN                                                         
                                                                                
FOUTITLE LA    R1,RISTITLH         CLEAR SCREEN                                 
*                                                                               
FT00     ZIC   R3,0(R1)                                                         
*                                                                               
         LTR   R3,R3                                                            
         BZ    FT10                                                             
*                                                                               
         SH    R3,=H'9'                                                         
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
*                                                                               
         FOUT  (R1)                                                             
*                                                                               
**       LA    R1,9(R3,R1)         I DON'T THINK I NEED THIS                    
**       B     FT00                                                             
                                                                                
FT10     MVC   RISTITL(L'TITLE1),TITLE1                                         
         FOUT  RISTITLH                                                         
         MVC   RISBOTM(L'RISBOTM),PFTITL                                        
         FOUT  RISBOTMH                                                         
*                                                                               
FIRSTLIN DS    0H                                                               
*                                                                               
         LA    R2,RISTTL2H         CLEAR 2ND TITLE LINE                         
         XC    RISTTL2,RISTTL2                                                  
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RISOUTH           CLEAR SCREEN                                
         LR    R1,R2                                                            
         LA    R4,RISBOTMH                                                      
*                                                                               
FL10B    ZIC   R3,0(R1)                                                         
*                                                                               
         LTR   R3,R3                                                            
         BZ    FL12                                                             
*                                                                               
         SH    R3,=H'9'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
*                                                                               
         FOUT  (R1)                                                             
*                                                                               
         LA    R1,9(R3,R1)                                                      
         CR    R1,R4               IF LAST LINE                                 
         BE    FL12                LEAVE IT ALONE                               
         B     FL10B                                                            
*                                                                               
FL12     LA    R2,RISOUTH                                                       
         CLI   PRNT,1              IF PRINTING                                  
         BNE   *+8                                                              
         LA    R2,MYP-7               BUILD LIST ON PRINT LINE                  
*                                                                               
         USING LSMKGD,R2          ESTABLISH OUTPUT LINE                         
*                                                                               
*        SET DATE FIELDS                                                        
*                                                                               
         LAY   R6,TODAY                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)        TODAY YYMMDD                  
         GOTO1 VDATCON,DMCB,(0,WORK),(2,0(R6))    SAVE TODAY'S DATE             
         LA    R6,10                              ADD TEN DAYS                  
*                                                                               
         LAY   R5,TODAY10                                                       
*                                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK+10,(R6)                                    
         GOTO1 VDATCON,DMCB,(0,WORK+10),(2,0(R5))  SAVE DATE                    
*                                                                               
         LA    RE,RECORD                                                        
         ST    RE,AIOAREA          SET RECORD I/O AREA                          
*                                                                               
*        BUILD STARTING KEY FOR LIST                                            
*                                                                               
         CLC   SAVEKEY(2),=X'A001' IF SECOND TIME IN                            
         BE    *+10                                                             
         CLC   SAVEKEY(2),=X'A002' IF SECOND TIME IN                            
         BNE   *+20                                                             
         MVC   KEY,SAVEKEY            SET STARTING KEY                          
         XC    SAVEKEY,SAVEKEY                                                  
         B     LIST1ST                                                          
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING RMKGREC,R6          ESTABLISH MAKEGOOD REC KEYS                  
*                                                                               
         MVC   RMGSPTYP,=X'A001'   SET PASSIVE RECORD TYPE                      
         MVC   RMGSPREP,REPALPHA   SET REP CODE                                 
         MVC   RMGSPSAL,TBLSLS2    SET SALESPERSON CODE                         
*                                                                               
         OC    SVDVSLS,SVDVSLS     IF DEVELOPMENTAL SALESPERSON ENTERED         
         BZ    *+16                                                             
         MVC   RMGSPSAL,SVDVSLS       USE IT                                    
         MVC   RMGSPTYP,=X'A002'      NEW RECORD TYPE                           
*                                                                               
         MVC   RMGSPSTA,TBLSTA     STARTING STATION                             
*                                                                               
LIST1ST  DS    0H                                                               
*                                                                               
         BRAS  RE,HIGH             READ FIRST PASSIVE                           
*                                                                               
LISTLOOP DS    0H                                                               
*                                                                               
         LA    R6,KEY              POINT TO KEY AREA                            
*                                                                               
         CLI   ERRAREA,0           EXIT ON ERRORS                               
         BNE   EXXMOD                                                           
*                                                                               
         CLC   RMKGKEY(RMGSPSAL-RMKGKEY),KEYSAVE                                
         BE    *+14                                                             
         XC    SAVEKEY,SAVEKEY     SET TO RESTART LIST                          
         B     LISTDONE            DONE IF BREAK IN REP                         
*                                                                               
LISTSAL  DS    0H                  FILTER ON SALESPERSON                        
*                                                                               
         OC    TBLSLS2,TBLSLS2     IF SALESPERSON FILTER                        
         BZ    *+14                                                             
         CLC   RMGSPSAL,TBLSLS2       MATCH ON SALESPERSON CODE                 
         BNE   LISTCONT                                                         
*                                                                               
         OC    SVDVSLS,SVDVSLS     IF DEVELOPMENTAL SALESPERSON ENTERED         
         BZ    *+14                                                             
         CLC   RMGSPSAL,SVDVSLS       MATCH ON DEVSAL CODE                      
         BNE   LISTCONT                                                         
*                                                                               
LISTSALX DS    0H                                                               
*                                                                               
LISTSTN  DS    0H                                                               
*                                                                               
*        CLC   RMGSPSTA,TBLSTA     MATCH ON STATION                             
*        BNE   LISTCONT                                                         
*                                                                               
LISTSTNX DS    0H                                                               
*                                                                               
         OC    TBLCDATS,TBLCDATS   IF FIRST OFFER START ENTERED                 
         BZ    *+14                                                             
         CLC   RMGSPDAT,TBLCDATS      MUST BE ON OR AFTER                       
         BL    LISTCONT                                                         
*                                                                               
         OC    TBLCDATE,TBLCDATE   IF FIRST OFFER END   ENTERED                 
         BZ    *+14                                                             
         CLC   RMGSPDAT,TBLCDATE      MUST BE ON OR BEFORE                      
         BH    LISTCONT                                                         
*                                                                               
*        FILTER ON WIP, OFFER AND DARE STATUS                                   
*                                                                               
LISTOPS  DS    0H                                                               
*                                                                               
         CLI   LISTWOPS,LISTWREP   IF ONLY REP CREATED OFFERS                   
         BNE   *+16                                                             
         TM    RMGSPSTT,RMKGSCRQ      DROP IF   NOT REP CREATED                 
         BNO   LISTCONT                                                         
         B     LISTWOPX                                                         
*                                                                               
         CLI   LISTWOPS,LISTWSTA   IF ONLY STA CREATED OFFERS                   
         BNE   *+16                                                             
         TM    RMGSPSTT,RMKGSCRQ      DROP IF   REP CREATED                     
         BO    LISTCONT                                                         
         B     LISTWOPX                                                         
*                                                                               
LISTWOPX DS    0H                                                               
*                                                                               
         CLI   LISTOOPS,LISTOSLF   IF ONLY SELF APPLIED OFFERS                  
         BNE   *+16                                                             
         TM    RMGSPSTT,RMKGSLFQ      DROP IF   NOT SELF APPLIED                
         BNO   LISTCONT                                                         
         B     LISTOPSX                                                         
*                                                                               
         CLI   LISTOOPS,LISTOAPL   IF ONLY APPLIED OFFERS                       
         BNE   *+16                                                             
         TM    RMGSPSTT,RMKGSAPQ      DROP IF   NOT APPLIED                     
         BNO   LISTCONT                                                         
         B     LISTOPSX                                                         
*                                                                               
         TM    RMGSPSTT,RMKGSAPQ+RMKGSLFQ   ELSE DROP IF APL OR SELF            
         BNZ   LISTCONT                                                         
*                                                                               
         CLI   LISTOOPS,LISTOCAN   IF ONLY CANCELLED OFFERS                     
         BNE   *+16                                                             
         TM    RMGSPSTT,RMKGSCNQ      DROP IF   NOT CANCELLED                   
         BNO   LISTCONT                                                         
         B     LISTOPSX                                                         
*                                                                               
         TM    RMGSPSTT,RMKGSCNQ   ELSE DROP IF       CANCELLED                 
         BO    LISTCONT                                                         
*                                                                               
         CLI   LISTOOPS,0          SKIP IF NO OTHER OPTION FILTERS              
         BNE   *+8                                                              
         CLI   LISTWOPS,0                                                       
         BE    LISTOPSX                                                         
*                                                                               
         CLI   LISTOOPS,LISTONEW   IF ONLY NEW OFFERS                           
         BNE   *+16                                                             
         TM    RMGSPSTT,X'FF'-RMKGSCRQ  NO ACTION ALLOWED                       
         BNZ   LISTCONT                                                         
         B     LISTOPSX                                                         
*                                                                               
         CLI   LISTOOPS,LISTOREV   IF ONLY REVISED OFFERS                       
         BNE   *+24                                                             
         TM    RMGSPSTT,RMKGSRVQ      DROP IF NOT REVISED                       
         BNO   LISTCONT                                                         
         TM    RMGSPWIP,RMGF2WPQ      DROP IF WIP                               
         BO    LISTCONT                                                         
         B     LISTOPSX                                                         
*                                                                               
         CLI   LISTOOPS,LISTOSRV   IF ONLY REVISED BY STATION OFFERS            
         BNE   *+32                                                             
         TM    RMGSPSTT,RMKGSRVQ      DROP IF A NOT REVISED                     
         BNO   LISTCONT                                                         
         TM    RMGSPWIP,RMGF2WPQ      DROP IF WIP                               
         BO    LISTCONT                                                         
         TM    RMGSPWIP,RMGF2STQ      DROP IF NOT STA REV                       
         BNO   LISTCONT                                                         
         B     LISTOPSX                                                         
*                                                                               
         CLI   LISTOOPS,LISTORRV   IF ONLY REVISED BY REP OFFERS                
         BNE   *+32                                                             
         TM    RMGSPSTT,RMKGSRVQ      DROP IF   NOT REVISED                     
         BNO   LISTCONT                                                         
         TM    RMGSPWIP,RMGF2WPQ      DROP IF WIP                               
         BO    LISTCONT                                                         
         TM    RMGSPWIP,RMGF2RPQ      DROP IF NOT REP REV                       
         BNO   LISTCONT                                                         
         B     LISTOPSX                                                         
*                                                                               
         CLI   LISTOOPS,LISTOREJ   IF ONLY REJECTED OFFERS                      
         BNE   *+16                                                             
         TM    RMGSPSTT,RMKGSRJQ      DROP IF   NOT REJECTED                    
         BNO   LISTCONT                                                         
         B     LISTOPSX                                                         
*                                                                               
         CLI   LISTOOPS,LISTOCAN   IF ONLY CANCELLED OFFERS                     
         BNE   *+16                                                             
         TM    RMGSPSTT,RMKGSCNQ      DROP IF   NOT REJECTED                    
         BNO   LISTCONT                                                         
         B     LISTOPSX                                                         
*                                                                               
LISTOPSX DS    0H                                                               
*                                                                               
         OC    TBLSTA,TBLSTA       IF FILTERING ON STATION                      
         BZ    *+14                                                             
         CLC   RMGSPSTA,TBLSTA        MUST BE EQUAL                             
         BNE   LISTCONT                                                         
*                                                                               
FLTADV   DS    0H                                                               
*                                                                               
         OC    TBLADV,TBLADV       IF FILTERING ON ADVERTISER                   
         BZ    *+14                                                             
         CLC   RMGSPADV,TBLADV        MUST BE EQUAL                             
         BNE   LISTCONT                                                         
*                                                                               
         LAY   RF,SETADVS          POINT TO ADV SET DISPLACEMENT                
         OC    0(L'SETADVS,RF),0(RF)  IF FILTERING ON ADV SET                   
         BZ    FLTADVX                                                          
*                                                                               
         GOTOR SETFLT,DMCB,0(RF),RMGSPADV  FILTER USING SET                     
         BNE   LISTCONT            FAILS FILTER                                 
*                                                                               
FLTADVX  DS    0H                                                               
*                                                                               
*        CHECK FOR OTHER FILTERING                                              
*                                                                               
         LAY   R4,SETDISPD         ESTABLISH SET DISPLACEMENTS                  
         USING SETDISPD,R4                                                      
*                                                                               
         OC    TBLGRP,TBLGRP       CONTINUE IF GROUP      FILTER                
         BNZ   LISTREAD                                                         
         OC    TBLSBGP,TBLSBGP     CONTINUE IF SUB-GROUP  FILTER                
         BNZ   LISTREAD                                                         
         OC    SETGRPS,SETGRPS                                                  
         BNZ   LISTREAD                                                         
*                                                                               
         OC    TBLOFF,TBLOFF       CONTINUE IF OFFICE     FILTER                
         BNZ   LISTREAD                                                         
         OC    SETOFFS,SETOFFS                                                  
         BNZ   LISTREAD                                                         
*                                                                               
         OC    TBLAGY,TBLAGY       CONTINUE IF AGENCY     FILTER                
         BNZ   LISTREAD                                                         
         OC    TBLAOFF,TBLAOFF     CONTINUE IF AGENCY OFC FILTER                
         BNZ   LISTREAD                                                         
         OC    SETAGYS,SETAGYS                                                  
         BNZ   LISTREAD                                                         
*                                                                               
         OC    TBLBGN,TBLBGN       CONTINUE IF START DATE FILTER                
         BNZ   LISTREAD                                                         
         OC    TBLEND,TBLEND       CONTINUE IF END   DATE FILTER                
         BNZ   LISTREAD                                                         
*                                                                               
         OC    TBLDEMO,TBLDEMO     CONTINUE IF DEMO      FILTER                 
         BNZ   LISTREAD                                                         
*                                                                               
         CLI   LISTDOPS,LISTDDRE   IF ONLY DARE OFFERS                          
         BE    *+8                                                              
         CLI   LISTDOPS,LISTDDRX   IF ONLY NON DARE OFFERS                      
         BE    LISTREAD               READ RECORD                               
*                                                                               
         B     LISTFLTX            NO OTHER FILTERS                             
*                                                                               
*        READ IN RECORD                                                         
*                                                                               
LISTREAD DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         BRAS  RE,GETREC                                                        
         NI    DMINBTS,X'FF'-X'08' TURN OFF BIT                                 
*                                                                               
         LA    R6,RECORD           POINT TO RECORD                              
         USING RMKGREC,R6          ESTABLISH MAKEGOOD RECORD                    
         TM    RMKGCNTL,X'80'      DELETED ?                                    
         BO    LISTCONT                                                         
*                                                                               
         CLI   LISTDOPS,LISTDDRE   IF ONLY DARE OFFERS                          
         BNE   *+18                                                             
         OC    RMKGDARN,RMKGDARN      DROP IF NO DARE NUMBER PRESENT            
         BZ    LISTCONT                                                         
         B     LISTDOPX                                                         
*                                                                               
         CLI   LISTDOPS,LISTDDRX   IF ONLY NON DARE OFFERS                      
         BNE   *+18                                                             
         OC    RMKGDARN,RMKGDARN      DROP IF DARE NUMBER PRESENT               
         BNZ   LISTCONT                                                         
         B     LISTDOPX                                                         
*                                                                               
LISTDOPX DS    0H                                                               
*                                                                               
         LA    R8,RECORD           POINT TO RECORD                              
*                                                                               
         MVI   ELCODE,RMKGXELQ     FIND EXTRA INFO ELM                          
         BRAS  RE,GETEL                                                         
         BNZ   LISTFLTX            NONE FOUND                                   
*                                                                               
         USING RMKGXEL,R8          ESTABLISH EXTRA INFO ELEMENT                 
*                                                                               
LISTFLT  DS    0H                  FILTER ON EXTRA DATA                         
*                                                                               
FLTGRP   DS    0H                                                               
*                                                                               
         OC    TBLGRP,TBLGRP       IF STATION GROUP FILTER                      
         BZ    *+14                                                             
         CLC   TBLGRP,RMKGXGRP        DROP IF NO MATCH                          
         BNE   LISTCONT                                                         
*                                                                               
         OC    TBLSBGP,TBLSBGP     IF STATION SUBGROUP FILTER                   
         BZ    *+14                                                             
         CLC   TBLSBGP,RMKGXGRP+1     DROP IF NO MATCH                          
         BNE   LISTCONT                                                         
*                                                                               
         LAY   RF,SETGRPS          POINT TO GRP SET DISPLACEMENT                
         OC    0(L'SETGRPS,RF),0(RF)  IF FILTERING ON GRP SET                   
         BZ    FLTGRPX                                                          
*                                                                               
         GOTOR SETFLT,DMCB,0(RF),RMKGXGRP  FILTER USING SET                     
         BNE   LISTCONT            FAILS FILTER                                 
*                                                                               
FLTGRPX  DS    0H                                                               
*                                                                               
FLTOFF   DS    0H                                                               
*                                                                               
         OC    TBLOFF,TBLOFF       IF OFFICE           FILTER                   
         BZ    *+14                                                             
         CLC   TBLOFF,RMKGXOFF        DROP IF NO MATCH                          
         BNE   LISTCONT                                                         
*                                                                               
         LAY   RF,SETOFFS          POINT TO OFF SET DISPLACEMENT                
         OC    0(L'SETOFFS,RF),0(RF)  IF FILTERING ON OFF SET                   
         BZ    FLTOFFX                                                          
*                                                                               
         GOTOR SETFLT,DMCB,0(RF),RMKGXOFF  FILTER USING SET                     
         BNE   LISTCONT            FAILS FILTER                                 
*                                                                               
FLTOFFX  DS    0H                                                               
*                                                                               
FLTAGY   DS    0H                                                               
*                                                                               
         OC    TBLAGY,TBLAGY       IF AGENCY           FILTER                   
         BZ    *+14                                                             
         CLC   TBLAGY,RMKGXAGY        DROP IF NO MATCH                          
         BNE   LISTCONT                                                         
*                                                                               
         OC    TBLAOFF,TBLAOFF     IF AGENCY OFFICE    FILTER                   
         BZ    *+14                                                             
         CLC   TBLAOFF,RMKGXAOF       DROP IF NO MATCH                          
         BNE   LISTCONT                                                         
*                                                                               
         LAY   RF,SETAGYS          POINT TO AGY SET DISPLACEMENT                
         OC    0(L'SETAGYS,RF),0(RF)  IF FILTERING ON AGY SET                   
         BZ    FLTAGYX                                                          
*                                                                               
         GOTOR SETFLT,DMCB,0(RF),RMKGXAGY  FILTER USING SET                     
         BNE   LISTCONT            FAILS FILTER                                 
*                                                                               
FLTAGYX  DS    0H                                                               
*                                                                               
         OC    TBLBGN,TBLBGN       IF START MONTH      FILTER                   
         BZ    *+14                                                             
         CLC   TBLBGN,RMKGXFLT+3      DROP IF CONTRACT ENDS BEFORE              
         BH    LISTCONT                                                         
*                                                                               
         OC    TBLEND,TBLEND       IF END   MONTH      FILTER                   
         BZ    *+14                                                             
         CLC   TBLEND,RMKGXFLT        DROP IF CONTRACT STARTS AFTER             
         BL    LISTCONT                                                         
*                                                                               
LISTFLTX DS    0H                                                               
*                                                                               
*        DISPLAY A LISTLINE                                                     
*                                                                               
LISTDSP  DS    0H                                                               
*                                                                               
         LA    R6,KEY              POINT TO MKG OFFER PASSIVE                   
*                                                                               
         MVC   LSMKGSAL,RMGSPSAL   SALESPERSON                                  
         MVC   LSMKGSTA,RMGSPSTA   STATION                                      
*                                                                               
         BRAS  RE,ADVGET           FIND ADVERTISER NAME                         
*                                                                               
         MVC   LSMKGADV,ADVNMSV    ADVERTISER                                   
*                                                                               
         ZAP   WORK(5),=P'0'       CONTRACT NUMBER                              
         MVC   WORK(4),RMGSPCON                                                 
         ZAP   WORK+10(5),=P'999999999'  UNCOMPLEMENT                           
         SP    WORK+10(5),WORK(5)                                               
*                                                                               
         UNPK  LSMKGCON(9),WORK+10(5)                                           
         MVI   LSMKGCON+8,C' '                                                  
*                                                                               
         MVC   LSMKGOFR,RMGSPGRP   OFFER GROUP                                  
*                                                                               
         MVI   LSMKGRS,C'R'        ASSUME CREATED BY REP                        
         TM    RMGSPSTT,RMKGSCRQ   SET TO STATION IF NOT                        
         BO    *+8                                                              
         MVI   LSMKGRS,C'S'           CREATED BY STATION                        
*                                                                               
*        OFFER STATUS                                                           
*                                                                               
         TM    RMGSPSTT,X'FF'-RMKGSPAQ-RMKGSCRQ  STATUS NEW                     
         BNZ   *+14                                                             
         MVC   LSMKGOFS,=CL8'NEW'                                               
         B     LISTOFSX                                                         
*                                                                               
         TM    RMGSPSTT,RMKGSRVQ      STATUS REVISED?                           
         BNO   LISTREVN                                                         
*                                                                               
         TM    RMGSPWIP,RMGF2STQ      STATION REVISED?                          
         BNO   *+14                                                             
         MVC   LSMKGOFS,=CL8'STAREV'                                            
         B     LISTOFSX                                                         
*                                                                               
         TM    RMGSPWIP,RMGF2RPQ      REP REVISED                               
         BNO   *+10                                                             
         MVC   LSMKGOFS,=CL8'REPREV'                                            
*                                                                               
         B     LISTOFSX                                                         
*                                                                               
LISTREVN DS    0H                                                               
*                                                                               
*                                                                               
         TM    RMGSPSTT,RMKGSLFQ   SELF APPLIED STATUS                          
         BNO   *+14                                                             
         MVC   LSMKGOFS,=CL8'SELFAPL'                                           
         B     LISTOFSX                                                         
*                                                                               
         TM    RMGSPSTT,RMKGSAPQ   APPLIED STATUS                               
         BNO   *+14                                                             
         MVC   LSMKGOFS,=CL8'APPLIED'                                           
         B     LISTOFSX                                                         
*                                                                               
         TM    RMGSPSTT,RMKGSBOQ   BACKED OUT STATUS                            
         BNO   *+14                                                             
         MVC   LSMKGOFS,=CL8'BACK OUT'                                          
         B     LISTOFSX                                                         
*                                                                               
         TM    RMGSPSTT,RMKGSRCQ   RECALLED STATUS                              
         BNO   *+14                                                             
         MVC   LSMKGOFS,=CL8'RECALLED'                                          
         B     LISTOFSX                                                         
*                                                                               
         TM    RMGSPSTT,RMKGSRJQ   REJECTED STATUS                              
         BNO   *+14                                                             
         MVC   LSMKGOFS,=CL8'REJECT'                                            
         B     LISTOFSX                                                         
*                                                                               
         TM    RMGSPSTT,RMKGSCNQ   CANCELLED STATUS                             
         BNO   *+14                                                             
         MVC   LSMKGOFS,=CL8'CANCEL'                                            
         B     LISTOFSX                                                         
*                                                                               
LISTOFSX DS    0H                                                               
*                                                                               
*        DARE OFFER STATUS                                                      
*                                                                               
         TM    RMGSPDST,RMGF1MER  ERROR STATUS                                  
         BNO   *+14                                                             
         MVC   LSMKGDRS,=CL8'ERROR'                                             
         B     LISTDFSX                                                         
*                                                                               
         TM    RMGSPDST,RMGF1MSN  SENT  STATUS                                  
         BNO   *+14                                                             
         MVC   LSMKGDRS,=CL8'SENT'                                              
         B     LISTDFSX                                                         
*                                                                               
         TM    RMGSPDST,RMGF1MAR  APPROVED STATUS                               
         BNO   *+14                                                             
         MVC   LSMKGDRS,=CL8'APPROVED'                                          
         B     LISTDFSX                                                         
*                                                                               
         TM    RMGSPDST,RMGF1MRR  REJECTED STATUS                               
         BNO   *+14                                                             
         MVC   LSMKGDRS,=CL8'REJECTED'                                          
         B     LISTDFSX                                                         
*                                                                               
         TM    RMGSPDST,RMGF1MCF  APPLIED STATUS                                
         BNO   *+14                                                             
         MVC   LSMKGDRS,=CL8'APPLIED'                                           
         B     LISTDFSX                                                         
*                                                                               
         TM    RMGSPDST,RMGF1MCN  CANCELLED STATUS                              
         BNO   *+14                                                             
         MVC   LSMKGDRS,=CL8'CANCEL'                                            
         B     LISTDFSX                                                         
*                                                                               
         TM    RMGSPDST,RMGF1MCR  RESENT  STATUS                                
         BNO   *+14                                                             
         MVC   LSMKGDRS,=CL8'RESENT'                                            
         B     LISTDFSX                                                         
*                                                                               
         TM    RMGSPDST,RMGF1MCM  CANCEL WITH MORE STATUS                       
         BNO   *+14                                                             
         MVC   LSMKGDRS,=CL8'RECALLED'                                          
         B     LISTDFSX                                                         
*                                                                               
*        NO FLAGGED DARE STATUS BUT STILL MAYBE DARE MKG                        
*                                                                               
         BRAS  RE,GETREC           READ IN MAKEGOOD RECORD                      
*                                                                               
         LA    R6,RECORD           ESTABLISH MKG REC                            
         USING RMKGREC,R6                                                       
*                                                                               
         OC    RMKGDARN,RMKGDARN   IF DARE NUMBER PRESENT                       
         BZ    *+10                                                             
         MVC   LSMKGDRS,=CL8'NEW'     FLAG AS NEW                               
*                                                                               
LISTDFSX DS    0H                                                               
*                                                                               
*        FIRST AIR DATE                                                         
*                                                                               
         LA    R6,KEY              RE-ESTBLISH KEY                              
*                                                                               
         GOTO1 VDATCON,DMCB,(2,RMGSPDAT),(11,LSMKG1ST)                          
*                                                                               
         OC    RMGSPDAT,RMGSPDAT   SKIP IF NO FIRST AIR DATE                    
         BZ    LIST1STX                                                         
*                                                                               
         LAY   R1,TODAY                                                         
         CLC   RMGSPDAT,0(R1)      IF 1ST OFFER BEFORE TODAY                    
         BNL   *+14                                                             
         MVC   LSMKG1ST+8(2),=C'**'    FLAG                                     
         B     LIST1STX                                                         
*                                                                               
         LAY   R1,TODAY10                                                       
         CLC   RMGSPDAT,0(R1)      IF WITHIN NEXT 10 DAYS                       
         BH    *+8                                                              
         MVI   LSMKG1ST+8,C'*'        FLAG                                      
*                                                                               
LIST1STX DS    0H                                                               
*                                                                               
         CLI   PRNT,1              IF PRINTING                                  
         BE    LISTPRNT               DIFFERENT PROCESSING                      
*                                                                               
         FOUT  (R2)                PUT LINE ON SCREEN                           
*                                                                               
*        NEXT SCREEN LINE                                                       
*                                                                               
         LLC   RF,0(R2)            LENGTH OF SCREEN LINE                        
         LA    R2,0(RF,R2)         A(NEXT LINE)                                 
*                                                                               
         LA    RE,RISBOTMH         A(LAST LINE ON SCREEN)                       
*                                                                               
         CR    RE,R2               DONE IF END OF LIST LINES REACHED            
         BH    LISTCONT                                                         
*                                                                               
         MVC   SAVEKEY,KEY         SAVE LAST KEY                                
*                                                                               
         B     LISTDONE                                                         
*                                                                               
LISTPRNT DS    0H                                                               
*                                                                               
         BRAS  RE,PRINT            PRINT LINE                                   
*                                                                               
LISTCONT DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD                             
*                                                                               
         B     LISTLOOP                                                         
*                                                                               
LISTDONE DS    0H                                                               
*                                                                               
         CLI   PRNT,1              IF PRINTING REPORT                           
         BNE   LISTDON1                                                         
*                                                                               
         BRAS  RE,LASTP               DO END OF REPORT ROUTINE                  
*                                                                               
LISTDON1 DS    0H                                                               
*                                                                               
         MVI   NEXTBYTE,3          SET TO COME BACK                             
         MVC   WOPSSAVE,LISTWOPS   SAVE LISTO FILTER                            
         MVC   OOPSSAVE,LISTOOPS   SAVE LISTO FILTER                            
         MVC   DOPSSAVE,LISTDOPS   SAVE LISTO FILTER                            
*                                                                               
         LA    R2,RISSTAH          CURSOR HERE                                  
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         SPACE 3                                                                
         GETEL R8,34,ELCODE                                                     
*                                                                               
         DROP  R2,R6,R8                                                         
*                                                                               
         EJECT                                                                  
TITLE1   DC    C' SAL  STA   ADVERTISER NAME      CON#     OFR S/R OFRSX        
               TAT  DARE     OFRFIRSTAIR'                                       
*                123456789º123456789º123456789º                                 
PFTITL   DC    C'PF1=PRNT 2=DSM 5=MGL 6=DARE'                                   
         EJECT                                                                  
* INITIALIZ PRINT QUE                                                           
INITP    NTR1  BASE=*,LABEL=*                                                   
         MVI   MAXLNES,78          MAX LINES                                    
         MVI   PG,1                PG COUNT                                     
* SET 1ST TIME FOR PRTQUE                                                       
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         MVI   MYP-1,0                                                          
         XC    MYP,MYP                                                          
*                                                                               
         LA    R3,MYP-1                                                         
         USING PQPLD,R3            PRINT QUEUE PRINT LNE                        
*                                                                               
INIT30   EQU   *                                                                
         MVI   QLEXTRA,X'FF'       INDICATE NEW STYLE LIST                      
         MVC   QLSUBID,=C'RIS'                                                  
         MVI   QLCLASS,C'Z'                                                     
         MVC   QLDESC,=C'RISLSTO'                                               
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'       KEEP PRINTED REPORT 2 HOURS                  
*                                                                               
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                 
         TM    DMCB+8,X'FF'                                                     
         BZ    INITPX                                                           
         DC    H'0',C'$PQFULL'                                                  
*                                                                               
INITPX   MVC   HALF2,QLREPRNO      SAVE REPORT NUMBER                           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* - HEADLINES WHEN PRINTING REPORT                                              
*                                                                               
DOHEADS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   LNE,0                 LNE CTR                                    
*                                                                               
         XC    MYP,MYP                                                          
         MVI   MYP-1,X'89'           SKIP TO NEW PG                             
         BRAS  RE,PRINT                                                         
*                                                                               
         MVC   MYP(7),=C'STATION'                                               
         MVC   MYP+8(7),RISSTA                                                  
         MVC   MYP+16(12),RISSTAN                                               
*                                                                               
*        PRINT SCREEN HEADER FIELDS                                             
*                                                                               
         MVC   MYP(7),=C'STATION'                                               
         MVC   MYP+8(7),RISSTA                                                  
         MVC   MYP+16(12),RISSTAN                                               
         MVC   MYP+35(6),=C'OFFICE'                                             
         MVC   MYP+44(4),RISOFF                                                 
         MVC   MYP+70(6),=C'AGENCY'                                             
         MVC   MYP+78(7),RISAGY                                                 
         BAS   RE,PRINT                                                         
         MVC   MYP(7),=C'ADVRTSR'                                               
         MVC   MYP+8(4),RISADV                                                  
         MVC   MYP+35(7),=C'SPERSON'                                            
         MVC   MYP+44(7),RISSLS                                                 
         MVC   MYP+70(7),=C'CATEGRY'                                            
         MVC   MYP+78(4),RISCTG                                                 
         BAS   RE,PRINT                                                         
         MVC   MYP(6),=C'DEMO  '                                                
         MVC   MYP+8(11),RISTAR                                                 
         MVC   MYP+35(8),=C'CREATION'                                           
         MVC   MYP+44(8),RISCRT                                                 
         MVC   MYP+70(10),=C'CON STATUS'                                        
         MVC   MYP+81(1),RISCST                                                 
         BAS   RE,PRINT                                                         
         MVC   MYP(6),=C'MONTHS'                                                
         MVC   MYP+8(13),RISMNTS                                                
         MVC   MYP+35(7),=C'OPTIONS'                                            
         MVC   MYP+43(17),RISDATE                                               
         MVC   MYP+70(8),=C'CON TYPE'                                           
         MVC   MYP+79(2),RISFILT                                                
         GOTO1 VDATCON,DMCB,(5,0),(5,MYP+100)                                   
         BAS   RE,PRINT                                                         
         MVC   MYP+70(5),=C'BLG $'                                              
         MVC   MYP+75(1),RISALTD                                                
         MVC   MYP+100(4),=C'PAGE'                                              
         EDIT  (B1,PG),(3,MYP+105),ALIGN=LEFT                                   
*                                                                               
         MVI   MYP-1,SPACE3          SPACE 3 LINES AFTER PRINT                  
         BRAS  RE,PRINT                                                         
*                                                                               
         MVC   MYP+40(19),=C'    DETAIL LISTING '                               
         BRAS  RE,PRINT                                                         
         MVC   MYP+40(19),=C'    ______ _______ '                               
         MVI   MYP-1,SPACE3          SPACE 3 LINES AFTER PRINT                  
         BRAS  RE,PRINT                                                         
*                                                                               
         LAY   R1,TITLE1           PRINT COLUMN TITLES                          
         MVC   MYP(L'TITLE1),0(R1)                                              
         BRAS  RE,PRINT                                                         
*                                                                               
         MVI   MYP-1,SPACE2          SPACE 2 LINES AFTER PRINT                  
         BRAS  RE,PRINT                                                         
*                                                                               
DOHEADX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*********************************************************************           
* PRINT ROUTINE                                                                 
*********************************************************************           
*                                                                               
PRINT    NTR1  BASE=*,LABEL=*                                                   
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                 
         TM    DMCB+8,X'FF'        ERROR?                                       
         BZ    PRT10                                                            
         DC    H'0',C'$PQFULL'     PRINT ERROR                                  
PRT10    XC    MYP,MYP                                                          
         ZIC   RE,LNE                                                           
         LA    RE,1(RE)            BUMP LNE COUNT                               
         CLI   MYP-1,SPACE2        2 LINES                                      
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         B     PRTXIT                                                           
         CLI   MYP-1,SPACE3        3 LINES                                      
         BNE   PRTXIT                                                           
         LA    RE,2(RE)                                                         
*                                                                               
PRTXIT   STC   RE,LNE                                                           
         MVI   MYP-1,SPACE1        DEFAULT TO 1 ALWAYS                          
**       C     RE,=F'74'           ENSURE LINES LEFT                            
         C     RE,=F'52'           ENSURE LINES LEFT                            
         BL    PRTX                                                             
         ZIC   R1,PG               NOT ENOUGH/BUMP PAGE NUMBER                  
         LA    R1,1(R1)                                                         
         STC   R1,PG                                                            
         BRAS  RE,DOHEADS          DO HEADLINES                                 
                                                                                
PRTX     XIT1                                                                   
*                                                                               
SPACE1   EQU   X'09'                                                            
SPACE2   EQU   X'11'                                                            
SPACE3   EQU   X'19'                                                            
         EJECT                                                                  
* LAST TIME WHEN PRINTING REPORT                                                
LASTP    NTR1  BASE=*,LABEL=*                                                   
         MVI   MYP-1,X'FF'           LAST TIME                                  
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                 
         CLI   DMCB+8,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,RISMESSH                                                      
         MVC   8(43,R2),=C'*ACTION COMPLETED  RIS,      ON PRINT QUEUE'         
         EDIT  (B2,HALF2),(5,31(R2)),ALIGN=LEFT                                 
         FOUT (R2)                                                              
         MVI   PRNT,0                                                           
*                                                                               
         NI    RISMNTSH+4,X'DF'    FORCE VALIDATION WHEN RETURNING              
*                                                                               
LASTPX   DS    0H                                                               
         XIT1                                                                   
                                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        READ ADVERTISER RECORD TO GET NAME                           *         
*              FIRST CHECK TO SEE IF CURRENT ADV SAME AS LAST ONE     *         
*                                                                     *         
*NTRY    R6==> RMGSPKEY                                               *         
*                                                                     *         
*EXIT    TBLADV   ADV CODE                                            *         
*        ADVNMSV= ADVERTISER NAME                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ADVGET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RMGSPTYP,R6         ESTABLISH MAKEGOOD OFFER PASSIVE             
*                                                                               
         OC    ADVNMSV,ADVNMSV     CONTINUE IF FIRST TIME IN                    
         BZ    *+14                                                             
         CLC   TBLADV,RMGSPADV     SKIP IF ADV UNCHANGED                        
         BE    ADVGETX                                                          
*                                                                               
         XC    ADVNMSV,ADVNMSV     INIT ADV NAME SAVEAREA                       
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
         LA    R6,SAVEKEY          POINT TO SAVED KEY                           
*                                                                               
         L     R0,AIOAREA          SAVE CURRENT RECORD I/O AREA                 
         LA    RF,IOAREA           SET A(IOAREA)                                
         ST    RF,AIOAREA                                                       
*                                                                               
         XC    KEY,KEY             ESTABLISH AS ADV KEY                         
         LA    R5,KEY                                                           
         USING RADVKEY,R5                                                       
*                                                                               
         MVI   RADVKTYP,X'08'      SET ADV RECORD TYPE                          
         MVC   RADVKADV,RMGSPADV   SET ADVERTISER CODE                          
         MVC   RADVKREP,RMGSPREP   SET REP CODE                                 
*                                                                               
         CLC   MASTRREP,=C'  '     IF THERE IS A MASTER REP                     
         BNH   *+10                                                             
         MVC   RADVKREP,MASTRREP      USE IT                                    
*                                                                               
         BRAS  RE,HIGH             READ FOR RECORD                              
*                                                                               
         CLC   RADVKEY,KEYSAVE     SKIP IF RECORD NOT FOUND                     
         BNE   ADVGET90                                                         
*                                                                               
         DROP  R5                                                               
*                                                                               
         BRAS  RE,GETREC           READ IN ADV RECORD                           
*                                                                               
         L     R8,AIOAREA                                                       
         MVI   ELCODE,X'01'        FIND ADVERTISER ELEMENT                      
         BRAS  RE,GETEL                                                         
         BNZ   ADVGET90            SKIP IF NOT FOUND                            
*                                                                               
         USING RADVELEM,R8         ESTABLISH ADVERTISER ELEMENT                 
*                                                                               
         MVC   ADVNMSV(L'RADVNAME),RADVNAME   SAVE ADV NAME                     
*                                                                               
ADVGET90 DS    0H                                                               
*                                                                               
         ST    R0,AIOAREA          RESTORE A(IOAREA)                            
         MVC   KEY,SAVEKEY         RESTORE INCOMING KEY                         
*                                                                               
         BRAS  RE,HIGH             RESTORE FILE POINTERS                        
*                                                                               
ADVGETX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        CHECK IF PASSED ADV CODE IS IN THE SET REQUESTED             *         
*              OR NOT IN IF SET IS EXCLUSION SET                      *         
*                                                                     *         
*NTRY    P0    A(DISPLACEMENT TO SET)                                 *         
*        P1    A(ITEM TO BE TESTED)                                   *         
*                                                                     *         
*EXIT    NEQ   ITEM NOT IN SET OR IN AN EXCLUSION SET                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SETFLT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,0(R1)            GET DISPLACMENT IN TWA TO SET                
*                                                                               
         LTR   R4,R4               DONE IF NO SET                               
         BZ    SETFLTOK                                                         
*                                                                               
         L     R4,0(R4)            GET DISPLACEMENT OF SET                      
         AR    R4,RA               POINT TO SET                                 
*                                                                               
         TM    0(R4),X'80'         MUST BE A SET                                
         BNO   SETFLTOK                                                         
*                                                                               
         L     R5,4(R1)            POINT TO ITEM TO BE TESTED                   
*                                                                               
         LA    R6,4(R4)            POINT TO FIRST ITEM IN SET                   
*                                                                               
         LLC   RF,3(R4)            GET LENGTH OF EACH ITEM IN SET               
*                                                                               
         LR    RE,RF               SAVE LENGTH OF ENTRY IN SET                  
*                                                                               
         CLC   =C'AG',1(R4)        ID AGENCY SET TEST FOR AGY ONLY              
         BNE   *+8                                                              
         LHI   RF,4                                                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
SETFLTLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             DONE AT END OF SET                           
         BE    SETFLTDN                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),0(R5)       SKIP IF ITEM IS NOT IN SET                   
         BNE   SETFLTCN                                                         
*                                                                               
         CLC   =C'AG',1(R4)        FOUND IF NOT AGENCY SET                      
         BNE   SETFLTFD                                                         
*                                                                               
         CLC   4(2,R6),=C'  '      OKAY IF NO AGENCY OFFICE FILTER              
         BNH   SETFLTFD                                                         
*                                                                               
         CLC   4(2,R6),4(R5)       AGENCY OFFICE MUST MATCH                     
         BE    SETFLTFD                                                         
*                                                                               
SETFLTCN DS    0H                                                               
*                                                                               
         LA    R6,0(RE,R6)         BUMP TO NEXT ITEM IN LIST                    
         B     SETFLTLP                                                         
*                                                                               
SETFLTFD DS    0H                  ITEM IS IN THE SET                           
*                                                                               
         TM    0(R4),X'40'         IF AN EXCLUSION SET                          
         BO    SETFLTNO               DROP ITEM                                 
*                                                                               
         B     SETFLTOK            ELSE KEEP                                    
*                                                                               
SETFLTDN DS    0H                  ITEM IS NO IN SET                            
         TM    0(R4),X'40'         IF AN EXCLUSION SET                          
         BO    SETFLTOK               KEEP ITEM                                 
*                                                                               
         B     SETFLTNO            ELSE DROP                                    
*                                                                               
SETFLTOK DS    0H                  ITEM PART OF SET                             
         CR    RB,RB               SET EQ CC                                    
         B     SETFLTX                                                          
*                                                                               
SETFLTNO DS    0H                  ITEM NOT PART OF SET                         
         LTR   RB,RB               SET NEQ CC                                   
*                                                                               
SETFLTX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
DBLOK    DS    0H                                                               
       ++INCLUDE DEDBLOCK                                                       
                                                                                
TEMPIO   DS    CL1000                                                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE RGENEROL                                                       
       ++INCLUDE RERISWRK                                                       
       ++INCLUDE REPIOBLK                                                       
       ++INCLUDE REGENMKG                                                       
       ++INCLUDE DMPRTQL                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'184RERIS07   11/20/14'                                      
         END                                                                    
