*          DATA SET TAREP66    AT LEVEL 002 AS OF 07/21/11                      
*PHASE T70366C,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70366 - BILLING SUMMARY OF INVOICES'                           
T70366   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70366,R6                                                      
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   YES                                                              
                                                                                
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
                                                                                
         BRAS  RE,PREP             PRINT REPORT                                 
                                                                                
         GOTO1 SORTER,DMCB,=C'END'                                              
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,12,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=12'                                    
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                             *         
***********************************************************************         
                                                                                
VOPT     NTR1  BASE=*,LABEL=*                                                   
         MVI   PROSTAT,0                                                        
                                                                                
         CLI   BSMOPTH+5,0                                                      
         JE    XIT                                                              
                                                                                
         LA    R2,BSMOPTH                                                       
                                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
         ZIC   R0,4(R1)                                                         
                                                                                
VOPT10   CLC   =C'CAT',SCDATA1                                                  
         JNE   VOPT20                                                           
         MVC   FILTCAT,SCDATA2                                                  
                                                                                
VOPT20   LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT10                                                        
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
PREP     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   BSMPERH+5,0                                                      
         BNE   PREP030                                                          
         MVC   TIQPSTR,TGTODAY1    TODAY'S DATE PWOS                            
         B     PREP050                                                          
                                                                                
PREP030  LA    R2,BSMPERH          VALIDATE PERIOD                              
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
         MVC   TIQPSTR,PVALPSTA    SET PWOS DATES FOR SYSIO                     
                                                                                
         USING DLCBD,R5                                                         
PREP050  LA    R5,DLCB                                                          
         BAS   RE,INITDOWN         INITIALIZE DOWNLOAD                          
                                                                                
         USING TLINPD,R3           USE INVOICE BIL                              
         LA    R3,KEY                                                           
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINDCDQ    INVOICES BY BILLED DATE                      
         MVC   TLINDDTE,TIQPSTR    ONLY BY START DATE                           
                                                                                
         GOTO1 HIGH                                                             
         J     PREP200                                                          
PREP100  GOTO1 SEQ                                                              
PREP200  CLI   TLINPCD,TLINDCDQ    HAS TO BE INVOICE BY BILL DATE               
         JNE   PREP250                                                          
         CLC   TLINDDTE,TIQPSTR                                                 
         JNE   PREP250                                                          
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   PREP100                                                          
         TM    TAINSTA2,TAINSHLR+TAINSHLP    PUR RELEASED AND PRINTED           
         BO    PREP100                       SKIP                               
                                                                                
         MVC   AGYCODE,TLINDAGY    SAVE AGY ID                                  
         MVI   TLINDINV+5,0        CREDITS SHOW UP REGULAR                      
         GOTO1 TINVCON,DMCB,TLINDINV,INVCODE,DATCON                             
         GOTO1 SORTER,DMCB,=C'PUT',AGYCODE      WRITE OUT SORT RECORD           
         J     PREP100                                                          
*--------------------------------------------------------------------           
PREP250  LA    R3,KEY                                                           
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINBCDQ    BILLING OPEN ITEMS                           
                                                                                
         GOTO1 HIGH                                                             
         J     PREP400                                                          
PREP300  GOTO1 SEQ                                                              
PREP400  CLI   TLINPCD,TLINBCDQ    HAS TO BE BILLING OPEN ITEMS                 
         JNE   PREP500                                                          
         TM    TLINBST2,TAINSHLD   HAS TO BE COD HOLD                           
         BZ    PREP300             NO, SKIP TO NEXT ONE                         
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   PREP300                                                          
         CLC   TAINHDTE,TIQPSTR    HLD (PUR) - PRINT DATE                       
         BNE   PREP300                                                          
                                                                                
         MVC   AGYCODE,TLINBAGY    SAVE AGY ID                                  
         MVC   WORK,TLINBINV                                                    
         XC    WORK(6),=X'FFFFFFFFFFFF'                                         
         GOTO1 TINVCON,DMCB,WORK,INVCODE,DATCON                                 
         GOTO1 SORTER,DMCB,=C'PUT',AGYCODE      WRITE OUT SORT RECORD           
         J     PREP300                                                          
                                                                                
PREP500  GOTO1 SORTER,DMCB,=C'GET',AGYCODE                                      
         ICM   RF,15,4(R1)                                                      
         BZ    PREP900                                                          
         MVC   AGYCODE(12),0(RF)                                                
                                                                                
PREP700  GOTO1 OUTPDOWN,DMCB,(C'T',AGYCODE),L'AGYCODE                           
         GOTO1 OUTPDOWN,DMCB,(C'T',INVCODE),L'INVCODE                           
         BAS   RE,EOLDOWN                                                       
         J     PREP500                                                          
                                                                                
PREP900  BRAS  RE,ENDDOWN          FINISH DOWNLOADABLE REPORT                   
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
INITDOWN NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
                                                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
AGYHEAD  DC    C'AGENCY'                                                        
COMHEAD  DC    C'COMMERCIAL'                                                    
VERHEAD  DC    C'VERSION'                                                       
NAMEHEAD DC    C'COMMERCIAL NAME'                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
DYNALLOC DS    A                                                                
AMASTD   DS    A                   A(MASTER)                                    
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
VSMTP    DS    V                   V(SMTP)                                      
                                                                                
FILTCAT  DS    CL3                 FILTER ON CATEGORY                           
SAVEKEY  DS    CL(L'TLCAKEY)                                                    
                                                                                
AGYCODE  DS    CL(L'TLCOAGY)       AGENCY CODE                                  
INVCODE  DS    CL6                 INVOICE CODE                                 
                                                                                
PROSTAT  DS    X                                                                
PSTRACE  EQU   X'80'                                                            
MYDLNQ   EQU   *-MYD                                                            
                                                                                
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPCCD                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP66   07/21/11'                                      
         END                                                                    
