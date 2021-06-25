*          DATA SET ACPRO67    AT LEVEL 029 AS OF 08/22/02                      
*PHASE T60B67A,*                                                                
         TITLE 'T60B67 - AUTHORIZTION HISTORY'                                  
T60B67   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B67**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         LA    R5,BUFF             SAVED AREA FOR TABLE                         
         USING BLOCKSD,R5                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,DISPKEY                                                     
         BNE   MODE4                                                            
         BAS   RE,DKEY                                                          
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE4    CLI   MODE,DISPREC                                                     
         BNE   XIT                                                              
         BAS   RE,DREC                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              VALIDATE KEY FIELDS                                              
***********************************************************************         
*                                                                               
VKEY     NTR1                                                                   
         XC    SVFLDS,SVFLDS       CLEAR KEY SAVE AREA                          
         LA    R2,HISOGRH          OFFICE GROUP                                 
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         GOTO1 VALOG                                                            
         MVC   SVOGR,EFFOFG                                                     
*                                                                               
VKEY2    LA    R2,HISOFCH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VKEY4                                                            
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   HISOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALOFF                                                           
         MVC   SVOFC,EFFOFFC                                                    
*                                                                               
VKEY4    LA    R2,HISCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VKEY6                                                            
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   HISOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   HISOFCH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALCLI                                                           
         MVC   SVCLI,CLICODE                                                    
*                                                                               
VKEY6    LA    R2,HISPROH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VKEY8                                                            
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   HISCLIH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALPROD                                                          
         MVC   SVPRO,PRODCODE                                                   
*                                                                               
VKEY8    LA    R2,HISMGRH          MEDIA GROUP                                  
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         GOTO1 VALMG                                                            
         MVC   SVMGR,MGROUP                                                     
*                                                                               
VKEY10   LA    R2,HISMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    VKEY12                                                           
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   HISMGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALMED                                                           
         MVC   SVMED,MEDIA                                                      
*                                                                               
VKEY12   LA    R2,HISNUMH          THIS IS THE ONLY REQUIRED FIELD              
         GOTO1 ANY                                                              
         MVC   SVAUTH,WORK                                                      
*                                                                               
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   AUTKEY,SPACES                                                    
         MVI   AUTKTYP,AUTKTYPQ                                                 
         MVI   AUTKSUB,AUTKSUBQ                                                 
         MVC   AUTKCPY(3),CUL                                                   
         MVC   AUTKOGR,SVOGR                                                    
         MVC   AUTKOFC,SVOFC                                                    
         MVC   AUTKCLI,SVCLI                                                    
         MVC   AUTKPRO,SVPRO                                                    
         MVC   AUTKMGR,SVMGR                                                    
         MVC   AUTKMED,SVMED                                                    
         MVC   AUTKNUM,SVAUTH                                                   
*                                                                               
         CLC   SAVEKEY(L'ACTKEY),KEY                                            
         BE    VKEYX                                                            
         MVC   STDISP,=H'0'                                                     
         MVC   PRVSTDSP,=H'0'                                                   
         BAS   RE,CLRSCRN                                                       
*                                                                               
VKEYX    MVC   SAVEKEY,KEY                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DISPLAY KEY                                                      
***********************************************************************         
*                                                                               
         USING AUTRECD,R6                                                       
DKEY     NTR1                                                                   
         L     R6,AIO                                                           
         LA    R2,HISOGRH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKOGR),AUTKOGR                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,HISOFCH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKOFC),AUTKOFC                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,HISCLIH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKCLI),AUTKCLI                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,HISPROH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKPRO),AUTKPRO                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,HISMGRH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKMGR),AUTKMGR                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,HISMEDH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKMED),AUTKMED                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,HISNUMH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKNUM),AUTKNUM                                           
         BAS   RE,MOVEFLD                                                       
         B     XIT                                                              
*                                                                               
MOVEFLD  NTR1                                                                   
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD                                                      
*                                                                               
         LA    R3,8(R2,R1)         GET LAST BYTE OF FIELD                       
         LA    RF,1(R1)            GET LENGTH OF FIELD                          
         CLI   0(R3),C' '          LOOK FOR SIGNIFICANT DATA                    
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   RF,*-10                                                          
         OI    6(R2),X'80'                                                      
         STC   RF,5(R2)                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DISPLAY RECORD                                                   
***********************************************************************         
*                                                                               
DREC     NTR1                                                                   
         GOTO1 PERSOUT             DATE OF LAST ACTIVITY                        
         LA    R2,HISLACTH                                                      
         MVC   8(20,R2),WORK+20                                                 
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
*                                                                               
         LA    R2,HISAUTDH         PRINT THE DESCRIPTION                        
         GOTO1 NAMEOUT                                                          
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
*                                                                               
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
         BAS   RE,BLDTAB                                                        
*                                                                               
         USING ELEMTABD,R4                                                      
         USING DSPLINED,R2                                                      
         LA    R4,ELEMBLK                                                       
         LA    R2,HISDATH                                                       
*                                                                               
         CLI   PFKEY,0                                                          
         BNE   DR32                                                             
         CLI   ACTEQU,ACTSEL                                                    
         BE    DR37                                                             
         B     DR38                                                             
*                                                                               
DR32     CLI   PFKEY,7             UP                                           
         BNE   DR34                                                             
         MVC   STDISP,PRVSTDSP                                                  
         LA    R1,ELLNQ                                                         
         MH    R1,=H'8'            8 LINES                                      
         LH    R0,PRVSTDSP                                                      
         SR    R0,R1                                                            
         CH    R0,=H'0'                                                         
         BNL   *+6                 DISP FROM TOP                                
         SR    R0,R0                                                            
         STH   R0,PRVSTDSP                                                      
         B     DR38                                                             
*                                                                               
DR34     CLI   PFKEY,8             DOWN                                         
         BNE   DR38                                                             
         MVC   PRVSTDSP,STDISP                                                  
         MVC   STDISP,DLINE1                                                    
         B     DR38                                                             
*                                                                               
DR37     MVC   STDISP,=H'0'        DEFAULT TO BEGINNING                         
         MVC   PRVSTDSP,=H'0'                                                   
*                                                                               
DR38     LA    R0,ELLNQ            LENGTH OF ONE ENTRY                          
         MH    R0,TABCOUNT         NUMBER OF ENTRIES                            
         LH    R1,STDISP                                                        
         CR    R0,R1                                                            
         BH    DR39                                                             
         LA    R1,0                                                             
         STH   R1,STDISP                                                        
DR39     AR    R4,R1                                                            
*                                                                               
DR40NX   LA    R1,ELEMBLK                                                       
         LR    R0,R4               R4 POINTS TO ELEMENT BEING DISPLAYED         
         SR    R0,R1                                                            
         STH   R0,DLINE1           SAVE DISPLACEMENT INTO TABLE                 
*                                                                               
         OC    0(ELLNQ,R4),0(R4)        ANY MORE ENTRIES?                       
         BNZ   DR42                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL     GET NEXT SELECTION                   
         LA    R0,0                START FROM TOP NEXT ENTER                    
         STH   R0,DLINE1                                                        
         B     DR100                                                            
*                                                                               
DR42     DS    0H                                                               
         MVC   DSPDATE,ELDATE                                                   
         OI    DSPDATEH+6,X'80'     XMIT                                        
*                                                                               
         MVC   DSPPER,ELPER                                                     
         OI    DSPPERH+6,X'80'     XMIT                                         
*                                                                               
         CURED (P6,ELAMT),(12,DSPAMT),2,ALIGN=RIGHT                             
         OI    DSPAMTH+6,X'80'     XMIT                                         
*                                                                               
         LA    R2,DSPLNQ(R2)       NEXT SCREEN LINE                             
         LA    R1,HISENDHH         END OF LIST                                  
         CR    R2,R1                                                            
         BH    DR100               DISP TOTALS                                  
         LA    R4,ELLNQ(R4)        NEXT TABLE ENTRY                             
         B     DR40NX                                                           
*                                                                               
* DISPLAY TOTAL                                                                 
*                                                                               
DR100    DS    0H                                                               
         LA    R3,SVLAMT                                                        
         LA    R6,HISAMTH                                                       
         CURED (P6,SVLAMT),(12,8(R6)),2,ALIGN=RIGHT                             
         OI    6(R6),X'80'                                                      
         OI    1(R6),X'20'                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CLEAR SOME FIELDS                                                      
***********************************************************************         
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,HISDATH          CLEAR ALL FIELDS                             
         LA    R3,HISENDHH                                                      
*                                                                               
CSCLR    DS    0H                                                               
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         MVI   5(R2),X'00'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BNH   CSCLR               NO                                           
*                                                                               
CSCLRX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*    CREATE ELEMENTS TABLE FOR SCROLLING                                        
***********************************************************************         
*                                                                               
BLDTAB   NTR1                                                                   
         USING ELEMTABD,R4                                                      
         LA    R4,ELEMBLK                                                       
         LR    R0,R4               CLEAR BLOCK FOR TABLE                        
         LH    R1,=Y(ELEMLNQ)                                                   
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ZAP   SVLAMT,=P'0'                                                     
         XC    TABCOUNT,TABCOUNT   COUNT OF TABLE ENTRIES                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACTKEY),KEYSAVE                                            
         BNE   BLDX                                                             
*                                                                               
         L     R6,AIO                                                           
         USING AUTHELD,R6                                                       
         MVI   ELCODE,AUTHELQ      GET DATA ELEMENT                             
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
BLD02    BAS   RE,NEXTEL                                                        
         BNE   BLDX                                                             
         MVC   ELDATE,AUTHDATE     DATE                                         
         MVC   ELTIME,AUTHTIME     TIME                                         
         MVC   ELPER,AUTHPER       PERSON                                       
         MVC   ELAMT,AUTHAMT       AMOUNT                                       
         ZAP   SVLAMT,ELAMT                                                     
*                                                                               
         LH    R1,TABCOUNT         INCREMENT TABLE COUNTER                      
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
*                                                                               
         MVI   ERROR,REC2BIG                                                    
         CLC   TABCOUNT,=Y(MAXCOUNT)  END OF TABLE REACHED?                     
         BH    *+12                                                             
         LA    R4,ELLNQ(R4)                                                     
         B     BLD02                                                            
         LA    R2,HISOGRH                                                       
         B     ERREXIT                                                          
*                                                                               
BLDX     B     XIT                                                              
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
ERREXIT  GOTO1 VERRCUR                                                          
         EJECT                                                                  
*DDSPOOLD                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT  OFF                                                             
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT  OFF                                                             
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROBBD                                                       
         ORG   T60BFFD+2304                                                     
SVFLDS   DS    0CL(SVFLDLQ)                                                     
SVOGR    DS    CL1                 OFFICE GROUP                                 
SVOFC    DS    CL2                 OFFICE                                       
SVCLI    DS    CL6                 CLIENT                                       
SVPRO    DS    CL6                 PRODUCT                                      
SVMGR    DS    CL1                 MEDIA GROUP                                  
SVMED    DS    CL1                 MEDIA                                        
SVAUTH   DS    CL19                AUTHORIZATION NUMBER                         
SVAAMT   DS    PL6                 AUTHORIZATION AMOUNT                         
SVFLDLQ  EQU   *-SVOGR                                                          
*                                                                               
STDISP   DS    H                                                                
PRVSTDSP DS    H                                                                
DLINE1   DS    H                                                                
TABCOUNT DS    H                                                                
SVLAMT   DS    PL6                 LAST AUTHORIZED AMOUNT                       
FLDH     DS    CL8                                                              
FLD      DS    CL80                                                             
SAVEKEY  DS    XL42                                                             
MAXCOUNT EQU   50                  MAX # OF TABLE ENTRIES                       
         EJECT                                                                  
BLOCKSD  DSECT                                                                  
ELEMBLK  DS    CL(ELLNQ*MAXCOUNT)                                               
ELEMLNQ  EQU   *-ELEMBLK                                                        
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* DSECT TO COVER ELEMENT TABLE                                                  
*                                                                               
ELEMTABD DSECT                                                                  
ELDATE   DS    CL8                 DATE                                         
ELTIME   DS    PL4                 TIME                                         
ELPER    DS    CL8                 PERSON                                       
ELAMT    DS    PL6                 AMOUNT                                       
ELLNQ    EQU   *-ELEMTABD                                                       
*                                                                               
* DSECT TO COVER DISPLAY LINE                                                   
*                                                                               
DSPLINED DSECT                                                                  
DSPDATEH DS    CL8                                                              
DSPDATE  DS    CL8                 DATE                                         
DSPPERH  DS    CL8                                                              
DSPPER   DS    CL8                 PERSON                                       
DSPAMTH  DS    CL8                                                              
DSPAMT   DS    CL12                AMOUNT                                       
DSPLNQ   EQU   *-DSPLINED                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029ACPRO67   08/22/02'                                      
         END                                                                    
