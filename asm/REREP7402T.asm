*          DATA SET REREP7402T AT LEVEL 104 AS OF 05/01/02                      
*PHASE RE7402A,+0                                                               
*INCLUDE CLPACK                                                                 
*INCLUDE MEDGET                                                                 
***********************************************************************         
*    HISTORY:                                                         *         
*                                                                     *         
* 07FEB/92 (SKU) --- NEW DETAIL REPORT                                *         
*                                                                     *         
* 25FEB/94 (SKU) --- INCREASE IOAREA TO 2000 FOR SPOT RECORDS         *         
*                                                                     *         
* DEC14/95 (BG ) 103 CHANGE REGENALL TO REGENALL1 2K CON            * *         
*                                                                     *         
*                        ** END TOMBSTONE **                          *         
***********************************************************************         
         TITLE 'PRODUCT LISTING PROGRAM'                                        
RE7402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STOREX-STORED,**RE7402,R7                                        
         USING STORED,RC                                                        
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
         EJECT                                                                  
***********************************************************************         
*              CHECK MODE SETTINGS                                              
***********************************************************************         
         B     START                                                            
         ANSR                      YES, NO, OR XIT                              
*                                                                               
START    CLI   MODE,REQFRST                                                     
         BNE   PR2                                                              
         MVC   PAGE,=H'1'                                                       
         B     PREXT                                                            
*                                                                               
PR2      CLI   QOPTION1,C'Y'       DETAIL REPORT??                              
         BE    DETAIL                                                           
*                                                                               
         CLI   MODE,ADVLAST                                                     
         BNE   PR4                                                              
         BAS   RE,PRINTEM                                                       
         B     PREXT                                                            
         SPACE 2                                                                
PR4      CLI   MODE,PROCPROD                                                    
         BNE   PREXT                                                            
         MVC   MYWORK2,SPACES                                                   
         MVC   MYWORK2+1(3),RPRDKPRD                                            
         MVC   MYWORK2+6(20),RPRDNAME                                           
         MVC   MYWORK2+27(2),RPRDCLSS                                           
         MVC   MYWORK2+30(2),RPRDCATG                                           
         BAS   RE,POSTEM                                                        
*                                                                               
PREXT    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO POST TO TABLE                                         
***********************************************************************         
POSTEM   NTR1                                                                   
*                                                                               
POST2    LA    R2,PAGETAB                                                       
         LA    R3,90                                                            
         SPACE 2                                                                
POST4    CLI   0(R2),0                                                          
         BE    POST6                                                            
         LA    R2,37(R2)                                                        
         BCT   R3,POST4                                                         
         BAS   RE,PRINTEM                                                       
         B     POST2                                                            
         SPACE 2                                                                
POST6    MVC   0(37,R2),MYWORK2                                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO PRINT FROM TABLE                                      
***********************************************************************         
PRINTEM  NTR1                                                                   
         MVI   RCSUBPRG,1                                                       
         LA    R2,PAGETAB                                                       
         LA    R3,30                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   HEAD4+12(4),RPRDKADV                                             
         MVC   HEAD4+17(20),RADVNAME                                            
         LA    R6,37                                                            
         LR    R8,R6                                                            
         MH    R8,=H'30'                                                        
         SPACE 2                                                                
PRINT2   LA    R4,0(R2,R8)                                                      
         LA    R5,0(R4,R8)                                                      
         MVC   P(37),0(R2)                                                      
         MVC   P+37(37),0(R4)                                                   
         MVC   P+74(37),0(R5)                                                   
         GOTO1 REPORT                                                           
         XC    0(37,R2),0(R2)                                                   
         XC    0(37,R4),0(R4)                                                   
         XC    0(37,R5),0(R5)                                                   
         AR    R2,R6                                                            
         BCT   R3,PRINT2                                                        
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DETAIL REPORT FOR PRODUCT RECORD                                              
*                                                                               
* MUST NOT DISTURB KEY W/O SAVING IT!! MUST REESTABLISH SEQ ORDER!!             
*                                                                               
**********************************************************************          
DETAIL   DS    0H                                                               
         MVI   RCSUBPRG,3                                                       
*                                                                               
         CLI   MODE,ADVLAST                                                     
         BNE   DET10                                                            
         BAS   RE,DPRINTEM                                                      
         B     PREXT                                                            
*                                                                               
DET10    CLI   MODE,PROCPROD                                                    
         BNE   PREXT                                                            
         XC    MYWORK,MYWORK       SETUP BLOCK OF RECORD DATA                   
         LA    R4,MYWORK                                                        
         USING PDETAIL,R4                                                       
         MVC   PRDCODE(L'PRDCODE),RPRDKPRD                                      
         MVC   PRDNAME(L'PRDNAME),RPRDNAME                                      
         MVC   CATCODE(L'CATCODE),RPRDCATG                                      
         MVC   NETCON#(L'NETCON#),RPRDNET#                                      
*                                                                               
         LA    R6,RPRDREC          NETWORK CONTRACT ELEMENT                     
         USING RPRDNELM,R6                                                      
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   DET20                                                            
         MVC   PDESC(L'PDESC),RPRDNDES                                          
         MVC   PPCODE(L'PPCODE),RPRDNPNT                                        
         DROP  R6                                                               
*                                                                               
DET20    LA    R6,RPRDREC          SPOTPAK INTERFACE ELEMENT                    
         USING RPRDSPOT,R6                                                      
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BNE   DET30                                                            
         MVC   SCLTCDE(L'SCLTCDE),RPRDSPCL                                      
         MVC   SPCODES(L'SPCODES),RPRDSPP1                                      
         EDIT  (1,RPRDSPS1),(3,SPSPL1),ZERO=BLANK,ALIGN=LEFT                    
         EDIT  (1,RPRDSPS2),(3,SPSPL2),ZERO=BLANK,ALIGN=LEFT                    
         EDIT  (1,RPRDSPES),(3,SESTCDE),ZERO=NOBLANK,ALIGN=LEFT                 
         DROP  R6                                                               
*                                                                               
DET30    DS    0H                  GET CATEGORY CODE AND NAME                   
         XC    RCTGKEY,RCTGKEY                                                  
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,RPRDKREP                                                
         MVC   RCTGKCTG,RPRDCATG                                                
         MVC   KEYSAVE,RCTGKEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,RCTGKEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   RCTGKEY(L'RCTGKEY),KEYSAVE                                       
         BNE   DET40                                                            
*                                                                               
         GOTO1 (RF),(R1),GETREC,REPFILE,RCTGKEY+28,RCTGREC,DMWORK               
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   CATNAME(L'CATNAME),RCTGNAME                                      
*                                                                               
DET40    DS    0H                  GET POINT PERSON CODE AND NAME               
         XC    RPTPKEY,RPTPKEY                                                  
         MVI   RPTPKTYP,RPTPKTYQ                                                
         MVC   RPTPKREP,RPRDKREP                                                
         MVC   RPTPKREC,RPRDNPNT                                                
         MVC   KEYSAVE,RPTPKEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,RPTPKEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   RPTPKEY(L'RPTPKEY),KEYSAVE                                       
         BNE   DET50                                                            
*                                                                               
         GOTO1 (RF),(R1),GETREC,REPFILE,RPTPKEY+28,RPTPREC,DMWORK               
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   PPNAME(L'PPNAME),RPTPNAME                                        
*                                                                               
DET50    DS    0H                  PREPARE FOR SPOT DATAMGR CALLS               
         LA    R6,RPRDREC                                                       
         USING RPRDSPOT,R6                                                      
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BNE   DET200                                                           
*                                                                               
         L     RE,ADCONLST         SAVE REP UTL SYS                             
         USING ADCONSD,RE                                                       
         L     RE,VUTL                                                          
         DROP  RE                                                               
         MVC   REPSYS#(1),4(RE)                                                 
*                                                                               
* SWITCH TO SPOT AND DO ALL SPOT DATAMGR CALLS HERE                             
*                                                                               
         GOTO1 GETREP              GET SPOTPAK INTERFACE DATA                   
         BNZ   DET100                                                           
         GOTO1 SPOTCLI,DMCB,RPRDSPCL,SNAME                                      
         BNZ   DET100                                                           
         GOTO1 SPOTPRD,DMCB,RPRDSPP1,SNAMES                                     
         BNZ   DET100                                                           
         GOTO1 SPOTEST,DMCB,RPRDSPP1,RPRDSPES,SDESC                             
         BNZ   DET100                                                           
         DROP  R6                                                               
*                                                                               
DET100   DS    0H                  DONE, SWITCH BACK TO REP                     
         L     RE,ADCONLST                                                      
         USING ADCONSD,RE                                                       
         L     RE,VUTL                                                          
         DROP  RE                                                               
         MVC   4(1,RE),REPSYS#                                                  
*                                                                               
DET200   DS    0H                  REESTABLISH SEQ ORDER FOR PRODUCTS           
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DPOSTEM                                                       
         B     PREXT                                                            
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO POST TO TABLE                                         
**********************************************************************          
DPOSTEM  NTR1                                                                   
*                                                                               
DPOST2   LA    R2,PAGETAB                                                       
         LA    R3,NUMREC                                                        
         SPACE 2                                                                
DPOST4   CLI   0(R2),0                                                          
         BE    DPOST6                                                           
         LA    R2,LPDETAIL(R2)                                                  
         BCT   R3,DPOST4                                                        
         BAS   RE,DPRINTEM                                                      
         B     DPOST2                                                           
         SPACE 2                                                                
DPOST6   MVC   0(LPDETAIL,R2),MYWORK                                            
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO PRINT FROM TABLE                                      
**********************************************************************          
DPRINTEM NTR1                                                                   
         LA    R2,PAGETAB                                                       
         USING PDETAIL,R2                                                       
         LA    R3,NUMREC                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   HEAD4+12(4),RPRDKADV                                             
         MVC   HEAD4+17(20),RADVNAME                                            
         GOTO1 REPORT                                                           
*                                                                               
DPRINT2  DS    0H                  PRINTS RECORD IN TWO COLUMNS                 
         MVC   P+2(15),=C'REPPAK PRD CODE'                                      
         MVC   P+19(L'PRDCODE),PRDCODE                                          
         MVC   P+70(16),=C'SPOTPAK CLT CODE'                                    
         MVC   P+88(L'SCLTCDE),SCLTCDE                                          
         GOTO1 REPORT                                                           
         MVC   P+2(12),=C'PRODUCT NAME'                                         
         MVC   P+16(L'PRDNAME),PRDNAME                                          
         MVC   P+70(4),=C'NAME'                                                 
         MVC   P+76(L'SNAME),SNAME                                              
         GOTO1 REPORT                                                           
         MVC   P+2(13),=C'CATEGORY CODE'                                        
         MVC   P+17(L'CATCODE),CATCODE                                          
         MVC   P+70(15),=C'PRODUCT CODE(S)'                                     
         MVC   P+87(L'SPCODES),SPCODES                                          
         GOTO1 REPORT                                                           
         MVC   P+2(13),=C'CATEGORY NAME'                                        
         MVC   P+17(L'CATNAME),CATNAME                                          
         MVC   P+70(7),=C'NAME(S)'                                              
         MVC   P+79(L'SNAMES),SNAMES                                            
         GOTO1 REPORT                                                           
         MVC   P+2(12),=C'NETWORK CON#'                                         
         MVC   P+16(L'NETCON#),NETCON#                                          
         MVC   P+70(14),=C'PRODUCT SPLIT1'                                      
         MVC   P+86(L'SPSPL1),SPSPL1                                            
         GOTO1 REPORT                                                           
         MVC   P+2(11),=C'DESCRIPTION'                                          
         MVC   P+15(L'PDESC),PDESC                                              
         MVC   P+70(14),=C'PRODUCT SPLIT2'                                      
         MVC   P+86(L'SPSPL2),SPSPL2                                            
         GOTO1 REPORT                                                           
         MVC   P+2(17),=C'POINT PERSON CODE'                                    
         MVC   P+21(L'PPCODE),PPCODE                                            
         MVC   P+70(13),=C'ESTIMATE CODE'                                       
         MVC   P+85(L'SESTCDE),SESTCDE                                          
         GOTO1 REPORT                                                           
         MVC   P+2(17),=C'POINT PERSON NAME'                                    
         MVC   P+21(L'PPNAME),PPNAME                                            
         MVC   P+70(11),=C'DESCRIPTION'                                         
         MVC   P+83(L'SDESC),SDESC                                              
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XC    0(LPDETAIL,R2),0(R2)                                             
         LA    R2,LPDETAIL(R2)                                                  
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         BCT   R3,DPRINT2          5 RECORDS PER PAGE                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        GETREP --- GET THE REP AND SPOTPAK INTERFACE INFO                      
*                                                                               
***********************************************************************         
GETREP   NTR1                                                                   
*                                                                               
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,RPRDKREP                                                
         MVC   KEYSAVE,RREPKEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,RREPKEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   RREPKEY(L'RREPKEY),KEYSAVE                                       
         BNE   NO                                                               
         GOTO1 (RF),(R1),GETREC,REPFILE,RREPKEY+28,RREPREC,DMWORK,0             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,RREPREC                                                       
         USING RREPSPOT,R6                                                      
         MVI   ELCODE,5                                                         
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
*                                                                               
         GOTO1 SPOTCT,DMCB,2(R6)   GET SPOTPAK INFO                             
         BNZ   NO                                                               
*                                                                               
         L     R3,ADCONLST         OPEN SPOT FILES                              
         USING ADCONSD,R3                                                       
         L     R3,VUTL                                                          
         DROP  R3                                                               
         MVC   4(1,R3),SPOTSYS#                                                 
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT   ',A(FLIST),IOAREA              
*                                                                               
         GOTO1 SPOTMED,DMCB,8(R6),2(R6),MYWORK2                                 
         BNZ   NO                                                               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        SPOTCT --- LOOK AT THE CONTROL FILE FOR SPOTPAK INFO                   
*                                                                               
*        P1      =   A(SPOTPAK POWER CODE)                                      
*                                                                               
***********************************************************************         
SPOTCT   NTR1                                                                   
*                                                                               
         L     R2,0(R1)            GET ADDR OF INPUT CLI CODE                   
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',=C'NCTFILE X',IOAREA         
         LA    R5,IOAREA                                                        
         XC    0(25,R5),0(R5)                                                   
         MVI   0(R5),C'5'          SYSTEM ACCESS RECORD                         
         MVC   23(2,R5),0(R2)                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R5),(R5),0                   
         CLI   DMCB+8,0                                                         
         BNE   NO                                                               
*                                                                               
         LA    R5,28(R5)           POINT TO THE START OF REC                    
*                                                                               
SPCT10   CLI   0(R5),0             NOT CLEAR FOR SPOT                           
         BE    NO                                                               
         CLI   0(R5),X'21'         SYSTEM ELEMENT?                              
         BNE   SPCT30                                                           
         CLI   2(R5),X'02'         SPOT??                                       
         BE    SPCT50                                                           
*                                                                               
SPCT30   DS    0H                                                               
         ZIC   RF,1(R5)            KEEP LOOKING FOR SPOT                        
         AR    R5,RF                                                            
         B     SPCT10                                                           
*                                                                               
SPCT50   DS    0H                                                               
         MVC   SPOTSYS#(1),3(R5)   FACPAK SYSTEM NUMBER                         
         MVC   SPOTAGY#(1),4(R5)   SPOTPAK AGY NUMBER                           
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        SPOTMED --- VALIDATE AND EXPAND SPOTPAK MEDIA CODE                     
*                                                                               
*        P1      =   A(SPOTPAK MEDIA CODE)                                      
*        P2      =   A(SPOTPAK AGENCY CODE)                                     
*        P3      =   A(40 BYTE WORK AREA)                                       
*                      ON RETURN, BYTE 1 = SPOTPAK AGY/MEDIA CODE               
*                                 BYTES 2-11 = MEDIA NAME                       
*                                                                               
***********************************************************************         
SPOTMED  NTR1                                                                   
*                                                                               
         LM    R2,R4,0(R1)         GET PARAMS                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         ST    R3,DMCB             LOAD A(AGENCY CODE)                          
         MVC   DMCB(1),0(R2)       LOAD (MEDIA CODE)                            
         MVC   DMCB+4(4),DATAMGR                                                
         ST    R4,DMCB+8           LOAD A(WORK AREA)                            
         GOTO1 =V(MEDGET),DMCB,RR=Y                                             
         CLI   8(R1),X'FF'                                                      
         BE    NO                                                               
SMED150  EQU   *                                                                
         MVC   SPOTMED#(1),0(R4)                                                
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        SPOTCLI --- VALIDATE AND EXPAND SPOTPAK CLIENT                         
*                                                                               
*        P1      =   A(INPUT CLIENT CODE)                                       
*        P2      =   A(OUTPUT CLIENT NAME, ZERO = NO EXPAND)                    
*                                                                               
***********************************************************************         
SPOTCLI  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                  GET ADDR OF INPUT CLI CODE             
         L     R3,4(R1)                  GET ADDR OF CLIENT NAME                
*                                                                               
         GOTO1 =V(CLPACK),DMCB,0(R2),SPOTCLI#                                   
         CLI   DMCB,0                                                           
         BNE   NO                                                               
*                                                                               
         XC    SPTKEY,SPTKEY                                                    
         LA    R4,SPTKEY                                                        
         USING CKEY,R4                                                          
         MVC   CKEYAM,SPOTMED#                                                  
         MVC   CKEYCLT,SPOTCLI#                                                 
         DROP  R4                                                               
         BAS   RE,SPOTHIGH                                                      
         CLC   SPTKEY(13),KEYSAVE                                               
         BNE   NO                                                               
*                                                                               
         BAS   RE,SPOTGET                                                       
         LA    R4,IOAREA                                                        
         USING SPCLTD,R4                                                        
         MVC   0(20,R3),CNAME                                                   
         DROP  R4                                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        SPOTPRD --- VALIDATE AND EXPAND SPOTPAK PRODUCT                        
*                                                                               
*        P1      =   A(INPUT PRODUCT CODE)                                      
*        P2      =   A(OUTPUT PRODUCT NAME, ZERO = NO EXPAND)                   
*                                                                               
***********************************************************************         
SPOTPRD  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                  GET ADDR OF INPUT PRD CODE             
         L     R3,4(R1)                  GET ADDR OF PRODUCT NAME               
*                                                                               
         XC    SPTKEY,SPTKEY                                                    
         LA    R4,SPTKEY                                                        
         USING PKEY,R4                                                          
         MVC   PKEYAM,SPOTMED#                                                  
         MVC   PKEYCLT,SPOTCLI#                                                 
         MVC   PKEYPRD,0(R2)                                                    
         DROP  R4                                                               
         BAS   RE,SPOTHIGH                                                      
         CLC   SPTKEY(13),KEYSAVE                                               
         BNE   NO                                                               
*                                                                               
         BAS   RE,SPOTGET                                                       
         LA    R4,IOAREA                                                        
         USING SPPRDD,R4                                                        
         MVC   0(20,R3),PNAME                                                   
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        SPOTEST --- VALIDATE (FILE) AND EXPAND SPOTPAK ESTIMATE                
*                                                                               
*        P1      =   A(INPUT PRODUCT CODE)                                      
*        P2      =   A(INPUT ESTIMATE CODE (BINARY))                            
*        P3      =   A(OUTPUT ESTIMATE NAME, ZERO = NO EXPAND)                  
*                                                                               
***********************************************************************         
SPOTEST  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                  GET ADDR OF INPUT PRD CODE             
         L     R3,4(R1)                  GET ADDR OF INPUT EST CODE             
         L     R4,8(R1)                  GET ADDR OF ESTIMATE NAME              
*                                                                               
         XC    SPTKEY,SPTKEY                                                    
         LA    R5,SPTKEY                                                        
         USING EKEY,R5                                                          
         MVC   EKEYAM,SPOTMED#                                                  
         MVC   EKEYCLT,SPOTCLI#                                                 
         MVC   EKEYPRD,0(R2)                                                    
         MVC   EKEYEST,0(R3)                                                    
         DROP  R5                                                               
         BAS   RE,SPOTHIGH                                                      
         CLC   SPTKEY(13),KEYSAVE                                               
         BNE   NO                                                               
*                                                                               
         BAS   RE,SPOTGET                                                       
         LA    R5,IOAREA                                                        
         USING SPESTD,R5                                                        
         MVC   0(20,R4),EDESC                                                   
         B     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        SPOT DATAMGR CALLS                                                     
*                                                                               
***********************************************************************         
SPOTHIGH NTR1                                                                   
         MVC   KEYSAVE,SPTKEY                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'SPTDIR  ',KEYSAVE,SPTKEY,0                
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
SPOTGET  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,=C'SPTFIL  ',SPTKEY+14,IOAREA,      X        
               DMWORK,0                                                         
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8'NSPTDIR '                                                    
         DC    CL8'NSPTFIL '                                                    
         DC    CL8'X       '                                                    
         DS    F                                                                
PAGETAB  DC    4000X'00'                                                        
NUMREC   EQU   5                                                                
*                                                                               
STORED   DSECT                                                                  
ELCODE   DS    C                                                                
SPTKEY   DS    CL32                                                             
MYWORK   DS    CL(LPDETAIL)                                                     
MYWORK2  DS    CL40                                                             
REPSYS#  DS    X                                                                
SPOTSYS# DS    X                                                                
SPOTAGY# DS    X                                                                
SPOTMED# DS    C                                                                
SPOTCLI# DS    CL2                                                              
IOAREA   DS    CL2000                                                           
STOREX   EQU   *                                                                
*                                                                               
         EJECT                                                                  
* DSECT FOR DETAIL REPORT                                                       
PDETAIL  DSECT                                                                  
PRDCODE  DS    CL3                                                              
PRDNAME  DS    CL20                                                             
CATCODE  DS    CL2                                                              
CATNAME  DS    CL30                                                             
NETCON#  DS    CL8                                                              
PDESC    DS    CL20                                                             
PPCODE   DS    CL3                                                              
PPNAME   DS    CL20                                                             
SCLTCDE  DS    CL3                                                              
SNAME    DS    CL20                                                             
SPCODES  DS    CL3                                                              
SNAMES   DS    CL20                                                             
SPSPL1   DS    CL3                                                              
SPSPL2   DS    CL3                                                              
SESTCDE  DS    CL3                                                              
SDESC    DS    CL20                                                             
LPDETAIL EQU   *-PDETAIL                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
SPCLTD   DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
SPPRDD   DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
SPESTD   DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104REREP7402T05/01/02'                                      
         END                                                                    
