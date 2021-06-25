*          DATA SET DDLOCKREP  AT LEVEL 031 AS OF 04/21/99                      
*PHASE LOCKREP                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'REPORT ON LOCKTAB FROM PARAMETER CARDS'                         
         PRINT NOGEN                                                            
MAIN     START                                                                  
         NBASE WORKL,LOCKREP*,=V(WORKAREA)                                      
         USING WORKD,RC                                                         
         USING PLINED,PLINE                                                     
         ST    RD,SAVERD                                                        
*                                                                               
         BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,PRINTT           PRINT TITLES                                 
         BAS   RE,CARDIN           READ AND VALIDATE INPUT CARDS                
         BL    MAINX               ERROR                                        
*                                                                               
         BAS   RE,GETSPC           BIND TO  REQUESTED DATASPACE                 
*                                                                               
         BAS   RE,WRTREP           WRITE REPORT                                 
         BAS   RE,PRINTX                                                        
*                                                                               
MAINX    B     XBASE                                                            
         EJECT                                                                  
***********************************************************************         
* GET EACH LINE FROM DATASPACE                                        *         
***********************************************************************         
         SPACE 1                                                                
WRTREP   NTR1  ,                                                                
         XR    R4,R4               TABLE IS HERE                                
         LAM   R4,R4,ALET                                                       
         CPYA  R3,R4                                                            
         SAC   512                                                              
*                                                                               
         ICM   R4,15,TABSLOX-FATABSD(R4)                                        
         BZ    WRTREPX             NO LOCK TABLE SET UP                         
         ICM   RF,15,4(R4)         RF=A(END OF TABLE)                           
         BZ    WRTREPX                                                          
*                                                                               
         ICM   R3,15,0(R4)         R3=A(NEXT ENTRY)                             
WRTR02   MVC   TLKNTRY,0(R3)                                                    
         SAC   0                                                                
         BAS   RE,PRTREP                                                        
         SAC   512                                                              
*                                                                               
         AHI   R3,TLKLEN                                                        
         C     R3,4(,R4)           SEE IF STILL BEFORE END                      
         BL    WRTR02                                                           
*                                                                               
         LA    R3,TLKLEN(R4)       R3=A(FIRST ENTRY)                            
WRTR04   C     R3,0(,R4)                                                        
         BNL   WRTREPX                                                          
*                                                                               
         MVC   TLKNTRY,0(R3)                                                    
         SAC   0                                                                
         BAS   RE,PRTREP                                                        
         SAC   512                                                              
*                                                                               
         AHI   R3,TLKLEN                                                        
         B     WRTR04                                                           
*                                                                               
WRTREPX  SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT SINGLE LINE IN TLKNTRY                                        *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   NTR1  ,                                                                
         ICM   RF,15,TLKTIME       LOCK ENTRY HAS A VALUE?                      
         BZ    PRTREPX             NO                                           
*                                                                               
         BAS   RE,TIMER            OUTPUT TIME                                  
         BAS   RE,ASIDOUT          OUTPUT ASID                                  
         BAS   RE,FACOUT           OUTPUT FACPAK/TASK                           
         BAS   RE,FACOUTO          OUTPUT OWNER FACPAK/TASK                     
         BAS   RE,TYPEOUT          OUTPUT CALL TYPE                             
         BAS   RE,TIMED            OUTPUT TIME LOCK HELD                        
         BAS   RE,TIMEX            OUTPUT OWNER LOCK INFO                       
*                                                                               
         MVC   PRDETL,TLKNTRY                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
PRTREPX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT TIME ONTO PRINT LINE                                         *         
***********************************************************************         
         SPACE 1                                                                
TIMER    NTR1  ,                                                                
         ICM   RE,15,TLKTIME                                                    
         BZ    EXITOK                                                           
*                                                                               
         MVI   PTIME+2,C':'                                                     
         MVI   PTIME+5,C':'                                                     
         MVI   PTIME+8,C':'                                                     
*                                                                               
         SRDL  RE,32                                                            
         LA    R0,100                                                           
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PTIME+09(2),DUB                                                  
*                                                                               
         XR    RE,RE                                                            
         LA    R0,60                                                            
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PTIME+06(2),DUB                                                  
*                                                                               
         XR    RE,RE                                                            
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PTIME+03(2),DUB                                                  
*                                                                               
         XR    RE,RE                                                            
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PTIME+00(2),DUB                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT HELD TIME                                                    *         
***********************************************************************         
         SPACE 1                                                                
TIMED    NTR1  ,                                                                
         ICM   RE,15,TLKDELT                                                    
         BZ    EXITOK                                                           
*                                                                               
         MVI   PDTIME+2,C':'                                                    
         MVI   PDTIME+5,C':'                                                    
         MVI   PDTIME+8,C':'                                                    
*                                                                               
         SRDL  RE,32                                                            
         LA    R0,100                                                           
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PDTIME+09(2),DUB                                                 
*                                                                               
         XR    RE,RE                                                            
         LA    R0,60                                                            
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PDTIME+06(2),DUB                                                 
*                                                                               
         XR    RE,RE                                                            
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PDTIME+03(2),DUB                                                 
*                                                                               
         XR    RE,RE                                                            
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PDTIME+00(2),DUB                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT OWNER TIME                                                   *         
***********************************************************************         
         SPACE 1                                                                
TIMEX    NTR1  ,                                                                
         ICM   RE,15,TLKOTIME                                                   
         BZ    EXITOK                                                           
*                                                                               
         MVI   POTIME+2,C':'                                                    
         MVI   POTIME+5,C':'                                                    
         MVI   POTIME+8,C':'                                                    
*                                                                               
         SRDL  RE,32                                                            
         LA    R0,100                                                           
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  POTIME+09(2),DUB                                                 
*                                                                               
         XR    RE,RE                                                            
         LA    R0,60                                                            
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  POTIME+06(2),DUB                                                 
*                                                                               
         XR    RE,RE                                                            
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  POTIME+03(2),DUB                                                 
*                                                                               
         XR    RE,RE                                                            
         DR    RE,R0                                                            
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  POTIME+00(2),DUB                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT ASID  AND CALLER RE-RB                                       *         
***********************************************************************         
         SPACE 1                                                                
ASIDOUT  NTR1  ,                                                                
         GOTO1 VHEXOUT,DMCB,TLKLOCK+2,PASID,2                                   
         GOTO1 (RF),(R1),TLKCDISP,PRERB,4                                       
         GOTO1 (RF),(R1),TLKMISC,POMSC,4                                        
         ICM   R0,15,TLKOLOCK                                                   
         BZ    EXITOK                                                           
         GOTO1 (RF),(R1),TLKOLOCK,PROLCK,4                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT FACPAK/TASK                                                  *         
***********************************************************************         
         SPACE 1                                                                
FACOUT   NTR1  ,                                                                
         LA    RF,FACIDTAB         FACPAK=                                      
         USING FACITABD,RF                                                      
FOUT02   CLC   =XL4'FFFFFFFF',FACISN4                                           
         BE    FOUT06                                                           
         CLC   FACIID,TLKTASK                                                   
         BE    FOUT04                                                           
         LA    RF,L'FACITAB(RF)                                                 
         B     FOUT02                                                           
*                                                                               
FOUT04   MVC   PFAC,FACISN4                                                     
         B     FOUT08                                                           
*                                                                               
FOUT06   MVC   PFAC,=CL4'????'                                                  
         DROP  RF                                                               
*                                                                               
FOUT08   CLI   TLKTASK+1,0                                                      
         BE    EXITOK                                                           
         MVC   PTASK,TLKTASK+1                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT FACPAK/TASK FOR LOCK OWNER                                   *         
***********************************************************************         
         SPACE 1                                                                
FACOUTO  NTR1  ,                                                                
         OC    TLKOTASK,TLKOTASK                                                
         BZ    EXITOK                                                           
*                                                                               
         LA    RF,FACIDTAB         FACPAK=                                      
         USING FACITABD,RF                                                      
FOTO02   CLC   =XL4'FFFFFFFF',FACISN4                                           
         BE    FOTO06                                                           
         CLC   FACIID,TLKOTASK                                                  
         BE    FOTO04                                                           
         LA    RF,L'FACITAB(RF)                                                 
         B     FOTO02                                                           
*                                                                               
FOTO04   MVC   POFAC,FACISN4                                                    
         B     FOTO08                                                           
*                                                                               
FOTO06   MVC   POFAC,=CL4'????'                                                 
         DROP  RF                                                               
*                                                                               
FOTO08   CLI   TLKOTASK+1,0                                                     
         BE    EXITOK                                                           
         MVC   POTASK,TLKOTASK+1                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT CALL TYPE                                                    *         
***********************************************************************         
         SPACE 1                                                                
TYPEOUT  NTR1  ,                                                                
         MVC   PACTN,=CL4'????'                                                 
         CLI   TLKACTN,C'L'                                                     
         BNE   *+10                                                             
         MVC   PACTN,=CL4'Lock'                                                 
         CLI   TLKACTN,C'U'                                                     
         BNE   *+10                                                             
         MVC   PACTN,=CL4'Unlk'                                                 
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,TLKWAIT                                                     
         BZ    TYPO2                                                            
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PWAIT,DUB                                                        
*                                                                               
TYPO2    XR    R0,R0                                                            
         ICM   R0,1,TLKRCNT                                                     
         BZ    TYPO4                                                            
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PRCNT,DUB                                                        
*                                                                               
TYPO4    MVC   PERR,TLKERR                                                      
         CLI   PERR,C' '                                                        
         BH    *+8                                                              
         MVI   PERR,C' '                                                        
*                                                                               
         MVC   PRSRC,TLKRSRC                                                    
         MVC   PRBSE,TLKCBASE                                                   
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINTER                                                  *         
***********************************************************************         
         SPACE 1                                                                
PRINTI   NTR1  ,                                                                
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,PZERO                                                       
         ZAP   PAGE,PONE                                                        
M1       USING PLINED,MID1                                                      
M2       USING PLINED,MID2                                                      
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   M1.PTIME,=CL11'Current'                                          
         MVC   M2.PTIME,=CL11'Lock Time'                                        
         MVC   M1.PERR,=C' '                                                    
         MVC   M2.PERR,=C' '                                                    
         MVC   M1.PASID,=CL4'Job'                                               
         MVC   M2.PASID,=CL4'ASID'                                              
         MVC   M1.PFAC,=CL4'Fac'                                                
         MVC   M2.PFAC,=CL4'pak'                                                
         MVC   M1.PTASK,=C'T'                                                   
         MVC   M2.PTASK,=C'k'                                                   
         MVC   M1.PRSRC,=CL4'Reso'                                              
         MVC   M2.PRSRC,=CL4'urce'                                              
         MVC   M1.PACTN,SPACES                                                  
         MVC   M2.PACTN,=CL4'Actn'                                              
         MVC   M1.PWAIT,=CL4'Wait'                                              
         MVC   M2.PWAIT,=CL4'Cnt'                                               
         MVC   M1.PDTIME,=CL11'Lock hold'                                       
         MVC   M2.PDTIME,=CL11'time'                                            
         MVC   M1.PRCNT,=CL4'Rsrc'                                              
         MVC   M2.PRCNT,=CL4'Cnt'                                               
         MVC   M1.PRBSE,=CL4'Call'                                              
         MVC   M2.PRBSE,=CL4'Base'                                              
         MVC   M1.PRERB,=CL8'Caller'                                            
         MVC   M2.PRERB,=CL8'RE-RB'                                             
         MVC   M1.PROLCK(25),=CL25'Owner Information'                           
         MVC   M2.PROLCK,=CL08'Lockword'                                        
         MVC   M2.POTIME,=CL11'Lock Time'                                       
         MVC   M2.POFAC,=CL4'Fac'                                               
         MVC   M2.POTASK,=C'T'                                                  
         MVC   M2.POMSC,=CL8'Misc'                                              
         MVC   M2.POMSC,=CL8'Info'                                              
         B     EXITOK                                                           
         DROP  M1,M2                                                            
         SPACE 2                                                                
***********************************************************************         
* PRINT TITLES                                                        *         
***********************************************************************         
         SPACE 1                                                                
PRINTT   NTR1  ,                                                                
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         PUT   SYSPRINT,SPACES     PRINT CLEAR SPACE UNDERNEATH                 
*                                                                               
         CLC   MID1,SPACES                                                      
         BNH   PRINTT2                                                          
         PUT   SYSPRINT,MID1                                                    
         AP    LINE,=P'1'                                                       
*                                                                               
PRINTT2  CLC   MID2,SPACES                                                      
         BNH   PRINTT4                                                          
         PUT   SYSPRINT,MID2                                                    
         AP    LINE,=P'1'                                                       
*                                                                               
PRINTT4  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRINTL   NTR1  ,                                                                
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL4                                                          
*                                                                               
         ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLES                                 
         PUT   SYSPRINT,SPACES     PRINT CLEAR SPACE UNDERNEATH                 
*                                                                               
         CLC   MID1,SPACES                                                      
         BNH   PRINTL2                                                          
         PUT   SYSPRINT,MID1                                                    
         AP    LINE,=P'1'                                                       
*                                                                               
PRINTL2  CLC   MID2,SPACES                                                      
         BNH   PRINTL4                                                          
         PUT   SYSPRINT,MID2                                                    
         AP    LINE,=P'1'                                                       
*                                                                               
PRINTL4  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CLOSE PRINTER                                                       *         
***********************************************************************         
         SPACE 1                                                                
PRINTX   NTR1  ,                                                                
         CLOSE SYSPRINT                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT LINE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    X                                                                
PTIME    DS    XL11                TIME OF LOCK ENTRY                           
         DS    X                                                                
PERR     DS    XL1                 ERROR RETURN CODE                            
         DS    X                                                                
PASID    DS    XL4                 ASID OF LOCK                                 
         DS    X                                                                
PFAC     DS    XL4                 FACPAK OF LOCK                               
         DS    X                                                                
PTASK    DS    XL1                 TASK OF LOCK                                 
         DS    X                                                                
PRSRC    DS    XL4                 RESOURCE                                     
         DS    X                                                                
PACTN    DS    XL4                 ACTION                                       
         DS    X                                                                
PWAIT    DS    XL4                 WAIT COUNT FOR RESOURCE                      
         DS    X                                                                
PRCNT    DS    XL4                 NUMBER OF TASKS USING SAME RESOURCE          
         DS    X                                                                
PDTIME   DS    XL11                TIME LOCK HAS BEEN HELD                      
         DS    X                                                                
PRBSE    DS    XL4                 CALLER                                       
         DS    X                                                                
PRERB    DS    XL8                 CALLER RE-RB                                 
         DS    X                                                                
PROLCK   DS    XL8                 OWNER LOCK INFO                              
         DS    X                                                                
POTIME   DS    XL11                OWNER LOCK TIME                              
         DS    X                                                                
POFAC    DS    XL4                 OWNER FACPAK                                 
         DS    X                                                                
POTASK   DS    X                   OWNER TASK                                   
         DS    X                                                                
POMSC    DS    XL8                 MISC INFO                                    
         DS    X                                                                
PRDETL   DS    XL48                LOCK DETAILS COPIED OUT                      
*                                                                               
MAIN     CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* READ AND VALIDATE INPUT CARDS                                       *         
***********************************************************************         
         SPACE 1                                                                
CARDIN   NTR1  ,                                                                
*                                                                               
CARD02   GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   =C'/*',CARD         END OF CARDS?                                
         BE    CARDOK              YES - RETURN NORMALLY                        
*                                                                               
         MVC   PLINE+1(L'CARD),CARD                                             
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LA    R2,CARD                                                          
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    CARD02                                                           
*                                                                               
         LA    R1,79(R2)                                                        
         ST    R1,CARDEND          SAVE A(LAST CHAR)                            
*                                                                               
         GOTO1 VSCANNER,DMCB,(C'C',(R2)),(1,SCNBLK)                             
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         L     R3,ACARDTAB                                                      
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
CARD04   CLI   CNAME,CARDEOT       END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,CARDCMP                                                       
         BE    CARD06                                                           
*                                                                               
         LA    R3,CARDTABL(R3)                                                  
         B     CARD04                                                           
*                                                                               
CARDCMP  CLC   SC1STFLD(0),CNAME                                                
*                                                                               
CARD06   CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CARD08              NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     CARD02                                                           
*                                                                               
CARD08   CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CARD10              NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),SC2NDFLD                                                 
         B     CARD02                                                           
*                                                                               
CARD10   DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,CINVLIN          INVALID LINE                                 
         B     CERR                                                             
CEINVKEY LA    R1,CINVKEY          INVALID KEYWORD                              
         B     CERR                                                             
CENOTNUM LA    R1,CNOTNUM          NOT A NUMBER                                 
         B     CERR                                                             
CENOTCHR LA    R1,CNOTCHR          NOT CHARACTER                                
         B     CERR                                                             
CETOOSHT LA    R1,CTOOSHT          TOO SHORT                                    
         B     CERR                                                             
CETOOLNG LA    R1,CTOOLNG          TOO LONG                                     
         B     CERR                                                             
CETOOLOW LA    R1,CTOOLOW          TOO SMALL                                    
         B     CERR                                                             
CETOOBIG LA    R1,CTOOBIG          TOO BIG                                      
         B     CERR                                                             
CENOINP  LA    R1,CNOINP           NO INPUT                                     
         B     CERR                                                             
*                                                                               
CERR     MVC   PLINE,SPACES                                                     
         MVC   PLINE(CERRHDRL),CERRHDR                                          
         MVC   PLINE+CERRHDRL(CERRMSGL),0(R1)                                   
         BAS   RE,PRINTL                                                        
         B     CARDL                                                            
*                                                                               
CARDL    B     EXITL                                                            
CARDOK   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET REQUESTED DATASPACE                                             *         
***********************************************************************         
         SPACE 1                                                                
GETSPC   NTR1  ,                                                                
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   OFFS,WORK+20        EXTRACT VALUES                               
         MVC   ALET,WORK+24                                                     
         MVC   STOKN,WORK+28                                                    
         OC    ALET,ALET                                                        
         BNZ   GETSPCX                                                          
         DC    H'0'                                                             
*                                                                               
GETSPCX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    XBASE ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
ARZERO   DC    16F'0'                                                           
SPACES   DC    CL166' '                                                         
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
MAXLINE  DC    P'60'                                                            
TITLE    DC    CL166'                              LOCKSPC OUTPUT LOG'          
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
ACARDTAB DC    A(CARDTAB)                                                       
VCARDS   DC    V(CARDS)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VSCANNER DC    V(SCANNER)                                                       
VWRKAREA DC    V(WORKAREA)                                                      
*                                                                               
       ++INCLUDE FACIDTAB                                                       
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
CARDTAB  DS    0D                                                               
         DC    CL8'DSPACE  ',F'001',F'012'                                      
         DC    X'05',AL1(CTCHR),AL1(L'DSPACE),AL1(0),AL4(DSPACE)                
         DC    X'FFFFFFFF'                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INPUT CARD STORAGE AREAS                                            *         
***********************************************************************         
         SPACE 1                                                                
DSPACE   DC    CL12' '             NAME OF DATASPACE                            
         SPACE 2                                                                
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
         SPACE 1                                                                
CERRMSGL EQU   40                                                               
CERRHDRL EQU   20                                                               
*                                                                               
CERRHDR  DC    CL20' *** CARD ERROR *** '                                       
CINVLIN  DC    CL40'Invalid Line Format'                                        
CINVKEY  DC    CL40'Invalid Keyword'                                            
CNOTNUM  DC    CL40'Value not a valid number'                                   
CNOTCHR  DC    CL40'Value not a valid character string'                         
CTOOSHT  DC    CL40'Length of input string too short'                           
CTOOLNG  DC    CL40'Length of input string too long'                            
CTOOLOW  DC    CL40'Numeric value too small'                                    
CTOOBIG  DC    CL40'Numeric value too large'                                    
CNOINP   DC    CL40'Invalid/missing value'                                      
         SPACE 1                                                                
         TITLE 'VARIABLE SCAN MODULE FOR CARD VALIDATION'                       
         EJECT                                                                  
***********************************************************************         
* VARIABLE SCAN MODULE FOR CARD VALIDATION                            *         
***********************************************************************         
         SPACE 1                                                                
SCANNER  CSECT                                                                  
         NMOD1 SWORKL,**SCAN**                                                  
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         XR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=H'42'         PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=H'20'                                                    
         MVC   LBOTH,=H'30'                                                     
         CLI   0(R9),C'C'                                                       
         BE    SCAN02                                                           
*                                                                               
SCAN02   SH    R2,=H'8'                                                         
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    SCERR2                                                           
         LA    R5,79(R2)                                                        
*                                                                               
SCAN04   CLI   0(R5),C' '                                                       
         BNE   SCAN06                                                           
         BCTR  R5,0                                                             
         BCT   R4,SCAN04                                                        
*                                                                               
SCAN06   LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         XR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
***********************************************************************         
* HANDLE LINES OF DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
SCAN08   XC    0(12,R3),0(R3)      PRESET A LINE                                
         LH    RF,LBOTH                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   12(0,R3),SSPACES                                                 
         MVC   2(2,R3),=X'E0E0'                                                 
         BAS   RE,GETL                                                          
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   2(R3),0                                                          
         CLI   1(R3),0                                                          
         BNE   *+8                                                              
         MVI   3(R3),0                                                          
         CLC   0(1,R3),LBOTH+1                                                  
         BH    SCERR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    SCERR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN10                                                           
         CLI   0(R3),10                                                         
         BH    SCERR                                                            
*                                                                               
SCAN10   XR    R7,R7                                                            
         ICM   R7,1,0(R3)                                                       
         BZ    SCAN16                                                           
         BCTR  R7,0                                                             
         EX    R7,*+4                                                           
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN12                                                           
         ST    R8,4(R3)                                                         
*                                                                               
SCAN12   LA    R2,2(R2,R7)                                                      
         ICM   R7,1,1(R3)                                                       
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+4                                                           
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN14                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN14                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN14                                                           
         ST    R8,8(R3)                                                         
*                                                                               
SCAN14   LA    R2,2(R2,R7)                                                      
         B     SCAN18                                                           
*                                                                               
VARPAK   PACK  SDUB,0(0,R2)                                                     
*                                                                               
SCAN16   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   SCERR                                                            
*                                                                               
SCAN18   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    SCANOK                                                           
         ICM   R7,1,MAXLINES                                                    
         BZ    SCAN08                                                           
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN08                                                           
*                                                                               
SCANOK   MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     SCANX                                                            
*                                                                               
SCERR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     SCANX                                                            
*                                                                               
SCERR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
*                                                                               
SCANX    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND GET LENGTHS OF FIELDS                                  *         
***********************************************************************         
         SPACE 1                                                                
GETL     NTR1  ,                                                                
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL02                                                           
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL02   CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL14                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL16                                                           
*                                                                               
GETL04   LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL06                                                           
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL12                                                           
*                                                                               
GETL06   NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL08                                                           
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL12                                                           
*                                                                               
GETL08   CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL10                                                           
         MVI   2(R4),0                                                          
         B     GETL12                                                           
*                                                                               
GETL10   CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL12                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
*                                                                               
GETL12   LA    R2,1(R2)                                                         
         B     GETL02                                                           
*                                                                               
GETL14   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     GETLX                                                            
*                                                                               
GETL16   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL04              TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL18                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL18   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL02                                                           
*                                                                               
GETLX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SCANNER LITERALS AND CONSTANTS                                      *         
***********************************************************************         
         SPACE 1                                                                
COMMA    DC    C','                                                             
EQUAL    DC    C'='                                                             
SSPACES  DC    CL80' '                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCANNER WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
SWORKD   DSECT                                                                  
SDUB     DS    D                                                                
SWORK    DS    CL32                                                             
LASTSTOP DS    F                                                                
BORROW   DS    CL1                                                              
MAXLINES DS    CL1                                                              
LROW     DS    H                                                                
LRIGHT   DS    H                                                                
LBOTH    DS    H                                                                
SDISP    DS    H                                                                
*                                                                               
SWORKL   EQU   *-SWORKD                                                         
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
*                                                                               
DMCB     DS    6F                                                               
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
CARDEND  DS    A                                                                
*                                                                               
*                                                                               
OFFS     DS    A                   DATASPACE OFFSET                             
ALET     DS    A                   ALET                                         
STOKN    DS    CL8                 STOKEN                                       
BUFFHDR  DS    A                   A(TABSZIP BUFFER HEADER)                     
THISHDR  DS    A                   A(TABSZIP BUFFER ENTRY)                      
*                                                                               
WORK     DS    CL64                                                             
CARD     DS    CL80                                                             
MID1     DS    CL166                                                            
MID2     DS    CL166                                                            
PLINE    DS    CL166                                                            
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
*                                                                               
       ++INCLUDE DDLOCKTLK                                                      
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    XL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    X                   OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    XL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DC                                                  *         
***********************************************************************         
         SPACE 1                                                                
WORKAREA CSECT                                                                  
         DC    200000X'00'                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 2                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031DDLOCKREP 04/21/99'                                      
         END                                                                    
***********************************************************************         
* LOG PKZIP WORK TO MSGLOG                                            *         
***********************************************************************         
         SPACE 1                                                                
LOGGER   NTR1  BASE=*,LABEL=*                                                   
         LAM   R2,R2,ALET                                                       
         L     R2,THISHDR                                                       
         SAC   512                                                              
         USING TZIPHDRD,R2                                                      
*                                                                               
         MVC   MSGACT,UNKNOWN      ACTION=                                      
         CLI   TZHFLAG1,TZHF1CMP                                                
         BNE   *+10                                                             
         MVC   MSGACT,CMPRS                                                     
         CLI   TZHFLAG1,TZHF1UNC                                                
         BNE   *+10                                                             
         MVC   MSGACT,UNCMPRS                                                   
*                                                                               
         L     RF,TIMEIN           START TIME (TU)                              
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGTIN,DUB                                                       
*                                                                               
         L     RF,TIMEOUT          END TIME (TU)                                
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGTOUT,DUB                                                      
*                                                                               
         L     RF,TIMEOUT           PROCESS TIME (TU)                           
         S     RF,TIMEIN                                                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGPTIM,DUB                                                      
*                                                                               
         L     RF,TZHULEN          INPUT LENGTH                                 
         CLI   TZHFLAG1,TZHF1CMP                                                
         BE    *+8                                                              
         L     RF,TZHCLEN                                                       
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGIN,DUB                                                        
*                                                                               
         L     RF,TZHCLEN          OUTPUT LENGTH                                
         CLI   TZHFLAG1,TZHF1CMP                                                
         BE    *+8                                                              
         L     RF,TZHULEN                                                       
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGOUT,DUB                                                       
*                                                                               
         MVC   MSGCMP,=CL8' '      COMPRESSION RATIO                            
         L     RF,TZHULEN          UNCOMPRESSED LENGTH                          
         CVD   RF,DUB                                                           
         ZAP   UNCPCK,DUB                                                       
         L     RF,TZHCLEN          COMPRESSED LENGTH                            
         CVD   RF,DUB                                                           
         ZAP   CMPPCK,DUB                                                       
         SAC   0                                                                
*                                                                               
         ZAP   DIVPCK,CMPPCK       COMPRESS LENGTH * 100.00                     
         MP    DIVPCK,=P'10000'                                                 
         DP    DIVPCK,UNCPCK                                                    
         ZAP   PCTPCK,=P'10000'                                                 
         SP    PCTPCK,DIVPCK(8)   100.00-(% OF ORIGINAL SIZE)                   
         EDIT  PCTPCK,MSGCMP,2,ALIGN=LEFT,ZERO=NOBLANK                          
         LA    R1,MSGCMP                                                        
         AR    R1,R0                                                            
         MVI   0(R1),C'%'                                                       
*                                                                               
         XR    R0,R0                HAVE TO CLEAR FOR MULTILINE WTO             
         WTO   TEXT=((MSGHL,C),(MSG1L,D),(MSG2L,D),(MSG3L,D),(0,E)),MCS*        
               FLAG=HRDCPY                                                      
