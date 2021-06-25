*          DATA SET DDSDSPACE  AT LEVEL 021 AS OF 05/01/02                      
*PHASE SDSPACE                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE UNSCAN                                                                 
         TITLE 'DDSDSPACE - REPORT ON DATA SPACES'                              
         PRINT NOGEN                                                            
DSPACE   CSECT                                                                  
         NBASE WORKX-WORKD,*DSPACE*,=A(WORKAREA),RA,R9                          
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         BRAS  RE,INIT             READ CARDS ETC                               
*                                                                               
         CLI   MODE,C'R'           MODE = REPORT                                
         BNE   DSP02                                                            
         BAS   RE,DREPORT                                                       
         B     XBASE                                                            
*                                                                               
DSP02    B     XBASE                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         MVC   TITLE,SPACES        SET UP TITLES AND INIT PRINTING              
         MVC   TITLED(L'CTITLE),CTITLE                                          
         MVC   TITLEU,SPACES                                                    
         MVC   TITLEDU(L'CTITLEU),CTITLEU                                       
         BAS   RE,PRINTI                                                        
         MVC   PLINE,SPACES                                                     
         BAS   RE,PRINTL                                                        
*                                                                               
         LA    R2,CARD                                                          
INIT02   GOTO1 VCARDS,DMCB,(R2),=C'RE00'                                        
         CLC   =C'/*',0(R2)        END OF CARDS?                                
         BE    INIT04              YES                                          
         MVC   PLINE+1(L'CARD),CARD                                             
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R2               VALIDATE KEYWORD=VALUE                       
         BAS   RE,CARDVAL                                                       
         BE    INIT02                                                           
         B     XBASE               CC:NE MEANS INVALID KEYWORD                  
*                                                                               
INIT04   LHI   R0,21               HOOK INTO DMGR DATASPACE                     
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DMGRDSPC                                              
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    INIT06                                                           
         LA    R1,1                                                             
         BRAS  RE,PUTMESS                                                       
         ABEND 101,DUMP                                                         
*                                                                               
INIT06   MVC   DMOFFS,WORK+20      EXTRACT VALUES                               
         MVC   DMALET,WORK+24                                                   
         MVC   DMTOKN,WORK+28                                                   
         OC    DMALET,DMALET                                                    
         BNZ   INIT08                                                           
         LA    R1,1                                                             
         BRAS  RE,PUTMESS                                                       
         ABEND 102,DUMP                                                         
*                                                                               
INIT08   LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),TABSDSPC                                              
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    INIT10                                                           
         LA    R1,2                                                             
         BRAS  RE,PUTMESS                                                       
         ABEND 103,DUMP                                                         
*                                                                               
INIT10   MVC   TBOFFS,WORK+20      EXTRACT VALUES                               
         MVC   TBALET,WORK+24                                                   
         MVC   TBTOKN,WORK+28                                                   
         OC    TBALET,TBALET                                                    
         BNZ   INIT12                                                           
         LA    R1,2                                                             
         BRAS  RE,PUTMESS                                                       
         ABEND 104,DUMP                                                         
*                                                                               
INIT12   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* REPORT MODE                                                         *         
***********************************************************************         
         SPACE 1                                                                
DREPORT  NTR1  ,                                                                
         BRAS  RE,DREPTABS         REPORT ON TABS DATASPACE INFO                
         BRAS  RE,DREPDMGR         REPROT ON DMGR DATASPACE INFO                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* REPORT ON TABS DATASPACE                                            *         
***********************************************************************         
         SPACE 1                                                                
DREPTABS NTR1  ,                                                                
         MVC   PLINE,SPACES        PRINT TITLES FOR TABS DSPACE                 
         BAS   RE,PRINTL                                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLED(L'TTITLE),TTITLE                                          
         MVC   PLINE,TITLED                                                     
         BAS   RE,PRINTL                                                        
         MVC   TITLEU,SPACES                                                    
         MVC   TITLEDU(L'TTITLEU),TTITLEU                                       
         MVC   PLINE,TITLEDU                                                    
         BAS   RE,PRINTL                                                        
         MVC   PLINE,SPACES                                                     
         BAS   RE,PRINTL                                                        
*                                                                               
         LAM   R0,RF,ARZERO                                                     
         LAM   R2,R2,TBALET                                                     
         L     R2,TBOFFS                                                        
         SAC   512                                                              
         AHI   R2,L'DSPHDR                                                      
         USING DMSPACED,R2                                                      
*                                                                               
         LHI   RF,2                FIRST RESOURCE IS 16*4 ADDRESSES             
         STH   RF,RESNUM                                                        
*                                                                               
DRPT02   OC    DSPNAME,DSPNAME     ANYTHING IN THIS HEADER?                     
         BZ    DRPT16              NO - DO NEXT ONE                             
*                                                                               
         MVC   PLINE+00(08),DSPNAME                                             
         MVC   PLINE+09(06),FREE                                                
         ICM   RF,15,DSPLOCK       LOCKED?                                      
         BNZ   DRPT04              YES - REPORT ON IT                           
         SAC   0                                                                
         BAS   RE,PRINTL                                                        
         SAC   512                                                              
         B     DRPT16                                                           
*                                                                               
DRPT04   MVC   PLINE+09(06),LOCKED                                              
*                                                                               
         LA    R4,UNBLOCK          R4=CURRENT OUTPUT BLOCK                      
         XR    R5,R5               R5 = NUMBER OF FIELDS                        
*                                                                               
         LR    R0,R4                                                            
         LHI   R1,UNBLOCKL                                                      
         LHI   RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
*                                                                               
         MVC   0(10,R4),TYPE                                                    
         MVC   10(10,R4),NORMAL    SET NORMAL OR LONG LOCK                      
         TM    DSPFLAG,DSPLONG                                                  
         BZ    *+10                                                             
         MVC   10(10,R4),LONG                                                   
         AHI   R4,20                                                            
         AHI   R5,1                                                             
*                                                                               
         TM    DSPFLAG,DSPIOW      SET IF WAITING FOR I/O                       
         BZ    *+18                                                             
         MVC   0(10,R4),IOWAIT                                                  
         AHI   R4,20                                                            
         AHI   R5,1                                                             
*                                                                               
         MVC   0(10,R4),ONLINE     SET ONLINE OR OFFLINE                        
         TM    DSPJOB,X'80'                                                     
         BZ    DRPT06                                                           
         MVC   0(10,R4),OFFLINE                                                 
         AHI   R4,20                                                            
         AHI   R5,1                                                             
*                                                                               
         MVC   0(14,R4),JOBNUM                                                  
         MVC   HALF,DSPJOB                                                      
         NC    HALF,=X'7FFF'                                                    
         EDIT  (B2,HALF),(6,10(R4)),FILL=0                                      
         AHI   R4,20                                                            
         AHI   R5,1                                                             
         B     DRPT12                                                           
*                                                                               
DRPT06   MVC   0(10,R4),FACPAK     SET FACPAK                                   
         LA    RF,FACIDTAB                                                      
         USING FACITABD,RF                                                      
DRPT08   LA    R1,QUERY                                                         
         CLI   FACISN4,X'FF'                                                    
         BE    DRPT10                                                           
         LA    R1,FACISN4                                                       
         CLC   FACIID,DSPJOB                                                    
         BE    DRPT10                                                           
         AHI   RF,L'FACITAB                                                     
         B     DRPT08                                                           
*                                                                               
DRPT10   MVC   10(L'FACISN4,R4),0(R1)                                           
         AHI   R4,20                                                            
         AHI   R5,1                                                             
         DROP  RF                                                               
*                                                                               
         MVC   0(10,R4),TASK       SET TASK                                     
         XR    R0,R0                                                            
         IC    R0,DSPJOB+1                                                      
         EDIT  (R0),(3,10(R4)),ZERO=NOBLANK,ALIGN=LEFT                          
         AHI   R4,20                                                            
         AHI   R5,1                                                             
*                                                                               
DRPT12   TM    DSPECB,X'40'        SET IF POSTED                                
         BZ    *+18                                                             
         MVC   0(10,R4),POSTED                                                  
         AHI   R4,20                                                            
         AHI   R5,1                                                             
*                                                                               
         GOTO1 ,DMCB,((R5),UNBLOCK),(C'C',PLINE+18),0,0                         
*                                                                               
DRPT14   GOTO1 VUNSCAN,(R1)                                                     
         SAFE  CLEAR=ARZERO                                                     
         SAC   0                                                                
         BAS   RE,PRINTL                                                        
         SAC   512                                                              
         CLI   0(R1),0                                                          
         BNE   DRPT14                                                           
*                                                                               
DRPT16   AHI   R2,L'DSPHDR                                                      
         LH    RF,RESNUM                                                        
         AHI   RF,1                                                             
         STH   RF,RESNUM                                                        
         CHI   RF,RMAXRES                                                       
         BNH   DRPT02                                                           
*                                                                               
DRPT18   SAC   0                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* REPORT ON DMGR DATASPACE                                            *         
***********************************************************************         
         SPACE 1                                                                
DREPDMGR NTR1  ,                                                                
         MVC   PLINE,SPACES        PRINT TITLES FOR TABS DSPACE                 
         BAS   RE,PRINTL                                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLED(L'DTITLE),DTITLE                                          
         MVC   PLINE,TITLED                                                     
         BAS   RE,PRINTL                                                        
         MVC   TITLEU,SPACES                                                    
         MVC   TITLEDU(L'DTITLEU),DTITLEU                                       
         MVC   PLINE,TITLEDU                                                    
         BAS   RE,PRINTL                                                        
         MVC   PLINE,SPACES                                                     
         BAS   RE,PRINTL                                                        
*                                                                               
         LAM   R0,RF,ARZERO                                                     
         LAM   R2,R2,DMALET                                                     
         L     R2,DMOFFS                                                        
         SAC   512                                                              
         AHI   R2,L'DSPHDR                                                      
         USING DMSPACED,R2                                                      
*                                                                               
         LHI   RF,2                FIRST RESOURCE IS 16*4 ADDRESSES             
         STH   RF,RESNUM                                                        
*                                                                               
DRPD02   OC    DSPNAME,DSPNAME     ANYTHING IN THIS HEADER?                     
         BZ    DRPD16              NO - DO NEXT ONE                             
*                                                                               
         MVC   PLINE+00(08),DSPNAME                                             
         MVC   PLINE+09(06),FREE                                                
         ICM   RF,15,DSPLOCK       LOCKED?                                      
         BNZ   DRPD04              YES - REPORT ON IT                           
         SAC   0                                                                
         BAS   RE,PRINTL                                                        
         SAC   512                                                              
         B     DRPD16                                                           
*                                                                               
DRPD04   MVC   PLINE+09(06),LOCKED                                              
*                                                                               
         LA    R4,UNBLOCK          R4=CURRENT OUTPUT BLOCK                      
         XR    R5,R5               R5 = NUMBER OF FIELDS                        
*                                                                               
         LR    R0,R4                                                            
         LHI   R1,UNBLOCKL                                                      
         LHI   RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
*                                                                               
         MVC   0(10,R4),TYPE                                                    
         MVC   10(10,R4),NORMAL    SET NORMAL OR LONG LOCK                      
         TM    DSPFLAG,DSPLONG                                                  
         BZ    *+10                                                             
         MVC   10(10,R4),LONG                                                   
         AHI   R4,20                                                            
         AHI   R5,1                                                             
*                                                                               
         TM    DSPFLAG,DSPIOW      SET IF WAITING FOR I/O                       
         BZ    *+18                                                             
         MVC   0(10,R4),IOWAIT                                                  
         AHI   R4,20                                                            
         AHI   R5,1                                                             
*                                                                               
         MVC   0(10,R4),ONLINE     SET ONLINE OR OFFLINE                        
         TM    DSPJOB,X'80'                                                     
         BZ    DRPD06                                                           
         MVC   0(10,R4),OFFLINE                                                 
         AHI   R4,20                                                            
         AHI   R5,1                                                             
*                                                                               
         MVC   0(14,R4),JOBNUM                                                  
         MVC   HALF,DSPJOB                                                      
         NC    HALF,=X'7FFF'                                                    
         EDIT  (B2,HALF),(6,10(R4)),FILL=0                                      
         AHI   R4,20                                                            
         AHI   R5,1                                                             
         B     DRPD12                                                           
*                                                                               
DRPD06   MVC   0(10,R4),FACPAK     SET FACPAK                                   
         LA    RF,FACIDTAB                                                      
         USING FACITABD,RF                                                      
DRPD08   LA    R1,QUERY                                                         
         CLI   FACISN4,X'FF'                                                    
         BE    DRPD10                                                           
         LA    R1,FACISN4                                                       
         CLC   FACIID,DSPJOB                                                    
         BE    DRPD10                                                           
         AHI   RF,L'FACITAB                                                     
         B     DRPD08                                                           
*                                                                               
DRPD10   MVC   10(L'FACISN4,R4),0(R1)                                           
         AHI   R4,20                                                            
         AHI   R5,1                                                             
         DROP  RF                                                               
*                                                                               
         MVC   0(10,R4),TASK       SET TASK                                     
         XR    R0,R0                                                            
         IC    R0,DSPJOB+1                                                      
         EDIT  (R0),(3,10(R4)),ZERO=NOBLANK,ALIGN=LEFT                          
         AHI   R4,20                                                            
         AHI   R5,1                                                             
*                                                                               
DRPD12   TM    DSPECB,X'40'        SET IF POSTED                                
         BZ    *+18                                                             
         MVC   0(10,R4),POSTED                                                  
         AHI   R4,20                                                            
         AHI   R5,1                                                             
*                                                                               
         GOTO1 ,DMCB,((R5),UNBLOCK),(C'C',PLINE+18),0,0                         
*                                                                               
DRPD14   GOTO1 VUNSCAN,(R1)                                                     
         SAFE  CLEAR=ARZERO                                                     
         SAC   0                                                                
         BAS   RE,PRINTL                                                        
         SAC   512                                                              
         CLI   0(R1),0                                                          
         BNE   DRPD14                                                           
*                                                                               
DRPD16   AHI   R2,L'DSPHDR                                                      
         LH    RF,RESNUM                                                        
         AHI   RF,1                                                             
         STH   RF,RESNUM                                                        
         CHI   RF,RMAXRES                                                       
         BNH   DRPD02                                                           
*                                                                               
DRPD18   SAC   0                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD ADCONS                                                        *         
* NTRY: ADSDATA POINTS TO FIRST TABLE POSITION                        *         
***********************************************************************         
         SPACE 1                                                                
ADCONS   NTR1  ,                                                                
         LAM   R2,R2,DMALET                                                     
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         L     RF,ADSDATA                                                       
         STCM  RF,15,TABSDUMP      DUMP HEADERS ARE ALLOWED 4K                  
         AHI   RF,FOURK                                                         
*                                                                               
         STCM  RF,15,TABSTMS       DUMP HEADERS ARE ALLOWED 4K                  
         ICM   RE,15,TPSTF         + PAGES FOR TEMPEST BUFFER DETAILS           
         SLL   RE,4                16*ENTRIES                                   
         AHI   RE,TTMSHDRL                                                      
         SRL   RE,12                                                            
         LA    RE,1(RE)                                                         
         SLL   RE,12                                                            
         AR    RF,RE                                                            
*                                                                               
         STCM  RF,15,TABSZIP       ZIP BLOCK HAS EQUATED LENGTHS                
         A     RF,=A(TZIPDSP+TZBLKDSP+TZBUFDSP+TABSID+TABSID)                   
         LA    RF,4095(RF)                                                      
         SRL   RF,12                                                            
         SLL   RF,12               ROUND UP TO NEXT 4K                          
*                                                                               
         STCM  RF,15,TABSRUN       SET A(SERVER TABLE)                          
         AHI   RF,TABRUNPG*FOURK   (#PAGES*4096)                                
*                                                                               
         STCM  RF,15,TABSPQNX      PQ BLOCK HAS EQUATED VALUES                  
         A     RF,=A((TPQQCNT*TPQQLEN)+TPQLID+TPQLENQ)                          
         LA    RF,4095(RF)                                                      
         SRL   RF,12                                                            
         SLL   RF,12               ROUND UP TO NEXT 4K                          
*                                                                               
         STCM  RF,15,TABSLOX                                                    
         ICM   RE,15,NLOCKS                                                     
         MHI   RE,TLKLEN                                                        
         AR    RF,RE                                                            
         LA    RF,4095(RF)                                                      
         SRL   RF,12                                                            
         SLL   RF,12               ROUND UP TO NEXT 4K                          
*                                                                               
         STCM  RF,15,ADSDATA                                                    
         SAC   0                                                                
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE ADCONS                                                   *         
***********************************************************************         
         SPACE 1                                                                
ADCINI   NTR1  ,                                                                
         BRAS  RE,SETDMPS      *** BUILD DUMP HEADER DETAILS                    
*                                                                               
ADCI18   LAM   R2,R2,DMALET    *** BUILD PKZIP BLOCKS                           
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSZIP                                                    
         USING TABSZIPD,R2                                                      
         MVC   0(16,R2),=CL16'*ZIP*ZIP*ZIP*ZIP'                                 
         LA    R2,TABSID(,R2)                                                   
*                                                                               
         LA    R0,TAZNUM                                                        
ADCI20   XC    TABSZIPD(TZLENQ),TABSZIPD                                        
         LA    R2,TZLENQ(,R2)                                                   
         BCT   R0,ADCI20                                                        
*                                                                               
         MVC   0(16,R2),=CL16'*ZIPHDR**ZIPHDR*'                                 
         LA    R2,TABSID(,R2)                                                   
         LR    R3,R2                                                            
         AH    R3,=Y(TZBLKDSP)     R3 POINTS TO OUTPUT BUFFERS                  
*                                                                               
         USING TZIPHDRD,R2                                                      
         LA    R0,TZHNUM                                                        
         L     RF,=A(TZBUFLEN)                                                  
ADCI22   XC    TZIPHDRD(TZHLENQ),TZIPHDRD                                       
         STCM  R3,15,TZHBODY                                                    
         STCM  RF,15,TZHBLEN                                                    
         LA    R2,TZHLENQ(,R2)                                                  
         AR    R3,RF                                                            
         BCT   R0,ADCI22                                                        
*                                                                               
ADCI24   LAM   R2,R2,DMALET    *** BUILD SERVER INDEX BLOCKS                    
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSRUN                                                    
         USING TABSRUND,R2                                                      
         MVC   TABSLRUN,=CL16'*RUN*RUN*RUN*RUN'                                 
         LR    R0,R2                                                            
         AHI   R0,TABSRUNL                                                      
         ST    R0,TABSASVR         SET A(SERVER TABLE)                          
         AHI   R0,TABSVRLN                                                      
         ST    R0,TABSAQUE         SET A(QUEUE TABLE)                           
         LHI   R0,TABSSVRN                                                      
         STH   R0,TABSNSVR         SET N'SERVER ENTRIES                         
         LHI   R0,TABSQUEN                                                      
         STH   R0,TABSNQUE         SET N'QUEUE ENTRIES                          
*                                                                               
         LAM   R2,R2,DMALET    *** BUILD PQ INDEX BLOCKS                        
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSPQNX                                                   
         MVC   0(TPQLID,R2),=CL16'*PQI*PQI*PQI*PQI'                             
         AHI   R2,TPQLENQ                                                       
         MVC   0(TPQLID,R2),=CL16'*PQQ*PQQ*PQQ*PQQ'                             
*                                                                               
ADCI26   LAM   R2,R2,DMALET    *** BUILD LOCKTABLE                              
         L     R2,AHEADER                                                       
         SAC   512                                                              
         ICM   R2,15,TABSLOX                                                    
         ICM   RF,15,NLOCKS                                                     
         STCM  RF,15,12(R2)    SET NUMBER OF ENTRIES                            
         MHI   RF,TLKLEN                                                        
         AR    RF,R2                                                            
         STCM  RF,15,4(R2)     SET A(END)                                       
         MVI   8(R2),C'Y'                                                       
         LA    RF,TLKLEN(R2)                                                    
         STCM  RF,15,0(R2)     SET A(CURRENT)                                   
*                                                                               
ADCI28   LAM   R2,R2,DMALET    *** BUILD TEMPEST INFORMATION                    
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSTMS                                                    
         USING TTMSHDRD,R2                                                      
         XC    0(TTMSHDRL,R2),0(R2)                                             
         MVC   0(8,R2),=CL8'*TEMPEST'                                           
         ICM   RE,15,TPST                                                       
         STCM  RE,15,TTMSNTRY                                                   
*                                                                               
ADCI30   DS    0H              *** NEXT ADCON HERE                              
         B     ADCIX                                                            
*                                                                               
ADCIX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER INFO                                                   *         
* NTRY: ADSDATA POINTS TO FIRST TABLE POSITION                        *         
***********************************************************************         
         SPACE 1                                                                
HEADERS  NTR1  ,                                                                
         LAM   R2,R2,DMALET                                                     
         L     R2,AHEADER                                                       
         SAC   512                 SET UP AMODE                                 
         USING DMSPACED,R2                                                      
*                                                                               
         LA    R3,HDRTAB           TABLE OF DATASPACE ENTRIES                   
         USING HDRTABD,R3                                                       
         B     HDR04               FIRST 64 BYTES FREE                          
*                                                                               
HDR02    CLI   HDRNAME,X'FF'       REACHED END OF HEADER TABLE?                 
         BE    HDR06               YES                                          
*                                                                               
         STCM  R2,15,HDRADR        SAVE A(IN DATASPACE) FOR LATER               
         MVC   DSPNAME,HDRNAME     EYECATCHER                                   
         MVC   DSPTYPE,HDRTYPE     TYPE                                         
         ICM   RF,15,ADSDATA       A(FIRST ROW)                                 
         STCM  RF,15,DSPECB                                                     
         MVC   DSPTWIDE,HDRWIDTH   WIDTH                                        
         ICM   R1,15,HDRNUM          NUMBER OF ENTRIES                          
         MH    R1,HDRWIDTH         * WIDTH OF 1 ENTRY                           
         AR    RF,R1               + A(START)                                   
         BCTR  RF,0                                                             
         STCM  RF,15,DSPTEND       A(END-1)                                     
         LA    RF,1(RF)                                                         
*                                                                               
         LA    RF,4095(RF)         ROUND TO NEXT HIGHEST 4K MULTIPLE            
         SRL   RF,12                                                            
         SLL   RF,12                                                            
         STCM  RF,15,ADSDATA       SET A(NEXT TABLE)                            
*                                                                               
         LA    R3,HDRTABL(R3)      NEXT TABLE ENTRY                             
*                                                                               
HDR04    LA    R2,L'DSPHDR(,R2)    NEXT HEADER                                  
         B     HDR02                                                            
*                                                                               
HDR06    SAC   0                                                                
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE DETAILS                                                 *         
* NTRY: ADSDATA POINTS TO FIRST TABLE POSITION                        *         
***********************************************************************         
         SPACE 1                                                                
TABLES   NTR1  ,                                                                
         BAS   RE,TSTSET                                                        
         BAS   RE,DDASET                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP TSTTAB ENTRIES                                               *         
***********************************************************************         
         SPACE 1                                                                
TSTSET   NTR1  ,                                                                
         STAR  CLEAR=ARZERO,ARS=ON                                              
         LAM   R2,R2,DMALET                                                     
         ICM   R2,15,=AL4(DTTST)   ADDRESS TSTTAB HEADER ENTRY                  
         SLL   R2,17                                                            
         SRL   R2,17-6                                                          
         A     R2,AHEADER                                                       
         USING DMSPACED,R2                                                      
*                                                                               
         CPYA  RF,R2                                                            
         LA    RF,*+10             GET INTO 31 BIT MODE (JUST IN CASE)          
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
*                                                                               
         XR    R4,R4               SET UP BXLE PARAMETERS                       
         ICM   R4,3,DSPTWIDE                                                    
         ICM   R5,15,DSPTEND                                                    
         ICM   R2,7,DSPTFRST+1                                                  
         USING TSTTABD,R2          PASS TSTTAB TO CLEAR A(TRCTAB)               
*                                                                               
         XC    TSTTRC,TSTTRC       FIRST LINE HOLDS SPECIAL INFORMATION         
         XC    TSTSCT,TSTSCT                                                    
         ICM   RF,15,PTSTDA        FIRST 2 BYTES ARE # TRACKS PER CI            
         STCM  RF,3,0(R2)                                                       
         ICM   RF,15,DCTST         NEXT 2 BYTES ARE # ENTRIES IN TABLE          
         STCM  RF,3,2(R2)                                                       
         B     TSTS06              REST OF LINE IS EMPTY                        
*                                                                               
TSTS02   XC    TSTTRC,TSTTRC                                                    
         ICM   RF,15,ADSDATA       NEXT FREE ADDRESS                            
         ST    RF,TSTTRC           SET BUFFER ADDRESS                           
*                                                                               
         ICM   R0,15,STRC          GET LENGTH OF TRC BUFFER                     
         BZ    TSTS04                                                           
         AR    R0,RF                                                            
         BCTR  R0,0                                                             
         STCM  R0,15,40(RF)        SET END ADDRESS                              
*                                                                               
         LA    R0,4                SET EYECATCHER                               
         MVC   0(8,RF),=C'*TRCBUFF'                                             
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ICM   RF,15,ADSDATA                                                    
         ICM   R0,15,STRC                                                       
         AR    RF,R0                                                            
         STCM  RF,15,ADSDATA       BUMP ADSDATA TO NEXT FREE                    
*                                                                               
TSTS04   XC    TSTSCT,TSTSCT       PROCESS SCRIPT TRACE TABLE                   
         ICM   R0,15,SSCT          TEST NEED SCRIPT TRACE BUFFER                
         BZ    TSTS06                                                           
*                                                                               
         STCM  RF,15,TSTSCT         SET BUFFER ADDRESS                          
         AR    R0,RF                                                            
         BCTR  R0,0                                                             
         STCM  R0,15,SCTEND-SCTTABD(RF)  SET END ADDRESS                        
*                                                                               
         LA    R0,4                SET EYECATCHER                               
         MVC   0(8,RF),=C'*SCTBUF*'                                             
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ICM   RF,15,ADSDATA                                                    
         ICM   R0,15,SSCT                                                       
         AR    RF,R0                                                            
         STCM  RF,15,ADSDATA       BUMP ADSDATA TO NEXT FREE                    
*                                                                               
TSTS06   BXLE  R2,R4,TSTS02                                                     
         DROP  R2                                                               
*                                                                               
         REAR  ARS=OFF                                                          
         LA    RF,*+6              GET OUT OF 31-BIT MODE                       
         BSM   0,RF                                                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP DARE MAIL TABLE ENTRIES                                      *         
***********************************************************************         
         SPACE 1                                                                
DDASET   NTR1  ,                                                                
         STAR  CLEAR=ARZERO,ARS=ON                                              
         LAM   R2,R2,DMALET                                                     
         ICM   R2,15,=AL4(DTDARE)  ADDRESS DARETAB HEADER ENTRY                 
         SLL   R2,17                                                            
         SRL   R2,17-6                                                          
         A     R2,AHEADER                                                       
         USING DMSPACED,R2                                                      
         ICM   R2,7,DSPTFRST+1                                                  
*                                                                               
         LA    RF,*+10             GET INTO 31 BIT MODE (JUST IN CASE)          
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
*                                                                               
         USING TABDARED,R2                                                      
         LHI   RF,TBDSORT-TABDARED STORAGE HEADER LENGTH                        
         XR    RE,RE                                                            
         LA    R0,TBDDARL          LENGTH OF A SLOT                             
         DR    RE,R0               NUMBER OF ENTRIES FOR STORAGE HEADER         
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RE,1(RF)            PUT SPACE FOR FRONT REQUIRED IN RE           
*                                                                               
         ICM   RF,15,DCDARE        HALVE THE NUMBER OF ENTRIES                  
         SR    RF,RE                                                            
         SRL   RF,1                                                             
         STCM  RF,15,TBDMAX        SET MAX NUMBER OF ENTRIES                    
*                                                                               
         LA    RE,TBDDARL          LENGTH OF A SINGLE ENTRY                     
         MSR   RF,RE               RF=LENGTH OF UNSORTED TABLE                  
         AR    RF,R2                                                            
         AHI   RF,TBDSORT-TABDARED PLUS HEADER                                  
         STCM  RF,15,TBDFRST       SET A(UNSORTED TABLE)                        
*                                                                               
         XC    TBDNOW,TBDNOW       RESET CURRENT COUNT                          
*                                                                               
         USING BSPARA,TBDBSP   *** SET PARAMETERS FOR BINSRCH                   
         LA    RF,TBDSORT                                                       
         STCM  RF,15,BSPSTRT       SET A(SORTED TABLE)                          
*                                                                               
         LA    RF,TBDKEYL                                                       
         STCM  RF,15,BSPLENK       KEY LENGTH                                   
         MVI   BSPKEYD,0           DISPLACEMENT TO KEY                          
         LA    RF,TBDDARL                                                       
         STCM  RF,15,BSPLENR       RECORD LENGTH                                
*                                                                               
         ICM   RE,15,DCDARE        HALVE THE NUMBER OF ENTRIES                  
         SRL   RE,1                                                             
         LA    RF,TBDDARL          LENGTH OF A SINGLE ENTRY                     
         MSR   RE,RF               RE=LENGTH OF SORTED TABLE                    
*                                                                               
         AHI   RE,-(TBDSORT-TABDARED)                                           
         SRDL  RE,32               REDUCE BY LENGTH OF HEADER INFO              
         LA    R0,TBDDARL                                                       
         DR    RE,R0               FIND HOW MANY WILL FIT INTO SPACE            
         STCM  RF,15,BSPEND        SET MAX NUMBER OF RECORDS                    
*                                                                               
         XC    BSPNOR,BSPNOR       TABLE IS EMPTY PRESENTLY                     
         STAM  R2,R2,BSPARS        SET ACCESS REGISTER                          
*                                                                               
         REAR  ARS=OFF                                                          
         LA    RF,EXITOK           GET OUT OF 31-BIT MODE                       
         BSM   0,RF                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
         SPACE 1                                                                
CARDVAL  NTR1  ,                                                                
         ST    RD,CARDRD                                                        
         LR    R2,R1               R2=A(CARD START)                             
         LA    R1,79(R2)                                                        
         ST    R1,CARDEND          SAVE A(LAST CHAR)                            
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),(1,SCNBLK)                          
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         LA    R3,CARDTAB                                                       
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,*+8                                                           
         BE    CARDV06                                                          
         CLC   SC1STFLD(0),CNAME                                                
CARDV04  LA    R3,CARDTABL(R3)                                                  
         B     CARDV02                                                          
*                                                                               
CARDV06  CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CARDV08             NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
CARDV08  CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CARDV10             NO                                           
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
         B     EXITOK                                                           
*                                                                               
CARDV10  DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,=CL40'Invalid Line Format'                                    
         B     CERR                                                             
*                                                                               
CEINVKEY LA    R1,=CL40'Invalid Keyword'                                        
         B     CERR                                                             
*                                                                               
CENOTNUM LA    R1,=CL40'Value not a valid number'                               
         B     CERR                                                             
*                                                                               
CENOTCHR LA    R1,=CL40'Value not a valid character string'                     
         B     CERR                                                             
*                                                                               
CETOOSHT LA    R1,=CL40'Length of input string too short'                       
         B     CERR                                                             
*                                                                               
CETOOLNG LA    R1,=CL40'Length of input string too long'                        
         B     CERR                                                             
*                                                                               
CETOOLOW LA    R1,=CL40'Numeric value too small'                                
         B     CERR                                                             
*                                                                               
CETOOBIG LA    R1,=CL40'Numeric value too large'                                
         B     CERR                                                             
*                                                                               
CENOINP  LA    R1,=CL40'Invalid/missing value'                                  
         B     CERR                                                             
*                                                                               
CERR     L     RD,CARDRD                                                        
         MVC   PLINE,SPACES                                                     
         MVC   PLINE(15),=CL15' *** ERROR ***'                                  
         MVC   PLINE+15(40),0(R1)                                               
         BAS   RE,PRINTL                                                        
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* OUTPUT TEST RESULTS FOR ANALYSIS                                    *         
***********************************************************************         
         SPACE 1                                                                
RESULTS  NTR1  ,                                                                
         MVC   TITLE,SPACES                                                     
         MVC   TITLED(L'DTITLE),DTITLE    SET UP TITLE                          
         MVC   PLINE,SPACES                                                     
         MVC   PLINED(L'DTITLEU),DTITLEU  SET UP TITLE UNDERLINE                
         ZAP   LINE,MAXLINE               FORCE TITLE PRINTING                  
         BAS   RE,PRINTL                  PRINT TITLE AND UNDERLINE             
*                                                                               
         L     R2,AHEADER          TEST IS IN CORE                              
         USING DMSPACED,R2                                                      
*                                                                               
         LA    R0,4                FIRST 64 BYTES ARE ADCONS                    
RES02    GOTO1 VHEXOUT,DMCB,(R2),MYWORK,16,0                                    
         MVC   PLINED+00+0(8),MYWORK+0                                          
         MVC   PLINED+08+1(8),MYWORK+8                                          
         MVC   PLINED+16+2(8),MYWORK+16                                         
         MVC   PLINED+24+3(8),MYWORK+24                                         
         BAS   RE,PRINTL                                                        
         LA    R2,16(R2)                                                        
         BCT   R0,RES02                                                         
*                                                                               
         LA    R3,HDRTAB           NOW OUTPUT DATASPACE ENTRIES                 
         USING HDRTABD,R3                                                       
*                                                                               
RES04    CLI   HDRNAME,X'FF'       REACHED END OF HEADER TABLE?                 
         BE    RES06               YES                                          
         CLC   DSPNAME,HDRNAME                                                  
         BE    *+6                                                              
         DC    H'0'                SOMETHING BADLY WRONG                        
*                                                                               
         MVC   PLINED(L'DSPNAME),DSPNAME             NAME                       
         MVC   PLINED+10(6),=CL6'Start='             START ADDRESS              
         GOTO1 VHEXOUT,DMCB,DSPECB,PLINE+17,4,0                                 
         MVC   PLINED+25(6),=CL6'Width='             WIDTH                      
         GOTO1 VHEXOUT,DMCB,DSPTWIDE,PLINE+32,2,0                               
         MVC   PLINED+37(6),=CL6'End-1='             END ADDRESS                
         GOTO1 VHEXOUT,DMCB,DSPTEND,PLINE+44,4,0                                
         MVC   PLINED+54(12),=CL12'Num Entries='     ENTRY COUNT                
         GOTO1 VHEXOUT,DMCB,HDRNUM,PLINE+67,4,0                                 
*                                                                               
         BAS   RE,PRINTL                                                        
         LA    R3,HDRTABL(R3)      NEXT TABLE ENTRY                             
         LA    R2,L'DSPHDR(,R2)    NEXT HEADER                                  
         B     RES04                                                            
*                                                                               
RES06    BAS   RE,PRINTL           SPACE LINE                                   
*                                                                               
         L     R2,AHEADER          TEST IN CORE                                 
         USING FATABSD,R2                                                       
         L     R2,TABSDUMP                                                      
         LHI   R0,256              OUTPUT DUMP TABLE                            
RES08    GOTO1 VHEXOUT,DMCB,(R2),MYWORK,16,0                                    
         MVC   PLINED+00+0(8),MYWORK+0                                          
         MVC   PLINED+08+1(8),MYWORK+8                                          
         MVC   PLINED+16+2(8),MYWORK+16                                         
         MVC   PLINED+24+3(8),MYWORK+24                                         
         MVC   PLINED+32+4(16),0(R2)                                            
         BAS   RE,PRINTL                                                        
         LA    R2,16(R2)                                                        
         BCT   R0,RES08                                                         
         B     EXITOK                                                           
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINTER                                                  *         
***********************************************************************         
         SPACE 1                                                                
PRINTI   NTR1  ,                                                                
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'1'                                                       
         ZAP   PAGE,=P'1'                                                       
         PUT   SYSPRINT,TITLE      PRINT TITLES                                 
         PUT   SYSPRINT,TITLEU                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRINTL   NTR1  ,                                                                
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLES                                 
         PUT   SYSPRINT,TITLEU                                                  
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXITOK              EXIT                                         
         SPACE 2                                                                
***********************************************************************         
* CLOSE PRINTER                                                       *         
***********************************************************************         
         SPACE 1                                                                
PRINTX   NTR1  ,                                                                
         CLOSE SYSPRINT                                                         
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET UP AREA FOR WRITING DUMPS                                       *         
***********************************************************************         
         SPACE 1                                                                
SETDMPS  NTR1  ,                                                                
         LAM   R2,R2,DMALET                                                     
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSDUMP                                                   
         USING TORDUMPD,R2                                                      
*                                                                               
         L     R3,=V(FACIDTAB)                                                  
         USING FACITABD,R3                                                      
*                                                                               
         LHI   RF,1                FIRST SLOT IS ERROR SLOT                     
         MVC   TDSYSNA,FACISN4     SET SYSTEM NAME (????)                       
         STCM  RF,15,TDDUMPFS      IT ONLY HAS 1 DUMP - ALWAYS DUMP #1          
         STCM  RF,15,TDDUMPNO                                                   
         STCM  RF,15,TDDUMPMX                                                   
         LHI   RF,2                DUMP WRITING STARTS AT DUMP #2               
         ST    RF,CTDMPA                                                        
         ST    RF,CTDMPR                                                        
         ST    RF,CTDMPT                                                        
         AHI   R2,TDLENQ           NEXT IN DATASPACE                            
         AHI   R3,L'FACIDTAB       NEXT IN TABLE                                
*                                                                               
SDMP02   CLI   0(R3),X'FF'         END OF TABLE REACHED?                        
         BE    SDMPX               YES                                          
         MVC   TDSYSNA,FACISN4     SET SYSTEM NAME                              
*                                                                               
         TM    FACIFL,FACITST      TEST SYSTEM?                                 
         BZ    SDMP04              NO                                           
         L     RF,CTDMPT                                                        
         STCM  RF,15,TDDUMPFS      SET FIRST NUMBER                             
         STCM  RF,15,TDDUMPNO      SET DEFAULT CURRENT NUMBER TO FIRST          
         A     RF,NMDMPT                                                        
*&&US*&& CLC   =C'TST',FACISN4     US ONLY - TST GETS 3X DUMPS OF               
*&&US*&& BNE   *+12                          OTHER SYSTEMS                      
*&&US*&& A     RF,NMDMPT                                                        
*&&US*&& A     RF,NMDMPT                                                        
         BCTR  RF,0                                                             
         STCM  RF,15,TDDUMPMX                                                   
         AHI   RF,1                                                             
         ST    RF,CTDMPT           SET CURRENT HIGH WATER                       
         B     SDMP10                                                           
*                                                                               
SDMP04   TM    FACIFL,FACIREP      REP SYSTEM?                                  
         BZ    SDMP06              NO                                           
         L     RF,CTDMPR                                                        
         STCM  RF,15,TDDUMPFS      SET FIRST NUMBER                             
         STCM  RF,15,TDDUMPNO      SET DEFAULT CURRENT NUMBER TO FIRST          
         A     RF,NMDMPR                                                        
         BCTR  RF,0                                                             
         STCM  RF,15,TDDUMPMX                                                   
         AHI   RF,1                                                             
         ST    RF,CTDMPR           SET CURRENT HIGH WATER                       
         B     SDMP10                                                           
*                                                                               
SDMP06   CLC   =CL4'DARE',FACISN4                                               
         BNE   SDMP08                                                           
         LHI   RF,1                DARE DUMPS INTO FIRST SLOT ALSO              
         STCM  RF,15,TDDUMPFS                                                   
         STCM  RF,15,TDDUMPNO                                                   
         STCM  RF,15,TDDUMPMX                                                   
         B     SDMP10                                                           
*                                                                               
SDMP08   L     RF,CTDMPA                                                        
         STCM  RF,15,TDDUMPFS      SET FIRST NUMBER                             
         STCM  RF,15,TDDUMPNO      SET DEFAULT CURRENT NUMBER TO FIRST          
         A     RF,NMDMPA                                                        
         BCTR  RF,0                                                             
         STCM  RF,15,TDDUMPMX                                                   
         AHI   RF,1                                                             
         ST    RF,CTDMPA           SET CURRENT HIGH WATER                       
         B     SDMP10                                                           
*                                                                               
SDMP10   AHI   R2,TDLENQ           NEXT IN DATASPACE                            
         AHI   R3,L'FACIDTAB       NEXT IN TABLE                                
         B     SDMP02                                                           
*                                                                               
SDMPX    SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         J     EXITOK                                                           
         DROP  R2,R3                                                            
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
EXIT     XIT1                                                                   
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* PUT A MESSAGE TO THE OPERATOR CONSOLE AND OPTIONALLY GET A REPLY.   *         
* ON ENTRY R1=MESSAGE NUMBER AND XTRAMESS CAN OPTIONALLY CONTAIN 8    *         
* CHARACTERS TO BE INSERTED INTO MESSAGE.                             *         
***********************************************************************         
         SPACE 1                                                                
PUTMESS  NTR1  ,                                                                
         AHI   R1,-1                                                            
         BM    PUTMESS1            R1=ZERO MEANS R8=A(MESSAGE)                  
*                                                                               
         XR    RE,RE                                                            
         LA    RF,L'MESSTAB                                                     
         MR    RE,R1                                                            
         L     R8,=A(MESSTAB)                                                   
         AR    R8,RF               R8=A(MESSTAB ENTRY)                          
*                                                                               
PUTMESS1 MVC   PLINE(L'MESSTAB),0(R8)                                           
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         ORG   DSPACE+((((*-DSPACE)/16)+1)*16)                                  
         SPACE 1                                                                
CARDTAB  DC    CL8'MODE    ',F'001',F'0000010'                                  
         DC    AL1(03,CTCHR,L'MODE,0),AL4(MODE)                                 
         DC    CL8'TABS    ',F'001',F'0000012'                                  
         DC    AL1(03,CTCHR,L'TABSDSPC,0),AL4(TABSDSPC)                         
         DC    CL8'DMGR    ',F'001',F'0000012'                                  
         DC    AL1(03,CTCHR,L'DMGRDSPC,0),AL4(DMGRDSPC)                         
CARDTABX DC    AL1(CARDEOT)                                                     
         SPACE 2                                                                
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
DSPACE   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
VHEXOUT  DC    V(HEXOUT)                                                        
VCARDS   DC    V(CARDS)                                                         
VUNSCAN  DC    V(UNSCAN)                                                        
*                                                                               
ARZERO   DC    16F'0'                                                           
RMAXRES  EQU   256                 INCREASE IF MORE THAN 256 TABLES             
*                                                                               
FREE     DC    CL6'FREE'                                                        
LOCKED   DC    CL6'LOCKED'                                                      
TYPE     DC    CL10'TYPE      '                                                 
NORMAL   DC    CL10'NORMAL    '                                                 
LONG     DC    CL10'LONG      '                                                 
IOWAIT   DC    CL10'I/O WAIT  '                                                 
ONLINE   DC    CL10'ONLINE    '                                                 
OFFLINE  DC    CL10'OFFLINE   '                                                 
JOBNUM   DC    CL10'JOBNUM    '                                                 
FACPAK   DC    CL10'FACPAK    '                                                 
TASK     DC    CL10'TASK      '                                                 
POSTED   DC    CL10'POSTED    '                                                 
*                                                                               
QUERY    DC    32C'?'                                                           
SPACES   DC    CL166' '                                                         
MAXLINE  DC    PL3'60'                                                          
         SPACE 2                                                                
CTITLE   DC    C'Input Card Details'                                            
CTITLEU  DC    C'------------------'                                            
TTITLE   DC    C'TABS dataspace lock status report'                             
TTITLEU  DC    C'---------------------------------'                             
DTITLE   DC    C'DMGR dataspace lock status report'                             
DTITLEU  DC    C'---------------------------------'                             
         SPACE 2                                                                
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
         SPACE 2                                                                
       ++INCLUDE FACIDTAB                                                       
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
MODE     DC    CL10'REPORT'                                                     
DMGRDSPC DC    CL12' '                                                          
TABSDSPC DC    CL12' '                                                          
         EJECT                                                                  
***********************************************************************         
* MESSAGE TABLE ENTRIES                                               *         
***********************************************************************         
         SPACE 1                                                                
MESSTAB  DS    0CL50                                                            
MESS1    DC    CL50'UNABLE TO FIND DMGR DATASPACE - CHECK PARAMETERS'           
MESS2    DC    CL50'UNABLE TO FIND TABS DATASPACE - CHECK PARAMETERS'           
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE DC                                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
BUFFER   DS    60000C                                                           
         DS    0D                                                               
WORKAREA DC    60000X'00'                                                       
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                              *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
RESNUM   DS    H                   RESOURCE NUMBER                              
*                                                                               
NDMPS    DS    F                   NUMBER OF DUMPS PER SYSTEM                   
RDMPS    DS    F                   REMAINDER                                    
*                                                                               
DMCB     DS    6F                                                               
CARDEND  DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
GETLEN   DS    F                   LENGTH RETURNED IF GETMAIN                   
ADSDATA  DS    A                   ADDRESS OF DS BLOCK                          
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
WAITER   DS    A                                                                
*                                                                               
DMOFFS   DS    A                   DMGR DATASPACE OFFSET                        
DMALET   DS    A                   DMGR DATASPACE ALET                          
DMTOKN   DS    CL8                 DMGR DATASPACE STOKEN                        
*                                                                               
TBOFFS   DS    A                   TABS DATASPACE OFFSET                        
TBALET   DS    A                   TABS DATASPACE ALET                          
TBTOKN   DS    CL8                 TABS DATASPACE TOKEN                         
*                                                                               
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
*                                                                               
PLINE    DS    0CL166                                                           
         DS    XL1                                                              
PLINED   DS    CL165                                                            
TITLE    DS    0CL166                                                           
         DS    XL1                                                              
TITLED   DS    CL165                                                            
TITLEU   DS    0CL166                                                           
         DS    XL1                                                              
TITLEDU  DS    CL165                                                            
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
UNBLOCK  DS    32XL20                                                           
UNBLOCKL EQU   *-UNBLOCK                                                        
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
         TITLE 'VARIABLE SCAN MODULE'                                           
SCANNER  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SWORKX-SWORKD,**SCAN**                                           
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         SR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=H'42'         PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=H'20'                                                    
         MVC   LBOTH,=H'30'                                                     
         CLI   0(R9),C'C'                                                       
         BE    SCAN1                                                            
*                                                                               
SCAN1    SH    R2,=H'8'                                                         
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    ERROR2                                                           
         LA    R5,79(R2)                                                        
         SPACE 2                                                                
SCAN2    CLI   0(R5),C' '                                                       
         BNE   SCAN4                                                            
         BCTR  R5,0                                                             
         BCT   R4,SCAN2                                                         
         SPACE 2                                                                
SCAN4    LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         SR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
*HANDLE LINES OF DATA                                                           
*                                                                               
SCAN6    XC    0(12,R3),0(R3)      PRESET A LINE                                
         LH    RF,LBOTH                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
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
         BH    ERROR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    ERROR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN8                                                            
         CLI   0(R3),10                                                         
         BH    ERROR                                                            
         SPACE 2                                                                
SCAN8    SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN10                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN10                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN10                                                           
         ST    R8,4(R3)                                                         
         SPACE 2                                                                
SCAN10   LA    R2,2(R2,R7)                                                      
         IC    R7,1(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN20                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN12                                                           
         ST    R8,8(R3)                                                         
         SPACE 2                                                                
SCAN12   LA    R2,2(R2,R7)                                                      
         B     SCAN20                                                           
         SPACE 2                                                                
VARPAK   PACK  SDUB,0(0,R2)                                                     
         SPACE 2                                                                
SCAN18   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   ERROR                                                            
         SPACE 2                                                                
SCAN20   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    OK                                                               
         IC    R7,MAXLINES                                                      
         LTR   R7,R7                                                            
         BZ    SCAN6                                                            
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN6                                                            
         SPACE 2                                                                
OK       MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     XIT                                                              
         SPACE 2                                                                
ERROR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     XIT                                                              
         SPACE 2                                                                
ERROR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
         SPACE 2                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
*VALIDATE AND GET LENGTHS                                                       
*                                                                               
GETL     NTR1                                                                   
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL2                                                            
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
         SPACE 2                                                                
GETL2    CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL12                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL14                                                           
         SPACE 2                                                                
GETL3    LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL4                                                            
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL10                                                           
         SPACE 2                                                                
GETL4    NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL6                                                            
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL10                                                           
         SPACE 2                                                                
GETL6    CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL8                                                            
         MVI   2(R4),0                                                          
         B     GETL10                                                           
         SPACE 2                                                                
GETL8    CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL10                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
         SPACE 2                                                                
GETL10   LA    R2,1(R2)                                                         
         B     GETL2                                                            
         SPACE 2                                                                
GETL12   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     XIT                                                              
         SPACE 2                                                                
GETL14   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL3               TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL16                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
         SPACE 2                                                                
GETL16   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL2                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
COMMA    DC    C','                                                             
EQUAL    DC    C'='                                                             
SSPACES  DC    CL80' '                                                          
         SPACE 2                                                                
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
       ++INCLUDE DDLOCKTLK                                                      
SWORKX   DS    0C                                                               
         SPACE 1                                                                
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 2                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
* FATABSDMP                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDMP                                                      
         PRINT ON                                                               
* FATABSTMS                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSTMS                                                      
         PRINT ON                                                               
* FATABSPQ                                                                      
         PRINT OFF                                                              
       ++INCLUDE FATABSPQ                                                       
         PRINT ON                                                               
* FATABSZIP                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSZIP                                                      
         PRINT ON                                                               
* FATABSRUN                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSRUN                                                      
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* FASCTTAB                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASCTTAB                                                       
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* DDASSISTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDASSISTD                                                      
         PRINT ON                                                               
* FADARETABD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDAR                                                      
         PRINT ON                                                               
* DDBSPARA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021DDSDSPACE 05/01/02'                                      
         END                                                                    
