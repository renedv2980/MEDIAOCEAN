*          DATA SET CTLFM01    AT LEVEL 035 AS OF 05/01/02                      
*PHASE TA0201A,*                                                                
*INCLUDE SCINKEY                                                                
         TITLE 'CTLFM01 - CONTROL FILE MAINT - ID RECORDS'                      
CTLFM01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**LFM1**,RA                                            
         USING WRKD,RC             RC=A(TEMP W/S)                               
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTIREC,R4           R4=A(IO)                                     
         EJECT                                                                  
* VALIDATE KEY FIELDS AND BUILD KEY                                             
*                                                                               
KEYVAL   XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
*                                   USER-ID CAN EITHER BE ALPH OR NUM           
         GOTO1 AFVAL,IDSIDH                                                     
         BZ    EXIT                                                             
         TM    FLDH+4,X'08'                                                     
         BO    KEYV2                                                            
         CLI   FLDH+5,3                                                         
         BL    EFTS                                                             
         MVC   CTIKID,FLD                                                       
         B     KEYV8                                                            
*                                                                               
KEYV2    OC    FLDH(4),FLDH        BUILD KEY OF NUMERIC REC & READ              
         BZ    EIIF                                                             
         OC    FLDH(2),FLDH                                                     
         BNZ   EFTB                                                             
         MVC   CTIKID+8(2),FLDH+2                                               
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         LA    R5,CTIDATA                                                       
         SR    R6,R6                                                            
*                                  FIND PASSIVE (ALPHA) POINTER                 
KEYV4    CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'02'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     KEYV4                                                            
         MVC   IDSID,2(R5)                                                      
         MVI   IDSIDH+5,8                                                       
         NI    IDSIDH+4,X'F7'                                                   
         OI    IDSIDH+6,X'80'                                                   
         B     KEYVAL                                                           
         EJECT                                                                  
* VALIDATE SYSTEM                                                               
*                                                                               
KEYV8    MVI   SYSTEM,0                                                         
         GOTO1 AFVAL,IDSSYSH                                                    
         BZ    KEYVE                                                            
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         L     R5,ASYSTBL          R5=A(VALID SYSTEMS TABLE)                    
         USING SYSLSTD,R5                                                       
*                                                                               
KEYVC    CLI   SYSLNUM,0           END OF LIST ?                                
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),SYSLNAME                                                  
         BE    *+12                                                             
         LA    R5,SYSLLEN(R5)                                                   
         B     KEYVC                                                            
         MVC   SYSTEM,SYSLNUM      SET SYSTEM NUMBER FROM LIST                  
         MVC   SFLAGS,SYSLIND1     AND SET INPUT FLAGS                          
         CLI   SYSLRPLT,C'C'       NO AGENCY BINARY FOR CONTROL SYSTEM          
         BNE   *+8                                                              
         MVI   SFLAGS,0                                                         
         MVC   IDSSYS(7),SYSLNAME  DISPLAY FULL SYSTEM NAME                     
         OI    IDSSYSH+6,X'80'                                                  
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE THIS/LAST ACTIONS                                                    
*                                                                               
KEYVE    MVC   KEY,CTIKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,IDSIDH                                                        
         ST    R1,FADR                                                          
         CLI   ACTN,CHANGE                                                      
         BNE   KEYVG                                                            
         MVI   ACTN,DISPLAY                                                     
         CLC   KEY,LKEY            KEY AND SYSTEM MUST NOT CHANGE               
         BNE   KEYVG                                                            
         CLC   SYSTEM,LSYSTEM                                                   
         BNE   KEYVG                                                            
         MVI   ACTN,CHANGE         IF KEYS ARE THE SAME RESET ACTION            
*                                                                               
KEYVG    CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         CLI   ACTN,COPY                                                        
         BNE   *+8                                                              
         MVI   ACTN,ADD            COPY TREATED AS AN ADD                       
         CLI   ACTN,RENAME                                                      
         BNE   *+8                                                              
         MVI   ACTN,ADD            AND SO IS RENAME                             
         TM    DMCB+8,X'10'                                                     
         BZ    *+16                                                             
         CLI   ACTN,ADD            N/F ONLY VALID FOR ADD                       
         BE    DATAVAL                                                          
         B     ERNF                                                             
         CLI   ACTN,ADD            F NOT VALID FOR ADD                          
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        DELETED REC CAN ONLY BE DISPLAYED            
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
* DISPLAY BASIC ID RECORD AND OPTIONALLY ONE SYSTEM                             
* ELEMENT (IF SYSTEM N/Z)                                                       
*                                                                               
DISPREC  TWAXC IDSAGYAH            CLEAR UNPROTS IN TWA                         
         XC    IDSSEAG,IDSSEAG     CLEAR SENAME/AGENCY# PROT. FIELD             
         OI    IDSSEAGH+6,X'80'                                                 
         MVI   IDCNT,0             SET ID DISPLAY COUNT                         
         MVI   EXCNT,0             SET PRGM EXCEPTION COUNT                     
         CLI   SYSTEM,0                                                         
         BE    *+8                                                              
         BAS   RE,GETSE                                                         
         MVC   IDSAGYA,=C'??'      FUDGE FOR CONVERSION PROBLEM                 
         XC    ASYSEL,ASYSEL       SET A(SYSTEM ELEMENT)                        
         LA    R5,CTIDATA                                                       
*                                                                               
DISP2    CLI   0(R5),0                                                          
         BE    DISPEND                                                          
         CLI   0(R5),X'03'         PRINCIPAL ID                                 
         BE    DISPPID                                                          
         CLI   0(R5),X'20'         ID                                           
         BE    DISPID                                                           
         CLI   0(R5),X'23'         PROGRAM EXCEPTION                            
         BE    DISPEX                                                           
         CLI   0(R5),X'21'         SYSTEM                                       
         BE    DISPSS                                                           
         CLI   0(R5),X'07'         ID OPTIONS ELEMENT                           
         BE    DISPOPTS                                                         
         CLI   0(R5),X'06'         AGENCY ID                                    
         BE    DISPAGY                                                          
*                                                                               
DISP3    CLI   0(R5),X'02'                                                      
         BNE   DISP4                                                            
         MVC   IDNUM,2(R5)                                                      
         BAS   RE,DISPIDN                                                       
*                                                                               
DISP4    SR    R6,R6               BUMP TO NEXT ELEMENT                         
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DISP2                                                            
         EJECT                                                                  
* ADD AN ID ELEMENT TO ID BLOCK (BLOCK1)                                        
*                                                                               
DISPID   SR    R1,R1                                                            
         IC    R1,IDCNT            BUMP BLOCK COUNT                             
         LR    R6,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,IDCNT                                                         
         MH    R6,=H'20'                                                        
         LA    R6,BLOCK1(R6)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R6),C' '          AND CLEAR IT                                 
         MVC   1(19,R6),0(R6)                                                   
         IC    R1,1(R5)                                                         
         OC    2(2,R5),2(R5)                                                    
         BNZ   *+10                                                             
         MVC   2(2,R5),=C'L='                                                   
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),2(R5)       MOVE IN ID                                   
         B     DISP4                                                            
         EJECT                                                                  
* ADD A PROGRAM EXCEPTION ELEMENT TO BLOCK (BLOCK2)                             
*                                                                               
DISPEX   DS    0H                                                               
         USING CTPRGD,R5                                                        
         MVC   PGNAME(4),CTPRGRAM  IF NOT IN SYSTEM MODE DISPLAY TSPP           
         CLI   SYSTEM,0                                                         
         BE    DISPEX2                                                          
         MVC   DUB(4),CTPRGRAM                                                  
         MVI   DUB,C'0'                                                         
         GOTO1 VHEXIN,DMCB,DUB,DUB+4,4                                          
         CLC   SYSTEM,DUB+4        EXCEPTION FOR THIS SYSTEM                    
         BNE   DISP4                                                            
         MVC   PROGRAM,DUB+5       YES - GET PROGRAM NAME                       
         BAS   RE,GETPRGN                                                       
         CLI   PROGRAM,X'FF'                                                    
         BE    DISP4                                                            
*                                                                               
DISPEX2  SR    R1,R1                                                            
         IC    R1,EXCNT            BUMP BLOCK COUNT                             
         LR    R6,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,EXCNT                                                         
         MH    R6,=H'20'                                                        
         LA    R6,BLOCK2(R6)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R6),C' '          AND CLEAR                                    
         MVC   1(19,R6),0(R6)                                                   
         MVC   0(4,R6),PGNAME      FIRST HALF IS NAME (TSPP OR XXXX)            
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNE   *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C'='                                                       
         MVC   2(1,R6),CTPRGTST    MOVE TEST LEVEL                              
         B     DISP4                                                            
         DROP  R5                                                               
         EJECT                                                                  
* SAVE A(SYSTEM ELEMENT) IF IT'S THE ONE REQUESTED                              
*                                                                               
DISPSS   CLI   SYSTEM,0                                                         
         BE    DISP4                                                            
         USING CTSYSD,R5                                                        
         CLC   CTSYSNUM,SYSTEM                                                  
         BNE   DISP4                                                            
         ST    R5,ASYSEL                                                        
         B     DISP4                                                            
         DROP  R5                                                               
         SPACE 1                                                                
* DISPLAY ID NUMBER                                                             
*                                                                               
DISPIDN  NTR1                                                                   
         XC    IDSIDN,IDSIDN                                                    
         OI    IDSIDNH+6,X'80'                                                  
         MVC   IDSIDN(10),=C'ID NUMBER='                                        
         LA    R4,IDSIDN+10                                                     
         EDIT  (B2,IDNUM),(4,0(R4)),ALIGN=LEFT                                  
         B     EXIT                                                             
         SPACE 1                                                                
* DISPLAY PRINCIPAL ID                                                          
*                                                                               
DISPPID  NTR1                                                                   
         EDIT  (B2,2(R5)),(5,IDSPID),FILL=0                                     
         LA    R4,IOAREA2                                                       
         ST    R4,AREC                                                          
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),2(R5)                                                
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD               READ ID RECORD                               
         LA    R1,CTIDATA                                                       
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   KEY,KEYNEXT                                                      
         CLI   DMCB+8,0            CHECK RECORD FOUND/OK                        
         BNE   EXIT                                                             
         SR    RE,RE                                                            
DISPPID2 CLI   0(R1),0             FIND POINTER ELEMENT AND DISPLAY ID          
         BE    EXIT                                                             
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     DISPPID2                                                         
         MVC   IDSPID,2(R1)                                                     
         B     EXIT                                                             
         SPACE 1                                                                
* DISPLAY ID OPTIONS                                                            
*                                                                               
DISPOPTS MVI   IDSACCR,C'N'        ACCESS REQD                                  
         TM    2(R5),X'80'                                                      
         BZ    *+8                                                              
         MVI   IDSACCR,C'Y'                                                     
         TM    2(R5),X'40'         GENERIC USER-ID (FOR SHUTTLE DRIVER)         
         BZ    *+10                                                             
         MVC   IDSPID(7),=C'GENERIC'                                            
         B     DISP4                                                            
         SPACE 1                                                                
* DISPLAY AGENCY ALPHA AND LANGUAGE                                             
*                                                                               
         USING CTAGYD,R5                                                        
DISPAGY  MVC   IDSAGYA,CTAGYID                                                  
         GOTO1 GETACC,CTAGYID      READ ACCESS RECORD AND SAVE                  
         BE    *+6                   COUNTRY CODE                               
         DC    H'00'                                                            
         L     R1,VLANG                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING LANGTABD,R1         R1=A(LANGUAGE TABLE)                         
         CLC   CTAGYLNG,LANGCODE   MATCH CODE TO TABLE                          
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   IDSLANG,LANGFUL                                                  
         B     DISP4                                                            
         DROP  R1,R5                                                            
         EJECT                                                                  
* PUT BLOCKS TO TWA                                                             
*                                                                               
DISPEND  CLI   IDCNT,0             VALID ID'S                                   
         BE    DISPEND2                                                         
         ZIC   R0,IDCNT                                                         
         GOTO1 =V(SCINKEY),DMCB,(3,IDSID1H),(20,BLOCK1),(R0),RR=RB              
*                                                                               
DISPEND2 CLI   EXCNT,0             PROGRAM EXCEPTIONS                           
         BE    DISPEND4                                                         
         ZIC   R0,EXCNT                                                         
         GOTO1 =V(SCINKEY),DMCB,(2,IDSPE1H),(20,BLOCK2),(R0),RR=RB              
*                                                                               
DISPEND4 OC    ASYSEL,ASYSEL       PROGRAM ACCESS                               
         BZ    DISPEND6                                                         
         BAS   RE,DISPSYS                                                       
         ZIC   R0,PGCNT                                                         
         GOTO1 =V(SCINKEY),DMCB,(3,IDSPA1H),(20,BLOCK1),(R0),RR=RB              
*                                                                               
DISPEND6 MVC   LSYSTEM,SYSTEM                                                   
         TM    CTISTAT,X'80'       OK TO RESTORE IF DELETED                     
         BZ    *+12                                                             
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
         LA    R1,IDSAGYAH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKDEL+OKCHA+OKCOPY                                         
         B     EXIT                                                             
         EJECT                                                                  
* BUILD PROGRAM ACCESS LIST INTO BLOCK1 AND SET PGCNT TO NUMBER OF              
* ENTRIES IN BLOCK.                                                             
*                                                                               
DISPSYS  NTR1                                                                   
         L     R5,ASYSEL           A(PGMS) INTO APGM                            
         USING CTSYSEL,R5                                                       
         MVC   DUB(1),CTSYSSE                                                   
         BAS   RE,GETSE4           GET A(SELIST ENTRY) INTO ASE AND             
         LA    R6,CTSYSPGM         R6=A(AUTHS)                                  
         ZIC   R7,CTSYSLEN         R7=LENGTH                                    
         MVI   PGCNT,0             SET COUNT                                    
         CLI   CTSYSLEN,16         CHECK FOR ALL=VALUE ONLY                     
         BE    DISPSYS8                                                         
*                                                                               
DISPSYS2 CH    R7,=H'16'                                                        
         BNH   DISPSYS8                                                         
         MVC   PROGRAM,0(R6)                                                    
         LA    R6,1(R6)            POINT TO THE AUTHORIZATION CODE              
         BAS   RE,GETPGAN          GET PROGRAM NAME                             
         CLI   PROGRAM,X'FF'       SET TO X'FF' IF N/F                          
         BE    DISPSYS6                                                         
*                                                                               
DISPSYS4 SR    R1,R1                                                            
         IC    R1,PGCNT            BUMP BLOCK COUNT                             
         LA    RE,1(R1)                                                         
         STC   RE,PGCNT                                                         
         MH    R1,=H'20'                                                        
         LA    R1,BLOCK1(R1)       GET A(BLOCK ENTRY)                           
         MVI   0(R1),C' '                                                       
         MVC   1(19,R1),0(R1)                                                   
         LR    R8,R1                                                            
         MVC   0(4,R8),PGNAME                                                   
         LA    R8,4(R8)                                                         
         CLI   0(R8),C' '                                                       
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         MVI   1(R8),C'='          AUTH IS Y N OR XXXX                          
         MVI   2(R8),C'Y'          AUTH IS Y N OR XXXX                          
         CLC   0(2,R6),YAUTH                                                    
         BE    DISPSYS6                                                         
         MVI   2(R8),C'N'                                                       
         CLC   0(2,R6),NAUTH                                                    
         BE    DISPSYS6                                                         
         GOTO1 VHEXOUT,DMCB,(R6),2(R8),2,=C'TOG'                                
*                                                                               
DISPSYS6 LA    R8,CTSYSALL         EXIT IF ALL=VALUE JUST DONE                  
         CR    R8,R6                                                            
         BE    DISPSYSA                                                         
         LA    R6,2(R6)            NEXT PROGRAM                                 
         SH    R7,=H'3'                                                         
         B     DISPSYS2                                                         
*                                                                               
DISPSYS8 LA    R6,CTSYSALL                                                      
         MVC   PGNAME,=CL8'ALL'                                                 
         B     DISPSYS4                                                         
*                                  DISPLAY REST OF ELEMENT                      
DISPSYSA MVC   AGNUM,CTSYSAGB                                                   
         BAS   RE,DISPSEAG         DISPLAY SENAME/AGENCY BINARY#                
*                                                                               
DISPSPWD TM    CTSYSIND,CTSYSIYP                                                
         BZ    DSPWD10                                                          
         MVI   IDSSPWD,C'Y'                                                     
         B     DSPWDX                                                           
DSPWD10  TM    CTSYSIND,CTSYSINP                                                
         BZ    DSPWDX                                                           
         MVI   IDSSPWD,C'N'                                                     
         B     DSPWDX                                                           
DSPWDX   EQU   *                                                                
*                                                                               
DISPSYSF OC    CTSYSLMT,CTSYSLMT                                                
         BZ    DISPSYSK                                                         
         MVC   IDSACCS(L'CTSYSLMT),CTSYSLMT                                     
         MVI   IDSACCSH+7,L'CTSYSLMT                                            
*                                                                               
         CLI   CTSYSNUM,9          MEDIABASE                                    
         BNE   DISPSYF4                                                         
         GOTO1 VHEXOUT,DMCB,CTSYSLMT,IDSACCS,4,=C'N'                            
*                                                                               
DISPSYF4 DS    0H                                                               
*                                                                               
*&&UK                                                                           
         CLI   CTSYSNUM,4          TEST UK/MEDIA                                
         BNE   DISPSYSJ                                                         
         OC    CTSYSLMT,CTSYSLMT                                                
         BZ    DISPSYSK                                                         
         LA    RE,IDSACCS                                                       
         LA    R5,CTSYSLMT                                                      
         LA    R7,4                                                             
DISPSYSG SR    R0,R0                255,255,99,99 MAX VALUES                    
         IC    R0,0(R5)                                                         
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    RE,R0     '                                                      
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         LA    R5,1(R5)                                                         
         BCT   R7,DISPSYSG                                                      
         BCTR  RE,0                                                             
         MVI   0(RE),C' '                                                       
         B     DISPSYSK                                                         
*&&                                                                             
*&&US                                                                           
         CLI   CTSYSNUM,3          TEST NETWORK                                 
         BE    *+12                                                             
         CLI   CTSYSNUM,2          TEST SPOT                                    
         BNE   DISPSYSI                                                         
*                                                                               
         MVC   DMCB+4(4),=X'D9000A15'  CLUNPK                                   
         L     RE,AFACLIST                                                      
         USING COMFACSD,RE                                                      
         L     RF,CCALLOV                                                       
         DROP  RE                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),CTSYSLMT,IDSACCS                                       
*                                                                               
DISPSYSI CLI   CTSYSNUM,6          TEST ACC                                     
         BNE   DISPSYSJ                                                         
         CLC   CTSYSLMT+3(3),=C'TAL'                                            
         BE    *+14                                                             
         CLC   CTSYSLMT+4(2),=C'TL'                                             
         BNE   DISPSYSK                                                         
         MVC   IDSACCS(2),=C'T='   FORMAT T=ULCC (DPS TALENT)                   
         MVC   IDSACCS+2(2),CTSYSLMT                                            
         LA    RE,CTSYSLMT+1       UNIT T HEXOUT PARAMS                         
         LA    RF,IDSACCS+3                                                     
         LA    R0,2                                                             
         CLI   IDSACCS+2,C'T'                                                   
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         LA    R0,1                                                             
         STM   RE,R0,DMCB                                                       
         GOTO1 VHEXOUT,DMCB                                                     
         CLC   CTSYSLMT+3(3),=C'TAL' OLD FORMAT                                 
         BE    DISPSYSK                                                         
         CLI   CTSYSLMT+3,C' '     IS OFFICE PRESENT                            
         BE    DISPSYSK                                                         
         LA    RF,IDSACCS+6                                                     
         CLI   IDSACCS+2,C'T'                                                   
         BNE   *+8                                                              
         LA    RF,1(RF)            FOR UNIT T STRING IS LONGER                  
         MVI   0(RF),C'*'                                                       
         MVC   1(1,RF),CTSYSLMT+3                                               
         TM    1(RF),X'40'                                                      
         BO    *+12                                                             
         OI    1(RF),X'40'                                                      
         MVI   0(RF),C'$'          SET OFF LIST IF BIT WAS OFF                  
         B     DISPSYSK                                                         
*&&                                                                             
DISPSYSJ CLI   CTSYSNUM,14         TEST PERSONNEL                               
         BNE   DISPSYSK                                                         
         XC    IDSACCS,IDSACCS                                                  
         OC    CTSYSLMT+2(2),CTSYSLMT+2                                         
         BZ    DISPSYSK                                                         
         LA    RE,IDSACCS          FORMAT N(NN)-N(NN)                           
         ZIC   R0,CTSYSLMT+2                                                    
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT                                        
         AR    RE,R0                                                            
         MVI   0(RE),C'-'                                                       
         ZIC   R0,CTSYSLMT+3                                                    
         EDIT  (R0),(3,1(RE)),ALIGN=LEFT                                        
*                                                                               
DISPSYSK SR    R0,R0                                                            
         ICM   R0,3,CTSYSPRI                                                    
         EDIT  (R0),(4,IDSPRI),ALIGN=LEFT                                       
         OI    IDSPRI,X'F0'                                                     
*                                                                               
DISPSYSX XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
* CHANGE/ADD ID RECORD                                                          
*                                                                               
DATAVAL  MVC   KEYSAVE,KEY                                                      
         MVC   CTIKEY,KEY                                                       
         XC    IDOPTS,IDOPTS       CLEAR ID OPTIONS                             
         LA    R1,IDSIDH                                                        
         ST    R1,FADR                                                          
         MVC   ACTN,ACTN2                                                       
         CLI   ACTN,COPY                                                        
         BE    *+12                                                             
         CLI   ACTN,RENAME                                                      
         BNE   DATAV2                                                           
* COPY/RENAME FUNCTIONS                                                         
         MVC   KEY,LKEY            RESTORE LAST KEY & READ RECORD               
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0            MUST BE OK                                   
         BNE   EIIO                                                             
         MVI   TEMP,X'01'          DELETE ACTIVITY ELEMENT                      
         GOTO1 ADELEL                                                           
         GOTO1 ABLDACT             BUILD & ADD NEW ACTIVITY ELEMENT             
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   KEY,KEYSAVE         RESTORE NEW KEY                              
         MVC   KEYNEXT,KEY                                                      
         MVC   CTIKEY,KEY                                                       
*                                  MERGE ACCESS SYSTEM DATA                     
         LA    R5,CTIDATA                                                       
         SR    R6,R6                                                            
*                                                                               
DATAV0L  CLI   0(R5),0             LOCATE AGENCY ID RECORD                      
         BE    DATAV0B             DO NOTHING IF N/F                            
         CLI   0(R5),X'06'                                                      
         BE    DATAV0A             ELSE GET ACCESS SYSTEM DATA                  
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DATAV0L                                                          
DATAV0A  LA    R6,CTAGYID-CTAGYD(R5)                                            
         GOTO1 GETACC,(R6)                                                      
         BNE   EXIT                                                             
         BAS   RE,MRGSYS                                                        
*                                                                               
DATAV0B  EQU   *                                                                
         CLI   ACTN,RENAME                                                      
         BNE   DATAV1X                                                          
         GOTO1 AADD                ADD NEW ID WITH OLD XREF NUM                 
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    R5,CTIDATA                                                       
         SR    R6,R6                                                            
*                                  LOCATE XREF NUMBER ELEMENT                   
DATAV1   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R5),X'02'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DATAV1                                                           
         MVC   IDNUM,2(R5)                                                      
         MVI   TEMP,X'02'                                                       
         GOTO1 ADELEL              DELETE XREF ELEMENT                          
         MVC   TEMP(2),=X'020C'                                                 
         MVC   TEMP+2(10),CTIKID                                                
         GOTO1 APUTEL              ADD NEW XREF ALPHA ELEMENT                   
         BZ    EXIT                                                             
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID+8(2),IDNUM                                                
         MVC   KEY,CTIKEY                                                       
         LA    R5,IOAREA2                                                       
         ST    R5,AREC                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         ST    R4,AREC                                                          
         GOTO1 AWRITE              WRITE OLD NUMBER WITH NEW XREF               
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVC   CTIKEY,LKEY         RESTORE COPY FROM KEY                        
         MVC   KEY,LKEY                                                         
*                                                                               
DATAV1X  MVI   TEMP,X'02'          DELETE XREF ELEMENT                          
         GOTO1 ADELEL                                                           
         B     DATAEND             GO TO ADD LOGIC                              
* ADD FUNCTION                                                                  
DATAV2   CLI   ACTN,ADD                                                         
         BNE   DATAV4                                                           
         MVI   TEMP,0              BUILD KEY+LEN                                
         GOTO1 ABLDREC                                                          
         B     DATAV8                                                           
* IF CHANGE - FIND PASSIVE NUMBER ELEMENT ON REC                                
DATAV4   LA    R5,CTIDATA                                                       
         SR    R6,R6                                                            
*                                                                               
DATAV6   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R5),X'02'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DATAV6                                                           
         MVC   IDNUM,2(R5)                                                      
* STRIP DOWN RECORD                                                             
         MVI   TEMP,X'01'          ACTIVITY                                     
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'02'          PASSIVE                                      
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'03'          PRINCIPAL ID ELEMENT                         
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'06'          AGENCY ID                                    
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'20'          ID                                           
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'07'          ID OPTIONS                                   
         GOTO1 ADELEL                                                           
*                                  DELETE PROGRAM EXCEPTION ELEMENTS            
         GOTO1 VHEXOUT,DMCB,SYSTEM,WORK,1,=C'TOG'                               
         MVI   WORK,C'T'                                                        
         GOTO1 ,DMCB,(C'D',=C'CTFILE '),(X'23',(R4)),(2,WORK)                   
         CLI   SYSTEM,0                                                         
         BNE   *+10                                                             
         XC    DMCB+8(4),DMCB+8                                                 
         GOTO1 VHELLO,DMCB                                                      
*                                                                               
DATAV8   GOTO1 ABLDACT             BUILD AND ADD ACTIVITY                       
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
* VALIDATE AND BUILD AGENCY ID ELEMENT                                          
*                                                                               
DATAV9   GOTO1 AFVAL,IDSAGYAH                                                   
         BZ    EXIT                                                             
         CLC   FLD(2),=C'??'                                                    
         BE    EIIF                                                             
         CLI   FLDH+5,2                                                         
         BL    EFTS                                                             
*                                                                               
         XC    SYSEL,SYSEL                                                      
         GOTO1 GETACC,FLD          GET ACCESS RECORD INTO IOAREA3               
         BNE   EXIT                                                             
         BAS   RE,MRGSYS           MERGE ACCESS SYSTEM DATA                     
         CLI   SYSTEM,0                                                         
         BE    DATAV92X                                                         
         LA    R1,IOAREA3          LOCATE SYSTEM ELEMENT ON ACCS RECORD         
         LA    R1,CT5DATA-CT5REC(R1)                                            
         SR    R0,R0                                                            
DATAV92  CLI   0(R1),0             TEST E-O-R                                   
         BNE   *+16                                                             
         LA    R1,IDSSYSH          YES - SYSTEM IS INVALID                      
         ST    R1,FADR                                                          
         B     EIIF                                                             
         CLI   0(R1),X'21'         TEST SYSTEM ELEMENT                          
         BNE   *+14                                                             
         CLC   SYSTEM,CTSYSNUM-CTSYSD(R1)                                       
         BE    DATAV94                                                          
         IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     DATAV92                                                          
DATAV94  MVC   SYSEL,0(R1)                                                      
*                                                                               
DATAV92X XC    TEMP,TEMP                                                        
         LA    R7,TEMP                                                          
         USING CTAGYD,R7                                                        
         MVC   CTAGYEL(2),=X'0606'                                              
         MVC   CTAGYID,FLD                                                      
*                                                                               
         GOTO1 AFVAL,IDSLANGH      VALIDATE LANGUAGE                            
         BZ    EXIT                                                             
         L     R1,VLANG                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING LANGTABD,R1         R1=A(LANGUAGE TABLE)                         
         ZIC   R6,FLDH+5                                                        
         BCTR  R6,0                R2=L'INPUT-1                                 
DATAV95A CLI   FLDH+5,L'LANGSHR                                                 
         BH    DATAV95B                                                         
         EX    R6,*+8                                                           
         BE    DATAV95C                                                         
         CLC   LANGSHR(0),FLD      MATCH ON SHORT CODE                          
DATAV95B EX    R6,*+8                                                           
         BE    DATAV95C                                                         
         CLC   LANGFUL(0),FLD      MATCH ON LONG NAME                           
         BXLE  R1,RE,DATAV95A                                                   
         B     EIIF                                                             
DATAV95C MVC   CTAGYLNG,LANGCODE   SET LANGUAGE CODE                            
         DROP  R1                                                               
         SPACE 1                                                                
         GOTO1 APUTEL              ADD AGENCY ALPHA ELEMENT                     
         SPACE 1                                                                
* VALIDATE AND BUILD PRINCIPAL ID ELEMENT                                       
*                                                                               
DATAV9A  GOTO1 AFVAL,IDSPIDH                                                    
         BZ    DATAV10                                                          
         CLI   FLDH+5,3                                                         
         BL    EFTS                                                             
         CLI   FLDH+5,7                                                         
         BH    DATAV9B                                                          
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8              TEST THIS IS A GENERIC USER-ID               
         B     *+10                                                             
         CLC   FLD(0),=C'GENERIC'                                               
         BNE   DATAV9B                                                          
         OI    IDOPTS,X'40'        YES - SET THIS IS A GENERIC USER-ID          
         B     DATAV10                                                          
DATAV9B  LA    R4,IOAREA2                                                       
         ST    R4,AREC                                                          
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,FLD                                                       
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD               READ ID RECORD                               
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                CHECK RECORD FOUND/OK                        
         LA    R1,CTIDATA                                                       
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   KEY,KEYSAVE                                                      
         SR    RE,RE                                                            
DATAV9C  CLI   0(R1),0             FIND POINTER ELEMENT                         
         BE    EIRT                                                             
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     DATAV9C                                                          
         MVC   TEMP(2),=X'0304'    BUILD AND ADD PRINCIPAL ID ELEMENT           
         MVC   TEMP+2(2),2(R1)                                                  
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         B     DATAV10                                                          
         EJECT                                                                  
* VALIDATE ACCESS REQUIRED                                                      
*                                                                               
DATAV10  GOTO1 AFVAL,IDSACCRH                                                   
         BZ    DATAV10A                                                         
         CLI   FLD,C'N'                                                         
         BE    DATAV10A                                                         
         CLI   FLD,C'Y'                                                         
         BNE   EIIF                                                             
         OI    IDOPTS,X'80'        SET PASSWORD IS REQUIRED FOR ID              
DATAV10A XC    TEMP,TEMP                                                        
         MVC   TEMP(2),=X'0704'                                                 
         MVC   TEMP+2(2),IDOPTS                                                 
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 1                                                                
* VALIDATE AND BUILD ID ELEMENTS                                                
*                                                                               
DATAV11  LA    R7,IDSID1H                                                       
         LA    R8,3                                                             
*                                                                               
DATAV12  TM    1(R7),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         LR    R1,R7                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV20                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCK1)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R6,BLOCK1           R6=A(SCAN BLOCK ENTRY)                       
         MVC   FCNT,4(R1)                                                       
         MVI   FNDX,1                                                           
*                                                                               
DATAV14  CLC   FNDX,FCNT                                                        
         BH    DATAV20                                                          
         CLI   1(R6),0                                                          
         BNE   DATAV16                                                          
         CLI   0(R6),3             VALIDATE USER-ID                             
         BL    EFTS                                                             
         CLI   0(R6),10                                                         
         BH    EFTL                                                             
         LA    R4,IOAREA2          SWITCH I/O AREAS                             
         ST    R4,AREC                                                          
         XC    CTIKEY,CTIKEY       BUILD ID KEY                                 
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,12(R6)                                                    
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD               CHECK RECORD FOUND/OK                        
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         MVC   KEY,KEYSAVE                                                      
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   TEMP(2),=X'200C'    BUILD & ADD ELEMENT                          
         MVC   TEMP+2(10),12(R6)                                                
         B     DATAV18                                                          
*                                                                               
DATAV16  CLI   0(R6),0             VALIDATE LIST-ID                             
         BE    EIIF                                                             
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=C'LIST'   CHECK FOR VALID KEYWORD                      
         BNE   EIIF                                                             
         CLI   1(R6),6                                                          
         BH    EFTL                                                             
         LA    R5,IOAREA2          SWITCH I/O AREAS                             
         ST    R5,AREC                                                          
         USING CTWREC,R5                                                        
         XC    CTWKEY,CTWKEY       BUILD LIST KEY                               
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'I'                                                     
         MVC   CTWKID,22(R6)                                                    
         MVC   KEY,CTWKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                CHECK RECORD FOUND/OK                        
         CLI   DMCB+8,0                                                         
         BNE   EIIF                                                             
         ST    R4,AREC                                                          
         MVC   KEY,KEYSAVE                                                      
         MVC   TEMP(2),=X'200C'    BUILD & ADD ELEMENT                          
         XC    TEMP+2(2),TEMP+2                                                 
         MVC   TEMP+4(8),22(R6)                                                 
*                                  ADD ELEMENT TO ID RECORD                     
DATAV18  GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE '),AREC,TEMP,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         BE    *+12                                                             
         MVI   FERN,68             CHECK RECORD TOO BIG                         
         B     EXIT                                                             
*                                  BUMP TO NEXT FIELD                           
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R6,32(R6)                                                        
         B     DATAV14                                                          
*                                                                               
DATAV20  ZIC   R1,0(R7)            BUMP TO NEXT TWA LINE                        
         AR    R7,R1                                                            
         BCT   R8,DATAV12                                                       
         B     DATAV30                                                          
         EJECT                                                                  
* VALIDATE AND BUILD PROGRAM EXCEPTION ELEMENTS                                 
*                                                                               
DATAV30  LA    R7,IDSPE1H                                                       
         LA    R8,2                                                             
*                                                                               
DATAV32  TM    1(R7),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         LR    R1,R7                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV39                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCK1)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R5,BLOCK1           R5=A(SCAN BLOCK ENTRY)                       
         MVC   FCNT,4(R1)                                                       
         MVI   FNDX,1                                                           
*                                                                               
DATAV34  CLC   FNDX,FCNT                                                        
         BH    DATAV39                                                          
         CLI   0(R5),1             L'PART1                                      
         BL    EMIF                                                             
         CLI   0(R5),7             L'PART1                                      
         BH    EFTL                                                             
         CLI   1(R5),1             L'PART2                                      
         BH    EFTL                                                             
         BL    EIIF                                                             
         CLI   0(R5),4             L'PART1                                      
         BNE   DATAV36                                                          
         CLI   12(R5),C'T'         'T' MEANS ONLINE                             
         BNE   DATAV36                                                          
         GOTO1 VHEXIN,DMCB,13(R5),DUB,3,0                                       
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   DATAV38                                                          
*                                                                               
DATAV36  CLI   SYSTEM,0            CAN'T INPUT PGMNAME IF NOT IN                
         BE    EIIF                SYSTEM MODE                                  
         BAS   RE,GETSE                                                         
         BAS   RE,GETPRGX          GET PROGRAM NUMBER                           
         CLI   PROGRAM,X'FF'                                                    
         BE    EIIF                                                             
         MVC   DUB(1),SYSTEM                                                    
         MVC   DUB+1(1),PROGRAM                                                 
         GOTO1 VHEXOUT,DMCB,DUB,12(R5),2,=C'TOG'                                
         MVI   12(R5),C'T'         NAME CONVERTED TO TSPP                       
*                                                                               
DATAV38  CLI   22(R5),C'A'         TEST LEVEL S/B A,B OR C                      
         BL    EIIF                                                             
         CLI   22(R5),C'C'                                                      
         BH    EIIF                                                             
         XC    TEMP,TEMP           BUILD PROGRAM EXCEPTION ELEMENT              
         LA    R6,TEMP                                                          
         USING CTPRGD,R6                                                        
         MVC   CTPRGEL(2),=X'2307'                                              
         MVC   CTPRGRAM,12(R5)                                                  
         MVC   CTPRGTST,22(R5)                                                  
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
         ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R5,32(R5)                                                        
         B     DATAV34                                                          
*                                                                               
DATAV39  ZIC   R1,0(R7)            BUMP TO NEXT TWA LINE                        
         AR    R7,R1                                                            
         BCT   R8,DATAV32                                                       
         B     DATAV3A                                                          
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE AND BUILD A SYSTEM ELEMENT                                           
*                                                                               
DATAV3A  XC    TEMP,TEMP           BUILD BASIC ELEMENT                          
         LA    R7,TEMP                                                          
         USING CTSYSD,R7                                                        
         MVC   CTSYSEL(2),=X'2110' 16 BYTE HEADER FIRST (VARIABLE LEN)          
         MVC   CTSYSNUM,SYSTEM                                                  
         MVC   CTSYSALL,XAUTH      PRESET ALL & PROGRAM VALUES                  
         MVC   CTSYSPGM,XAUTH                                                   
         MVC   CTSYSPGM+2(126),CTSYSPGM                                         
         SR    R5,R5                                                            
         CLI   SYSTEM,0                                                         
         BE    DATAVSP                                                          
         OC    SYSEL,SYSEL                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CTSYSSE,SYSEL+(CTSYSSE-CTSYSD)                                   
         MVC   CTSYSAGB,SYSEL+(CTSYSAGB-CTSYSD)                                 
         MVC   DUB(1),CTSYSSE                                                   
         BAS   RE,GETSE4                                                        
         MVC   AGNUM,CTSYSAGB                                                   
         BAS   RE,DISPSEAG                                                      
*                                      VALIDATE SYSTEM PASSWORD FLAG            
DATAVSP  GOTO1 AFVAL,IDSSPWDH                                                   
         BZ    DATAV3M                                                          
         CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
         CLI   FLD,C'N'                                                         
         BNE   DVSP10                                                           
         OI    CTSYSIND,CTSYSINP                                                
         B     DVSPX                                                            
DVSP10   CLI   FLD,C'Y'                                                         
         BNE   EIIF                                                             
         OI    CTSYSIND,CTSYSIYP                                                
DVSPX    EQU   *                                                                
*                                  VALIDATE LIMIT ACCESS                        
DATAV3M  GOTO1 AFVAL,IDSACCSH                                                   
         BZ    DATAV3Q                                                          
         CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
         MVC   CTSYSLMT,FLD                                                     
*                                                                               
         CLI   SYSTEM,9            MEDIABASE                                    
         BNE   DATAV3N                                                          
         XC    CTSYSLMT,CTSYSLMT                                                
         CLI   FLDH+5,8            MUST BE LESS THAN 8                          
         BH    EFTL                                                             
         TM    FLDH+5,X'01'        AND MUST BE EVEN                             
         BNZ   EIIF                                                             
         ZIC   R0,FLDH+5           LENGTH                                       
         XC    DUB,DUB                                                          
         GOTO1 VHEXIN,DMCB,FLD,DUB,(R0),0                                       
         OC    DMCB+12(4),DMCB+12                                               
         BZ    EIIF                                                             
         MVC   CTSYSLMT(4),DUB                                                  
*                                                                               
DATAV3N  DS    0H                                                               
         CLI   SYSTEM,14           TEST PERSONNEL SYSTEM                        
         BNE   DATAV3O                                                          
         XC    CTSYSLMT,CTSYSLMT                                                
         GOTO1 VSCANNER,DMCB,FLDH,(2,BLOCK1),C',=*-'                            
         XC    8(4,R1),8(R1)       CLEAR SPECIAL SCAN CHARACTERS                
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         CLI   4(R1),1             FORMAT=N(NN)-N(NN)                           
         BNE   EIIF                                                             
         CLI   BLOCK1,0            VALIDATE LOWER LIMIT                         
         BE    EIIF                                                             
         TM    BLOCK1+2,X'80'                                                   
         BZ    EFNN                                                             
         OC    BLOCK1+4(4),BLOCK1+4                                             
         BZ    EIIF                                                             
         OC    BLOCK1+4(3),BLOCK1+4                                             
         BNZ   EIIF                                                             
         MVC   CTSYSLMT+2(1),BLOCK1+7                                           
         MVC   CTSYSLMT+3(1),BLOCK1+7                                           
         CLI   BLOCK1+1,0          TEST UPPER LIMIT GIVEN                       
         BE    DATAV3Q                                                          
         MVI   FNDX,2                                                           
         TM    BLOCK1+3,X'80'      VALIDATE UPPER LIMIT                         
         BZ    EFNN                                                             
         OC    BLOCK1+8(4),BLOCK1+8                                             
         BZ    EIIF                                                             
         OC    BLOCK1+8(3),BLOCK1+8                                             
         BNZ   EIIF                                                             
         MVC   CTSYSLMT+3(1),BLOCK1+11                                          
         CLC   CTSYSLMT+3(1),CTSYSLMT+2                                         
         BL    EIIF                                                             
         B     DATAV3Q                                                          
DATAV3O  DS    0H                                                               
*&&UK                                                                           
         CLI   SYSTEM,4            TEST UK/MEDIA                                
         BNE   DATAV3Q                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(5,BLOCK1),X'6B5E6BFF'                        
         CLI   4(R1),4             FORMAT=N(NN),N(NN),N(N),N(N)                 
         BNE   EIIF                                                             
         LA    RF,BLOCK1                                                        
         LA    R1,CTSYSLMT                                                      
         LA    R0,4                                                             
         LA    RE,1                                                             
DATAV3O1 STC   RE,FNDX                                                          
         CLI   0(RF),0                                                          
         BE    EMIF                                                             
         CLI   0(RF),3                                                          
         BH    EFTL                                                             
         TM    2(RF),X'80'                                                      
         BZ    EFNN                                                             
DATAV3O2 CLI   FNDX,2              MAXVALUES=255,255,99,99                      
         BH    DATAV3O3                                                         
         CLC   6(2,RF),=H'255'                                                  
         BH    EFTB                                                             
         B     DATAV3O4                                                         
DATAV3O3 CLC   6(2,RF),=H'99'                                                   
         BH    EFTB                                                             
DATAV3O4 MVC   0(1,R1),7(RF)                                                    
         LA    RF,L'BLOCK1(RF)                                                  
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,DATAV3O1                                                      
         MVI   FNDX,0                                                           
         B     DATAV3Q                                                          
*&&                                                                             
*&&US                                                                           
         CLI   SYSTEM,3            TEST NETWORK                                 
         BE    *+12                                                             
         CLI   SYSTEM,2            TEST SPOT                                    
         BNE   DATAV3P                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A14'  CLPACK                                   
         L     RE,AFACLIST                                                      
         USING COMFACSD,RE                                                      
         L     RF,CCALLOV                                                       
         DROP  RE                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),FLD,CTSYSLMT                                           
         CLI   0(R1),0                                                          
         BNE   EIIF                                                             
*                                                                               
DATAV3P  CLI   SYSTEM,6            TEST ACC                                     
         BNE   DATAV3Q                                                          
         CLC   FLD(2),=C'T='       TEST FOR T=ULLCC (DPS TALENT)                
         BNE   DATAV3Q                                                          
         CLI   FLD+2,C'T'                                                       
         BNE   *+12                                                             
         CLI   FLDH+5,7            LENGTH FOR UNIT T IS LONGER                  
         B     *+8                                                              
         CLI   FLDH+5,6                                                         
         BL    EFTS                                                             
         MVC   CTSYSLMT(2),FLD+2                                                
         LA    RE,FLD+3            HEXIN PARAMS FOR T UNIT                      
         LA    RF,CTSYSLMT+1                                                    
         LA    R0,4                                                             
         CLI   FLD+2,C'T'                                                       
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         LA    R0,2                                                             
         STM   RE,R0,DMCB                                                       
         GOTO1 VHEXIN,DMCB                                                      
         OC    12(4,R1),12(R1)                                                  
         BZ    EFNH                                                             
         CLI   FLD+2,C'T'                                                       
         BNE   *+12                                                             
         CLI   CTSYSLMT+1,X'40'                                                 
         BNH   EIIF                                                             
         MVC   CTSYSLMT+3(3),=C' TL'                                            
         CLI   FLD+2,C'T'                                                       
         BNE   *+12                                                             
         CLI   FLDH+5,9                                                         
         B     *+8                                                              
         CLI   FLDH+5,8                                                         
         BL    DATAV3Q                                                          
         LA    RF,FLD+6                                                         
         CLI   FLD+2,C'T'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         MVC   CTSYSLMT+3(1),1(RF)                                              
         CLI   0(RF),C'*'                                                       
         BE    DATAV3Q                                                          
         CLI   0(RF),C'$'                                                       
         BNE   EIIF                                                             
         NI    CTSYSLMT+3,X'BF'                                                 
         B     DATAV3Q                                                          
*&&                                                                             
DATAV3Q  GOTO1 AFVAL,IDSPRIH       VALIDATE PRIORITY                            
         BZ    DATAV3X                                                          
         TM    FLDH+4,X'08'                                                     
         BZ    EFNN                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R1,DUB                                                           
         STCM  R1,3,CTSYSPRI                                                    
*                                                                               
DATAV3X  GOTO1 AFVAL,IDSPA1H                                                    
         BNZ   DATAV42                                                          
         CLI   SYSTEM,0                                                         
         BE    DATAEND                                                          
         B     EMIF                                                             
*                                                                               
DATAV42  CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
*                                                                               
DATAV44  CLI   FLDH+5,6                                                         
         BNE   DATAV46                                                          
         CLC   FLD(6),=C'DELETE'                                                
         BNE   DATAV46                                                          
         CLI   ACTN,ADD            CANNOT DELETE EL IF ACTN=ADD                 
         BE    EIIF                                                             
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE '),(X'21',(R4)),(1,SYSTEM)           
         B     DATAEND                                                          
*                                                                               
DATAV46  LA    R6,IDSPA1H                                                       
         LA    R8,3                                                             
*                                                                               
DATAV48  TM    1(R6),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         AR    R6,R1                                                            
         LR    R1,R6                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV59                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCK1)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R5,BLOCK1           R5=A(SCAN BLOCK ENTRY)                       
         MVC   FCNT,4(R1)                                                       
         MVI   FNDX,1                                                           
*                                                                               
DATAV50  CLC   FNDX,FCNT                                                        
         BH    DATAV59                                                          
         CLI   0(R5),7             L'PART1                                      
         BH    EFTL                                                             
         CLI   0(R5),1             L'PART1                                      
         BL    EFTS                                                             
         CLI   1(R5),4             L'PART2                                      
         BH    EFTL                                                             
         BE    DATAV52                                                          
         CLI   1(R5),1             L'PART2                                      
         BNE   EIIF                                                             
         MVC   DUB(2),YAUTH        ONE CHR INPUT S/B Y OR N                     
         CLI   22(R5),C'Y'                                                      
         BE    DATAV54                                                          
         MVC   DUB(2),NAUTH                                                     
         CLI   22(R5),C'N'                                                      
         BE    DATAV54                                                          
         B     EIIF                                                             
*                                                                               
DATAV52  TM    3(R5),X'20'         IF NOT Y OR N MUST BE VALID HEX              
         BZ    EFNH                                                             
         GOTO1 VHEXIN,DMCB,22(R5),DUB,4                                         
         OC    DMCB+12(4),DMCB+12  DOUBLE CHECK FOR VALID HEX                   
         BZ    EFNH                                                             
*                                                                               
DATAV54  CLI   0(R5),3                                                          
         BNE   DATAV56                                                          
         CLC   12(3,R5),=C'ALL'                                                 
         BNE   DATAV56                                                          
         CLC   CTSYSALL,XAUTH      ALL VALUES ALREADY INPUT ?                   
         BNE   EDIF                                                             
         MVC   CTSYSALL,DUB                                                     
         B     DATAV58                                                          
*                                                                               
DATAV56  BAS   RE,GETPGAX          GET PROGRAM NUMBER                           
         CLI   PROGRAM,X'FF'                                                    
         BE    EIIF                                                             
         SR    R1,R1                                                            
         IC    R1,PROGRAM                                                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,CTSYSPGM(R1)     POINT TO PROGRAM AUTH                        
         CLC   0(2,R1),XAUTH       PRGM VALUE PREVIOUSLY INPUT ?                
         BNE   EDIF                                                             
         MVC   0(2,R1),DUB                                                      
*                                                                               
DATAV58  ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R5,32(R5)                                                        
         B     DATAV50                                                          
*                                                                               
DATAV59  ZIC   R1,0(R6)            BUMP TO NEXT TWA LINE                        
         AR    R6,R1                                                            
         BCT   R8,DATAV48                                                       
DATAV60  CLC   CTSYSALL,XAUTH      SET ALL VALUE TO N IF N/I                    
         BNE   *+10                                                             
         MVC   CTSYSALL,NAUTH                                                   
         LA    R1,CTSYSPGM         SET PRG VALUES TO ALL VALUE IF N/I           
         LA    RE,64                                                            
*                                                                               
DATAV62  CLC   0(2,R1),XAUTH                                                    
         BNE   *+10                                                             
         MVC   0(2,R1),CTSYSALL                                                 
         LA    R1,2(R1)                                                         
         BCT   RE,DATAV62                                                       
         OI    CTSYSEL+5,X'80'                                                  
         CLC   CTSYSALL(128),CTSYSPGM                                           
         BE    DATAV63                                                          
         BAS   RE,CNVELM21                                                      
         B     DATAV63A                                                         
DATAV63  MVI   CTSYSLEN,16         SET SHORT IF NO PRGM OVERRIDES               
DATAV63A CLI   ACTN,ADD                                                         
         BE    DATAV64                                                          
*                                  DELETE SYSTEM ELEMENT                        
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE '),(X'21',(R4)),(1,SYSTEM)           
*                                  ADD SYSTEM ELEMENT                           
DATAV64  GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
* I/O HANDLING FOR ADD/CHANGE                                                   
*                                                                               
DATAEND  LA    R1,IDSIDH           POSN TO 1ST KEY FLD & SET OK                 
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         CLI   ACTN,CHANGE                                                      
         BNE   DATAEND2                                                         
*                                  CHANGE ID RECORD                             
         MVC   TEMP(2),=X'0204'                                                 
         MVC   TEMP+2(2),IDNUM                                                  
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   KEYSAVE,KEY         WRITE ID REC                                 
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         XC    CTIKEY,CTIKEY       BUILD AND WRITE POINTER REC                  
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID+8(2),IDNUM                                                
         MVI   TEMP,X'02'                                                       
         GOTO1 ADELEL                                                           
         MVC   TEMP(2),=X'020C'                                                 
         MVC   TEMP+2(10),KEYSAVE+15                                            
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   KEY,CTIKEY                                                       
         LA    R5,IOAREA2                                                       
         ST    R5,AREC                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         ST    R4,AREC                                                          
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     DATAX                                                            
*                                  ADD ID RECORD                                
DATAEND2 LA    R4,IOAREA2          READ MASTER ID REC INTO I/O2                 
         ST    R4,AREC                                                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY,CTIKEY                                                       
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    R5,CTIDATA                                                       
         SR    R6,R6                                                            
*                                  SEARCH FOR NUMBER ELEMENT                    
DATAEND4 CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R5),X'02'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DATAEND4                                                         
         MVC   IDNUM,2(R5)         UPDATE POINTER NUMBER                        
*                                  NEW CODE TO ALOCATE ID# BY                   
         LA    R4,IOAREA3            FILLING GAPS                               
         ST    R4,AREC                                                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
*                                                                               
DATAE010 MVC   CTIKNUM,IDNUM                                                    
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD                                                            
         BE    EIIO                FILE ERROR                                   
         CLI   DMCB+8,0            RECORD FOUND                                 
         BE    DATAE020                                                         
         TM    DMCB+8,X'02'        RECORD FOUND DELETED                         
         BNZ   DATAE020                                                         
         TM    DMCB+8,X'10'        RECORD NOT FOUND                             
         BNZ   DATAE030                                                         
         DC    H'00'                                                            
*                                                                               
DATAE020 SR    RF,RF                                                            
         ICM   RF,3,IDNUM                                                       
         LA    RF,1(RF)                                                         
         STCM  RF,3,IDNUM                                                       
         LTR   RF,RF                                                            
         BNZ   DATAE010                                                         
         DC    H'00'                                                            
*                                                                               
DATAE030 LA    R4,IOAREA2                                                       
         ST    R4,AREC                                                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   KEY,CTIKEY                                                       
         MVI   TEMP,0              RE-BUILD MASTER REC & WRITE BACK             
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   TEMP(2),=X'0204'                                                 
         MVC   TEMP+2(2),IDNUM                                                  
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                                                               
         LA    R4,IOAREA           RESTORE I/O PARMS                            
         ST    R4,AREC                                                          
         MVC   KEY,KEYSAVE                                                      
         MVC   TEMP(2),=X'0204'    BUILD PASSIVE (NUMBER) ELEMENT               
         MVC   TEMP+2(2),IDNUM                                                  
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         L     RF,AADD             COPY ADDS A RECORD                           
         CLI   ACTN,RENAME                                                      
         BNE   *+12                                                             
         L     RF,AWRITE           RENAME WRITES A DELETED REC                  
         OI    CTISTAT,X'80'                                                    
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   TEMP,X'02'                                                       
         GOTO1 ADELEL                                                           
         MVC   TEMP(2),=X'020C'    BUILD PASSIVE (ALPHA) ELEMENT                
         MVC   TEMP+2(10),KEYSAVE+15                                            
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID+8(2),IDNUM                                                
         MVC   KEY,CTIKEY                                                       
         CLI   ACTN,RENAME                                                      
         BNE   *+8                                                              
         OI    CTISTAT,X'80'       RENAME ADD A DELETED REC                     
         GOTO1 AADD                ADD NUMERIC RECORD                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         BAS   RE,DISPIDN                                                       
*                                                                               
DATAX    MVI   NACTN,OKDEL+OKCHA+OKCOPY                                         
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* LOCATE SELIST ENTRY FOR SYSTEM                                                
*                                                                               
GETSE    NTR1                                                                   
         L     RE,AFACLIST                                                      
         USING SYSFACD,RE                                                       
         L     R5,VSELIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
*                                                                               
         CLC   SYSTEM,SEOVSYS                                                   
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
         ST    R5,ASE              SAVE A(SELIST ENTRY)                         
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
         XIT1                                                                   
         EJECT                                                                  
* LOCATE SELIST ENTRY FOR SYSTEM/SYSTEMSE                                       
*                                                                               
GETSE2   NTR1                                                                   
         L     RE,AFACLIST                                                      
         L     R5,VSELIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         MVI   FERN,X'FF'                                                       
*                                                                               
GETSE22  CLC   SYSTEM,SEOVSYS                                                   
         BNE   *+14                                                             
         CLC   SENAME,FLD                                                       
         BE    *+12                                                             
         BXLE  R5,R6,GETSE22                                                    
         MVI   FERN,0                                                           
         ST    R5,ASE                                                           
         CLI   FERN,0                                                           
         XIT1                                                                   
*                                                                               
GETSE4   NTR1                                                                   
         L     RE,AFACLIST                                                      
         L     R5,VSELIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
*                                                                               
         CLC   DUB(1),SESYS                                                     
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
         ST    R5,ASE                                                           
         MVC   APGM,SEPGMS                                                      
         XIT1                                                                   
         DROP  RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
* CONVERT THE X'21' ELEMENT TO THE NEW FORMAT                                   
*                                                                               
CNVELM21 NTR1                                                                   
         MVC   BLOCK1(L'TEMP),TEMP COPY THE ELEMENT OVER                        
         LA    R7,BLOCK1           POINT FROM WHERE TO COPY                     
         USING CTSYSD,R7                                                        
         LA    R2,TEMP+CTSYSPGM-CTSYSD  WHERE TO COPY                           
         LA    R3,1                FIRST PROGRAM                                
         LA    R0,16               LENGTH IS HEADER FIRST                       
         LA    R6,CTSYSPGM                                                      
CNV21LP  CLC   0(2,R6),CTSYSALL    DEFAULT?                                     
         BE    CNV21NX             YES, SKIP TO NEXT ONE                        
         STC   R3,0(R2)            STORE THE PROGRAM NUMBER                     
         MVC   1(2,R2),0(R6)       AND ITS AUTHORIZATION CODE                   
         AH    R0,=H'3'            LENGTH IS CHANGED BY 3                       
         LA    R2,3(R2)            NEXT POSTION FOR NEXT PROGRAM                
CNV21NX  LA    R3,1(R3)            NEXT PROGRAM NUMBER                          
         LA    R6,2(R6)            NEXT AUTHORIZATION CODE                      
         CH    R3,=H'64'           DID WE DO ALL 64 PROGRAMS?                   
         BNH   CNV21LP             NO, CONTINUE UNTIL WE'RE DONE                
         LA    R7,TEMP             STORE THE NEW LENGTH OF THE ELEMENT          
         STC   R0,CTSYSLEN                                                      
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
* SET PROGRAM NAME FROM PROGRAM NUMBER                                          
*                                                                               
GETPRGN  NTR1                                                                   
         L     R5,APGM                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
*                                                                               
         CLC   PGMNUM,PROGRAM                                                   
         BE    *+16                                                             
GETPRGN2 BXLE  R5,R6,*-10                                                       
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPRGNX                                                         
         CLC   PGMCTRY,IDCTRY                                                   
         BE    *+12                                                             
         CLI   PGMCTRY,0                                                        
         BNE   GETPRGN2                                                         
         MVC   PGNAME,PGMNAME                                                   
GETPRGNX XIT1                                                                   
         DROP  R5                                                               
         SPACE 1                                                                
* SET PROGRAM NUMBER FROM PROGRAM NAME                                          
* R5=A(SCANNER BLOCK ENTRY)                                                     
*                                                                               
GETPRGX  NTR1                                                                   
         LR    R3,R5                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         BCTR  R1,0                R1=L'COMPARE                                 
         L     R5,APGM                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
*                                                                               
GETPRGX2 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),12(R3)                                                
         BE    *+16                                                             
GETPRGX4 BXLE  R5,R6,GETPRGX2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPRGXX                                                         
         CLC   PGMCTRY,IDCTRY                                                   
         BE    *+12                                                             
         CLI   PGMCTRY,0                                                        
         BNE   GETPRGX4                                                         
         MVC   PROGRAM,PGMNUM                                                   
GETPRGXX XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
* SET PROGRAM NAME FROM PROGRAM ACCESS NUMBER                                   
*                                                                               
GETPGAN  NTR1                                                                   
         L     R5,APGM                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
*                                                                               
GETPGAN2 CLC   PGMNUM,PROGRAM      MATCH ON PROGRAM NUMBER                      
         BE    GETPGANY                                                         
         CLI   PGMALNUM,0                                                       
         BE    *+14                                                             
         CLC   PGMALNUM,PROGRAM    OR ACCESS OVERRIDE (IF SET)                  
         BE    GETPGANY                                                         
GETPGAN4 BXLE  R5,R6,GETPGAN2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPGANX                                                         
GETPGANY CLC   PGMCTRY,IDCTRY                                                   
         BE    *+12                                                             
         CLI   PGMCTRY,0                                                        
         BNE   GETPGAN4                                                         
         MVC   PGNAME,PGMNAME                                                   
GETPGANX XIT1                                                                   
         DROP  R5                                                               
         SPACE 1                                                                
* SET PROGRAM NUMBER FROM PROGRAM NAME                                          
* R5=A(SCANNER BLOCK ENTRY)                                                     
*                                                                               
GETPGAX  NTR1                                                                   
         LR    R3,R5                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         BCTR  R1,0                R1=L'COMPARE                                 
         L     R5,APGM                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
*                                                                               
GETPGAX2 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),12(R3)                                                
         BE    *+16                                                             
GETPGAX4 BXLE  R5,R6,GETPGAX2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPGAXX                                                         
         CLC   PGMCTRY,IDCTRY                                                   
         BE    *+12                                                             
         CLI   PGMCTRY,0                                                        
         BNE   GETPGAX4                                                         
         MVC   PROGRAM,PGMNUM                                                   
         CLI   PGMALNUM,0          USE ACCESS OVERRIDE IF SET                   
         BE    *+10                                                             
         MVC   PROGRAM,PGMALNUM                                                 
GETPGAXX XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
* ROUTINE TO READ ACCESS RECORD INTO IOAREA3                                    
*                                                                               
* NTRY - R1=A(AGENCY ALPHA ID)                                                  
* EXIT - IDCTRY=COUNTRY CODE FROM ACCESS RECORD                                 
*                                                                               
GETACC   LR    R0,RE                                                            
         MVI   IDCTRY,0                                                         
         MVI   FERN,X'FF'                                                       
         MVC   WORK(L'CT5KALPH),0(R1)                                           
         MVC   KEYSAVE,KEY                                                      
         MVC   DUB(4),AREC                                                      
         LA    R1,IOAREA3                                                       
         USING CT5REC,R1                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,WORK                                                    
         MVC   KEY,CT5KEY                                                       
         DROP  R1                                                               
         ST    R1,AREC                                                          
         GOTO1 AREAD                                                            
         MVC   KEY,KEYSAVE                                                      
         MVC   AREC,DUB                                                         
         TM    DMCB+8,X'FF'-X'92'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB+8,0                                                         
         BE    GETACC2                                                          
         MVI   FERN,13             SET RECORD NOT FOUND                         
         B     GETACCX                                                          
*                                                                               
GETACC2  LA    R1,IOAREA3          SAVE AGENCY COUNTRY CODE                     
         LA    R1,CT5DATA-CT5REC(R1)                                            
         SR    RF,RF                                                            
         USING CTAGDD,R1                                                        
GETACC4  CLI   CTAGDEL,0                                                        
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   CTAGDEL,CTAGDELQ                                                 
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GETACC4                                                          
         CLI   CTAGDLEN,CTAGDLNQ                                                
         BNH   GETACCX                                                          
         MVC   IDCTRY,CTAGDCTY                                                  
         DROP  R1                                                               
         B     GETACCX                                                          
*                                                                               
GETACCX  LR    RE,R0                                                            
         CLI   FERN,X'FF'                                                       
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO MERGE SYSTEM INFO FROM ACCESS RECORD                               
*                                                                               
MRGSYS   LR    R0,RE                                                            
         LA    R5,CTIDATA                                                       
MRL1     CLI   0(R5),0             READ EACH ELEMENT IN ID RECORD               
         BE    MRGSYSX                                                          
         CLI   0(R5),X'21'         SYSTEM ELEMENT FOUND                         
         BNE   MRL1A                                                            
         LA    R6,IOAREA3                                                       
         LA    R6,CT5DATA-CT5REC(R6)                                            
MRL2     CLI   0(R6),0             READ EACH ELEMENT IN ACCESS RECORD           
         BE    MRL2A                                                            
         CLI   0(R6),X'21'         SYSTEM ELEMENT FOUND                         
         BNE   MRL2B                                                            
         CLC   CTSYSNUM-CTSYSD(1,R5),CTSYSNUM-CTSYSD(R6)                        
         BNE   MRL2B                                                            
         MVC   CTSYSSE-CTSYSD(2,R5),CTSYSSE-CTSYSD(R6)                          
         B     MRL1A                                                            
*                                                                               
MRL2A    MVI   0(R5),X'99'         SET TEMP ELEM CODE FOR DELETE                
         B     MRL1A                                                            
*                                                                               
MRL2B    SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     MRL2                                                             
*                                                                               
MRL1A    SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     MRL1                                                             
*                                                                               
MRGSYSX  GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE '),(X'99',(R4)),0                    
         CLI   12(R1),0                                                         
         BE    MRGSYSX1                                                         
         CLI   12(R1),X'06'                                                     
         BE    MRGSYSX1                                                         
         DC    H'00'                                                            
MRGSYSX1 LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO DISPLAY SENAME AND AGENCY BINARY NUMBER IN ONE FIELD               
*                                                                               
         SPACE 1                                                                
DISPSEAG NTR1                                                                   
         XC    IDSSEAG,IDSSEAG                                                  
         OI    IDSSEAGH+6,X'80'                                                 
         L     RF,ASE                                                           
         USING SELISTD,RF                                                       
         MVC   IDSSEAG(L'SENAME),SENAME                                         
         LA    R0,L'SENAME                                                      
         DROP  RF                                                               
         LA    R6,IDSSEAG                                                       
DSEAG10  CLI   0(R6),0                                                          
         BE    DSEAG20                                                          
         CLI   0(R6),C' '                                                       
         BE    DSEAG20                                                          
         LA    R6,1(R6)                                                         
         BCT   R0,DSEAG10                                                       
*                                                                               
DSEAG20  OC    AGNUM,AGNUM                                                      
         BZ    DSEAGX                                                           
         MVI   0(R6),C'/'                                                       
         LA    R6,1(R6)                                                         
         GOTO1 VHEXOUT,DMCB,AGNUM,(R6),1,=C'TOG'                                
*                                                                               
DSEAGX   XIT1                                                                   
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
WRKD     DSECT                                                                  
ASYSEL   DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
*                                                                               
SYSTEM   DS    C                                                                
SFLAGS   DS    C                                                                
PROGRAM  DS    C                                                                
PGNAME   DS    CL8                                                              
*                                                                               
IDOPTS   DS    XL2                                                              
IDCNT    DS    C                                                                
EXCNT    DS    C                                                                
PGCNT    DS    C                                                                
FCNT     DS    X                                                                
*                                                                               
AGNUM    DS    XL(L'CTSYSAGB)      AGENCY BINARY # SAVE                         
*                                                                               
IDNUM    DS    H                                                                
IDCTRY   DS    XL1                 COUNTRY CODE FROM ACCESS RECORD              
PASSFLAG DS    XL1                                                              
*                                                                               
SYSEL    DS    XL12                                                             
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
*                                                                               
IOAREA2  DS    1024C                                                            
IOAREA3  DS    1024C                                                            
WRKX     EQU   *                                                                
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
LSYSTEM  DS    C                                                                
         SPACE 1                                                                
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMFED                                                                      
       ++INCLUDE CTLFMFED                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
       SPACE   1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035CTLFM01   05/01/02'                                      
         END                                                                    
