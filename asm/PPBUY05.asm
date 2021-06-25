*          DATA SET PPBUY05    AT LEVEL 059 AS OF 02/26/20                      
*PHASE T41105A                                                                  
*INCLUDE PUBED                                                                  
*INCLUDE OUTER                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY05 - RECALL AND DISPLAY'                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* JSAY 09/13/19 SUPPORT FOR EXTENDED ALLOCATIONS                                
*                                                                               
* KWAN 04/12/16 LINE NUMBER FIX FOR PBU                                         
*                                                                               
* KWAN 07/15/15 SUPPORT MEDIA B (MOBILE), V (NATL VIDEO), W (LOCAL VID)         
*                                                                               
* KWAN 02/24/14 SUPPORT MEDIA L (SOCIAL)                                        
*                                                                               
* BPLA  06/12   FIX LARGE NEGATIVE COST DISPLAY                                 
*                                                                               
* SMYE  01/10   ALLOW FOR RETENTION AND DISPLAY OF "S" COST INDICATOR           
*                 IN "FREE" BUYS                                                
*                                                                               
* KWAN 10/16/06 SKIP DATA RECALL FOR ADBUYER (T41106)                           
*                                                                               
* SMYE 12/05    MORE CHANGES FOR AD-ID IN FMTJOB                                
*                                                                               
* SMYE 08/24/05 CHANGES FOR AD-ID IN FMTJOB                                     
*                                                                               
* KWAN 05/15/01 FIX FOR PRD EXCLUSION CLASS MSG DISPLAYS                        
*                                                                               
* KWAN 03/05/01 RELINK WITH MODIFIED PPBUYWRK1 (4000K BUY REC)                  
*                                                                               
* BPLA 09/00    FIX BUG WHEN USING "RM" RECALL WITH PRD ZZZ                     
*                                                                               
* KWAN 03/00    FIX BUG CAUSED BY CONSOLIDATION OF COMMON CODES                 
*                                                                               
* KWAN 02/00    PRD EXCLUSION WARNING MSG IN PLACE OF PUBNAME                   
*                                                                               
* BPLA 06/99    CHANGES TO "RM" LOGIC TO SET INSERTION DATE                     
*               TO LAST BUY DISPLAYED SO THEY CAN JUST HIT ENTER TO             
*               CONTINUE LISTING BUYS                                           
*                                                                               
* BPLA 03/99    WHEN DISPLAYING EDITIONS ON WSJ SCREEN                          
*               DISPLAY '00' IF BASE (NO EDITIOM) PUB                           
*               IS IN THE WSJ LIST                                              
*                                                                               
* BPLA 12/98    DISPLAY PUB NAMES WHEN LIST BUYING                              
*               CODE FOR FORMATTING OLD JWT CONVERSION ELEMENTS                 
*               NO-OPED                                                         
*                                                                               
* BPLA 12/98    CHANGES FOR INTERACTIVE LIST BUYING                             
*                                                                               
* BPLA 08/98    CHANGES FOR MAGAZINE LIST BUYING                                
*                                                                               
* BPLA 02/98    NEW CODE FOR INTERACTIVE (MEDIA I)                              
*                                                                               
* BPLA 01/97    ADD DISPLAY OF GST CODE (BUYER = GST (DDS ONLY)                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41105   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41105*                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         LA    R8,4095(RB)                                                      
         LA    R8,1(R8)                                                         
         USING T41105+4096,R8      ** NOTE USE OF 2ND BASE REGISTER **          
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         RELOC RELO05                                                           
*                                                                               
         XC    SVINS,SVINS         CLEAR INSERTION DISPLAY TABLE                
         XC    KEY,KEY                                                          
         MVC   TRCODE,BUYTR1       SAVE RECALL MODE                             
*                                                                               
* CLEAR DISPLAY AREA OF SCREEN                                                  
*                                                                               
         LA    R4,BUYAD1H          THIS FIELD LOCATION IS CONSTANT              
         CLI   SVSCRN,X'FE'        SEE IF WSJ                                   
         BNE   CLRSC1                                                           
         CLI   5(R4),0             SEE IF AD INPUT                              
         BE    CLRSC1                                                           
         ZIC   R5,0(R4)                                                         
         AR    R4,R5               LEAVE AD CODE ALONE                          
*                                                                               
CLRSC1   SR    R5,R5                                                            
CLRSC2   IC    R5,0(R4)                                                         
         AHI   R5,-9                                                            
         TM    1(R4),X'20'                                                      
         BZ    CLRSC3                                                           
         CLC   =C'ALLO',8(R4)                                                   
         BE    CLRSC4                                                           
         CLC   =C'OPTI',8(R4)                                                   
         BE    CLRSC4                                                           
         CLC   =C'-EDT',8(R4)                                                   
         BE    CLRSC4                                                           
         CLC   =C'-LIN',8(R4)                                                   
         BE    CLRSC4                                                           
         CLC   =C'-PRE',8(R4)                                                   
         BE    CLRSC4                                                           
         CLC   =C'SPACE CLOSING',8(R4)                                          
         BE    CLRSC4                                                           
         CLC   =C'ON-SALE',8(R4)                                                
         BE    CLRSC4                                                           
CLRSC3   EX    R5,CLROC                                                         
         BZ    CLRSC4                                                           
         EX    R5,CLRXC                                                         
         FOUT  (R4)                                                             
         TM    1(R4),X'20'         SEE IF PROTECTED                             
         BNZ   CLRSC4                                                           
         MVI   5(R4),0             ZERO INPUT LENGHT                            
*                                                                               
CLRSC4   LA    R4,9(R4,R5)         POINT TO NEXT FIELD                          
         CLI   0(R4),0                                                          
         BNE   CLRSC2                                                           
         B     DSPL                                                             
CLRXC    XC    8(0,R4),8(R4)                                                    
CLROC    OC    8(0,R4),8(R4)                                                    
*                                                                               
DSPL     DS    0H                                                               
         XC    WSJJOB,WSJJOB                                                    
         XC    WSJPREM,WSJPREM                                                  
         XC    WSJSPC,WSJSPC                                                    
         MVI   WSJSLEN,0                                                        
*                                                                               
         LA    R2,BUYTR1H                                                       
         ST    R2,TRADDR           SET FIRST TRADDR AND R2                      
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         AR    R4,R2               POINT TO INSERTION DATE                      
         CLI   5(R4),0                                                          
         BNE   DSPL3                                                            
         CLC   BUYPB(2),=C'L='                                                  
         BE    DSPLLST                                                          
         CLC   BUYPB(3),=C'LW='                                                 
         BE    DSPLLST                                                          
DSPL3    BAS   R9,EDTINS           EDIT INSERTION DATE                          
*                                                                               
         CLI   SVSCRN,X'FE'        WSJ EDITION LIST                             
         BNE   DSPL5                                                            
         BAS   R9,BUMPFLD2                                                      
         BAS   R9,EDTJOB           SPECIAL WSJ JOB EDIT                         
*                                  USED WITH INS DATE TO FILTER BUYS            
         B     DSPLLST                                                          
*                                                                               
DSPL5    CLI   SVSCRN,X'FC'                                                     
         BNL   DSPLMIN                                                          
         CLI   SVSCRN,X'FA'                                                     
         BNL   DSPLMIN                                                          
         CLI   SVSCRN,X'F8'                                                     
         BNL   DSPLLST                                                          
         CLI   SVSCRN,X'F6'                                                     
         BNL   DSPLODR                                                          
         CLI   SVSCRN,X'F2'        NOTE-F4 & F5 SHOULD NEVER GET HERE           
         BNL   DSPLMIN             F2 AND F3 - INTERACTIVE                      
         CLI   SVSCRN,X'E8'        MAGAZINE LIST                                
         BE    DSPLLST                                                          
         CLI   SVSCRN,X'E9'        MAGAZINE LIST - ZZZ                          
         BE    DSPLLST                                                          
         CLI   SVSCRN,X'EA'        INTERACTIVE LIST                             
         BE    DSPLLST                                                          
         CLI   SVSCRN,X'EB'        INTERACTIVE LIST - ZZZ                       
         BE    DSPLLST                                                          
*                                                                               
         DC    H'0'                SOMETHING VARY WRONG                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPLMIN  DS    0H                  DISPLAY LOGIC FOR MEDIA M,I,N,S,T            
         CLC   =C'RM',TRCODE                                                    
         BE    DSPL2                                                            
         BAS   R9,NXTINS                                                        
         BNE   NOINS                                                            
         BAS   R9,FMTTR                                                         
         BAS   R9,FMTINS                                                        
         BAS   R9,FMTJOB                                                        
*                                                                               
         CLI   BUYMD,C'N'          APPLIES TO NEWSPAPER ONLY                    
         BNE   DSPL1H                                                           
         BAS   R9,FMTLNS                                                        
         BAS   R9,FMTRTN                                                        
         BAS   R9,BUMPFLD          POINTER NOT INCREMENTED                      
         BAS   R9,FMTPR                                                         
         BAS   R9,BUMPFLD          POINTER NOT INCREMENTED                      
         B     DSPL1P                                                           
*                                                                               
DSPL1H   DS    0H                                                               
         BAS   R9,FMTSP                                                         
         BAS   R9,FMTRTM                                                        
         BAS   R9,FMTCL                                                         
*                                                                               
         CLI   BUYMD,C'M'                                                       
         BE    DSPL1N                                                           
         CLI   BUYMD,C'S'                                                       
         BE    DSPL1N                                                           
         CLI   BUYMD,C'T'                                                       
         BNE   DSPL1P                                                           
DSPL1N   BAS   R9,FMTSL            APPLIES TO M,S,T                             
*                                                                               
DSPL1P   DS    0H                                                               
         BAS   R9,FMTMCL           MATERIALS CLOSING DATE                       
         BAS   R9,FMTD2                                                         
         BAS   R9,BUMPFLD                                                       
         BAS   RE,FMTCOM                                                        
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   DSPL10                                                           
         BAS   R9,BUMPFLD2                                                      
         BAS   RE,FMTPRDS                                                       
         MVC   8(47,R2),DUMEL+8                                                 
         B     DSPL10                                                           
*                                                                               
DSPL2    DS    0H                  DISPLAY MULTIPLE INSERTIONS                  
         XC    DUMEL+250(10),DUMEL+250                                          
         BAS   R9,NEXTFLD                                                       
         LA    R0,DUMEL                                                         
         ST    R0,TRADDR                                                        
         BAS   R9,NXTINS                                                        
         BNE   NOINS                                                            
*                                                                               
         CLI   BUYMD,C'N'          NEWSPAPER HAS OWN DSECT                      
         BE    DSPLNWS                                                          
*                                                                               
DSPL4    BAS   R9,BUMPFLD                                                       
         BE    DSPLAX                                                           
         LR    R7,R2                                                            
         USING MAGLINE,R7                                                       
         XC    DUMEL(50),DUMEL                                                  
         LA    R2,DUMEL                                                         
         BAS   R9,FMTTR                                                         
         MVC   MAGTR,DUMEL+8                                                    
         BAS   R9,FMTINS                                                        
         LA    R1,MAGDT-1                                                       
         CLI   DUMEL+8,C'B'                                                     
         BE    DSPL4B                                                           
         CLI   DUMEL+8,C'W'                                                     
         BE    DSPL4B                                                           
         LA    R1,1(R1)                                                         
*                                                                               
DSPL4B   DS    0H                                                               
         MVC   0(8,R1),DUMEL+8                                                  
         MVC   DUMEL+250(8),DUMEL+8 SAVE LAST DATE DISPLAYED                    
         BAS   R9,FMTJOB                                                        
         MVC   MAGJB,DUMEL+8                                                    
         BAS   R9,FMTSP                                                         
         MVC   MAGSP,DUMEL+8                                                    
         BAS   R9,FMTRTM                                                        
         MVC   MAGRT,DUMEL+8                                                    
         BAS   R9,FMTCL                                                         
*                                                                               
         CLI   BUYMD,C'M'                                                       
         BE    DSPL4G                                                           
         CLI   BUYMD,C'S'                                                       
         BE    DSPL4G                                                           
         CLI   BUYMD,C'T'                                                       
         BNE   DSPL4H                                                           
DSPL4G   MVC   MAGCL,DUMEL+8       APPLIES TO M,S,T                             
         BAS   R9,FMTSL                                                         
         MVC   MAGSL,DUMEL+8                                                    
         BAS   R9,FMTMCL                                                        
*                                                                               
DSPL4H   DS    0H                                                               
         CLI   BUYMD,C'I'          APPLIES TO INTERACTIVE ONLY                  
         JE    DSPL4H_X                                                         
         CLI   BUYMD,C'L'          TREAT SOCIAL SAME AS INTERACTIVE             
         JE    DSPL4H_X                                                         
         CLI   BUYMD,C'B'          TREAT MOBILE SAME AS INTERACTIVE             
         JE    DSPL4H_X                                                         
         CLI   BUYMD,C'D'          TREAT DIGITAL AUDIO SAME AS INTERAC          
         JE    DSPL4H_X                                                         
         CLI   BUYMD,C'V'          TREAT NVIDEO SAME AS INTERACTIVE             
         JE    DSPL4H_X                                                         
         CLI   BUYMD,C'W'          TREAT LVIDEO SAME AS INTERACTIVE             
         JE    DSPL4H_X                                                         
         J     DSPL4L                                                           
*                                                                               
DSPL4H_X MVC   MAGCL,DUMEL+8                                                    
         BAS   R9,FMTMCL                                                        
*                                                                               
DSPL4L   DS    0H                                                               
         MVC   MAGMCL,DUMEL+8                                                   
         BAS   RE,FMTTS            FORMAT TEARSHEET STATUS                      
*                                                                               
         CLC   DUMEL+8(3),=C'T/S'  SEE IF I HAVE DATA                           
         BNE   *+10                                                             
         MVC   MAGMCL,DUMEL+8                                                   
*                                                                               
         LR    R2,R7                                                            
         CLC   =C'TST',BUYNM                                                    
         BNE   *+8                                                              
         BAS   RE,FMTDA                                                         
         CLC   =C'GST',BUYNM       GST DISPLAY FOR DDS                          
         BNE   *+8                                                              
         BAS   RE,FMTDA                                                         
         FOUT  (R2)                                                             
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   DSPL6                                                            
         BAS   R9,BUMPFLD                                                       
         LR    R7,R2                                                            
         BAS   RE,FMTPRDS                                                       
         MVC   MAGSP(47),DUMEL+8                                                
         FOUT  (R2)                                                             
*                                                                               
DSPL6    DS    0H                                                               
         BAS   R9,NXTINS                                                        
         BE    DSPL4                                                            
*                                                                               
         B     DSPLAX                                                           
         DROP  R7                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPLNWS  DS    0H                  DISPLAY LOGIC FOR NEWSPAPER                  
DSPLN4   BAS   R9,BUMPFLD                                                       
         BE    DSPLAX                                                           
         LR    R7,R2                                                            
         USING NWSLINE,R7                                                       
         XC    DUMEL(50),DUMEL                                                  
         LA    R2,DUMEL                                                         
         BAS   R9,FMTTR                                                         
         MVC   NWSTR,DUMEL+8                                                    
         BAS   R9,FMTINS                                                        
         LA    R1,NWSDT-1                                                       
         CLI   DUMEL+8,C'B'                                                     
         BE    DSPLN4B                                                          
         CLI   DUMEL+8,C'W'                                                     
         BE    DSPLN4B                                                          
         LA    R1,1(R1)                                                         
*                                                                               
DSPLN4B  DS    0H                                                               
         MVC   0(8,R1),DUMEL+8                                                  
         MVC   DUMEL+250(8),DUMEL+8 SAVE LAST DATE DISPLAYED                    
         BAS   R9,FMTJOB                                                        
         MVC   NWSJB,DUMEL+8                                                    
         BAS   R9,FMTLNS                                                        
         MVC   NWSLNS,DUMEL+8                                                   
         BAS   R9,FMTRTN                                                        
         MVC   NWSRT,DUMEL+8                                                    
         BAS   R9,FMTPR                                                         
         MVC   NWSPR,DUMEL+8                                                    
         BAS   R9,FMTMCL                                                        
         MVC   NWSMCL,DUMEL+8                                                   
*                                                                               
         BAS   RE,FMTTS            FORMAT TEARSHEET STATUS                      
         CLC   DUMEL+8(3),=C'T/S'  SEE IF I HAVE DATA                           
         BNE   *+10                                                             
         MVC   NWSMCL,DUMEL+8                                                   
*                                                                               
         LR    R2,R7                                                            
         MVC   77(3,R2),PBDLIST    LIST BUY DESC                                
         CLC   =C'TST',BUYNM                                                    
         BNE   *+8                                                              
         BAS   RE,FMTDA                                                         
         CLC   =C'GST',BUYNM       GST DISPLAY FOR DDS                          
         BNE   *+8                                                              
         BAS   RE,FMTDA                                                         
         FOUT  (R2)                                                             
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   DSPLN6                                                           
         BAS   R9,BUMPFLD                                                       
         LR    R7,R2                                                            
         BAS   RE,FMTPRDS                                                       
         MVC   NWSLNS(47),DUMEL+8                                               
         FOUT  (R2)                                                             
*                                                                               
DSPLN6   DS    0H                                                               
         BAS   R9,NXTINS                                                        
         BE    DSPLN4                                                           
         DROP  R7                                                               
*                                                                               
* ALL "RM" DISPLAYS NOW END HERE, SINCE CODES IS SAME FOR ALL MEDIA             
*                                                                               
DSPLAX   CLI   DUMEL+250,C' '                                                   
         BNH   DSPLAXX                                                          
         FOUT  BUYDT1H,DUMEL+250,8  PUT LAST DATE IN DATE FIELD                 
         OI    BUYDT1H+1,X'01'     SET MODIFIED                                 
*                                                                               
DSPLAXX  B     ALLDONE             NO MORE                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPLLST  XC    KEY,KEY                                                          
         MVC   KEY(11),SVLSTKEY                                                 
         MVC   KEY+27(4),SVLSTDA                                                
         OC    SVLSTDA,SVLSTDA     TEST LIST COMPLETED                          
         BZ    DLSTEND                                                          
         MVI   ELCODE,X'20'                                                     
         LA    R5,REC+33                                                        
         BAS   RE,GETREC                                                        
         OC    SVNPUBS,SVNPUBS     TEST FIRST TIME                              
         BNE   DLST10                                                           
         MVC   SVNPUBS,REC+38      SAVE NUMBER OF ENTRIES                       
*                                                                               
         MVC   HALF,=H'110'        OLD MAX/REC                                  
         BAS   R9,NEXTEL                                                        
         CLI   1(R5),8                                                          
         BE    *+10                                                             
         MVC   HALF,=H'80'         NEW MAX/REC                                  
         LA    R5,REC+33                                                        
*                                                                               
         LA    R3,LFRSTERR                                                      
         CLC   SVNFRST,SVNPUBS                                                  
         BNH   *+12                                                             
         LA    R2,BUYPBH                                                        
         B     ERROR                                                            
         LH    R4,SVNFRST                                                       
         LTR   R4,R4                                                            
         BNZ   *+12                                                             
         LA    R4,1                                                             
         STH   R4,SVNFRST                                                       
DLST2    CH    R4,HALF             CALCULATE SUBLINE (110 ELS/REC)              
         BNH   DLST4                                                            
         IC    RE,KEY+10           INCREMENT SUBLINE                            
         LA    RE,1(RE)                                                         
         STC   RE,KEY+10                                                        
         SH    R4,HALF                                                          
         B     DLST2                                                            
DLST4    CLC   KEY(11),SVLSTKEY                                                 
         BE    DLST6                                                            
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         MVC   SVLSTKEY,KEY                                                     
         MVC   SVLSTDA,KEY+27                                                   
*                                                                               
DLST6    DS    0H                                                               
         CLI   0(R5),X'20'                                                      
         BE    *+8                                                              
DLST8    BAS   R9,NEXTEL                                                        
         BNE   ERROR                                                            
         LTR   R4,R4                                                            
         BZ    DLST20                                                           
         BCT   R4,DLST8                                                         
         B     DLST20                                                           
*                                                                               
DLST10   OC    SVLSTDA,SVLSTDA     TEST END OF LIST REACHED                     
         BZ    DLSTEND                                                          
         LA    R1,SVLST            FIND LAST ENTRY IN CURRENT LIST              
         LH    RE,SVNFRST                                                       
         LA    RE,1(RE)                                                         
DLST12A  CLI   6(R1),X'FF'         EOL                                          
         BE    DLST12B                                                          
         OC    6(6,R1),6(R1)                                                    
         BZ    DLST12B                                                          
         LA    RE,1(RE)                                                         
         LA    R1,6(R1)                                                         
         B     DLST12A                                                          
*                                                                               
DLST12B  CLI   SVSCRN,X'FE'                                                     
         BE    *+8                 FOR WSJ - LEAVE SVNFRST ALONE                
         STH   RE,SVNFRST                                                       
*                                                                               
         BAS   R9,NEXTEL           MATCH LAST ENTRY TO LIST ITEM                
         BNE   DLST24                                                           
         CLC   0(6,R1),2(R5)                                                    
         BNL   *-14                                                             
*                                                                               
DLST20   XC    SVLST,SVLST                                                      
         MVI   SVLSTX-1,X'FF'      SET EOL                                      
         CLI   SVSCRN,X'FE'        FOR WSJ SET TO 8 PUBS                        
         BNE   DLST20C                                                          
         LA    R7,8                                                             
         B     DLST21X                                                          
*                                                                               
DLST20C  L     R4,TRADDR           COUNT NUMBER OF PUBS TO DISPLAY              
         SR    R0,R0                                                            
         SR    R7,R7                                                            
DLST21   LH    R6,SVNTRNS                                                       
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         BCT   R6,*-6                                                           
         LA    R7,1(R7)                                                         
         CLI   0(R4),9                                                          
         BE    *+12                                                             
         CLI   0(R4),0                                                          
         BNE   DLST21                                                           
*                                                                               
DLST21X  LA    R4,SVLST                                                         
DLST22   MVC   0(6,R4),2(R5)                                                    
         LA    R4,6(R4)                                                         
         BAS   R9,NEXTEL                                                        
         BNE   DLST24                                                           
DLST23   BCT   R7,DLST22                                                        
         B     DLST30                                                           
DLST24   XC    KEY,KEY                                                          
         MVC   KEY(11),SVLSTKEY                                                 
         BAS   RE,HIGH                                                          
         BAS   RE,SEQ                                                           
         CLC   KEY(10),KEYSAVE                                                  
         BNE   DLST26                                                           
         BAS   RE,GETREC                                                        
         LA    R5,REC+33                                                        
         CLI   0(R5),X'20'                                                      
         BE    *+8                                                              
         BAS   R9,NEXTEL                                                        
         MVC   SVLSTKEY,KEY                                                     
         MVC   SVLSTDA,KEY+27                                                   
         OC    SVLST(6),SVLST                                                   
         BZ    DLST20                                                           
         B     DLST23                                                           
*                                                                               
DLST26   CLI   SVSCRN,X'FE'        FOR WSJ - LEAVE SVLSTDA ALONE                
         BE    *+10                                                             
         XC    SVLSTDA,SVLSTDA     SET EOL FLAG                                 
*                                                                               
DLST30   LA    R7,SVLST                                                         
         CLI   SVSCRN,X'FE'        SEE IF WSJ EDITION LIST                      
         BNE   DLST31                                                           
         ZAP   TOTCOST,=P'0'                                                    
         LA    R2,BUYTR1H                                                       
         LHI   RF,11                                                            
         BAS   R9,BUMPFLDS                                                      
         ST    R2,TRADDR           SET TO FIRST EDITION                         
*                                                                               
DLST31   OC    0(6,R7),0(R7)       TEST NO PUBS IN LIST                         
         BE    DLSTEND                                                          
DLST32   MVC   BPUB,0(R7)                                                       
         XC    KEY,KEY                                                          
         OC    BINSDT,BINSDT       TEST INSERTION DATE ENTERED                  
         BZ    DLST36                                                           
DLST32A  BAS   R9,NXTINS                                                        
         BNE   DLST36                                                           
*                                                                               
         CLI   SVSCRN,X'FE'        WSJ EDITION LIST                             
         BE    DLST33                                                           
         BAS   R9,FMTTR                                                         
         BAS   R9,FMTINS                                                        
*                                                                               
         BAS   R9,BUMPFLD                                                       
         BAS   R9,FMTPUB                                                        
         BAS   R9,FMTPNAME         GO FORMAT PUB NAME                           
*                                                                               
         BAS   R9,FMTJOB                                                        
*                                                                               
         CLI   SVSCRN,X'E8'        SEE IF MAGAZINE LIST                         
         BE    DSPL32M                                                          
         CLI   SVSCRN,X'E9'        SEE IF MAGAZINE LIST - ZZZ                   
         BE    DSPL32M                                                          
*                                                                               
         CLI   SVSCRN,X'EA'        SEE IF INTERACTIVE LIST                      
         BE    DSPL32I                                                          
         CLI   SVSCRN,X'EB'        SEE IF INTERACTIVE LIST -ZZZ                 
         BE    DSPL32I                                                          
*                                                                               
         BAS   R9,FMTLNS                                                        
         BAS   R9,FMTRTN                                                        
         BAS   R9,BUMPFLD          POINTER NOT INCREMENTED                      
         BAS   R9,FMTPR                                                         
         BAS   R9,BUMPFLD          POINTER NOT INCREMENTED                      
         BAS   R9,FMTMCL                                                        
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   DLST34                                                           
         BAS   R9,BUMPFLD2         PAST MAT CLOSING AND PUB NAME                
         BAS   R9,BUMPFLD          PAST ALLOCATION ANNO                         
         BAS   RE,FMTPRDS                                                       
         MVC   8(47,R2),DUMEL+8                                                 
         B     DLST34                                                           
*                                                                               
DSPL32M  DS    0H                  MAGAZINE LIST ROUTINE                        
         BAS   R9,FMTSP                                                         
         BAS   R9,FMTRTM                                                        
         BAS   R9,FMTMCL                                                        
         BAS   R9,BUMPFLD2         PUB NAME + SPACE CLOSING ANNO                
         BAS   R9,FMTCL                                                         
         BAS   R9,BUMPFLD          MUST GET PAST ON-SALE ANNO                   
         BAS   R9,FMTSL                                                         
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   DLST34                                                           
         BAS   R9,BUMPFLD2                                                      
         BAS   RE,FMTPRDS                                                       
         MVC   8(47,R2),DUMEL+8                                                 
         B     DLST34                                                           
*                                                                               
DSPL32I  DS    0H                  MAGAZINE LIST ROUTINE                        
         BAS   R9,FMTSP                                                         
         BAS   R9,FMTRTM                                                        
         BAS   R9,FMTMCL                                                        
         BAS   R9,BUMPFLD2         PUB NAME AND SPACE CLOSING ANNO              
         BAS   R9,FMTCL                                                         
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   DLST34                                                           
         BAS   R9,BUMPFLD2                                                      
         BAS   RE,FMTPRDS                                                       
         MVC   8(47,R2),DUMEL+8                                                 
         B     DLST34                                                           
*                                                                               
DLST33   DS    0H                                                               
         BAS   R9,FMTEDT                                                        
         BAS   R9,FMTLNS                                                        
         BAS   R9,FMTPR                                                         
         BAS   R9,BUMPFLD          POINTER NOT INCREMENTED                      
         BAS   R9,FMTINFO                                                       
         LA    R1,SVLST                                                         
         CR    R7,R1               SEE IF DOING FIRST PUB                       
         BNE   DLST34                                                           
         ST    R2,FULL             SAVE R2                                      
         MVC   SAVTRAD,TRADDR      SAVE TRANSACTION ADDR                        
         LA    R2,BUYTR1H          SET TO TRANSACTION FIELD                     
         ST    R2,TRADDR                                                        
         BAS   R9,FMTTR                                                         
         BAS   R9,FMTINS                                                        
         BAS   R9,FMTJOB                                                        
         BAS   R9,FMTNRCD                                                       
         BAS   R9,FMTRCD                                                        
         BAS   R9,FMTMCL                                                        
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   DLST33D                                                          
         BAS   R9,BUMPFLD2                                                      
         BAS   R9,BUMPFLD2                                                      
         BAS   RE,FMTPRDS                                                       
         MVC   8(47,R2),DUMEL+8                                                 
DLST33D  L     R2,FULL             RESTORE R2                                   
         MVC   TRADDR,SAVTRAD      RESTORE TRADDR                               
*                                                                               
DLST34   LA    R7,6(R7)                                                         
         CLI   0(R7),X'FF'                                                      
         BE    DLST38                                                           
         OC    0(6,R7),0(R7)                                                    
         BZ    DLST38                                                           
         BAS   R9,NEXTFLD                                                       
         CLI   0(R2),9             EOS                                          
         BE    DLST38                                                           
         B     DLST32                                                           
*                                                                               
DLST36   CLI   SVSCRN,X'FE'        WSJ EDITION LIST                             
         BNE   DLST37                                                           
         BAS   R9,FMTEDT                                                        
         ST    R2,SAVR2                                                         
         BAS   R9,BUMPFLD                                                       
         OC    WSJSPC,WSJSPC       DISPLAY SPACE FROM JOBREC                    
         BZ    DLST36B                                                          
         CLI   5(R2),0             CHK FOR INPUT                                
         BNE   DLST36B                                                          
         MVC   8(8,R2),WSJSPC                                                   
         MVC   5(1,R2),WSJSLEN     SET INPUT LENGHT                             
         FOUT  (R2)                                                             
*                                                                               
DLST36B  OC    WSJPREM,WSJPREM     DISPLAY PREM FROM JOBREC                     
         BZ    DLST36C                                                          
         BAS   R9,BUMPFLD                                                       
         CLI   5(R2),0             CHK FOR INPUT                                
         BNE   DLST36C             YES LEAVE ALONE                              
         MVC   8(3,R2),WSJPREM                                                  
         MVI   5(R2),2                                                          
         FOUT  (R2)                                                             
DLST36C  L     R2,SAVR2                                                         
*                                                                               
DLST36D  LA    R1,SVLST                                                         
         CR    R7,R1               SEE IF DOING FIRST PUB                       
         BNE   DLST34                                                           
         LA    R9,BUYTR1H                                                       
         MVC   8(2,R9),=C'* '                                                   
         FOUT  (R9)                                                             
         B     DLST34                                                           
*                                                                               
DLST37   MVC   8(2,R2),=C'* '                                                   
         FOUT  (R2)                                                             
         BAS   R9,BUMPFLD2                                                      
         BAS   R9,FMTPUB                                                        
         BAS   R9,FMTPNAME         GO FORMAT PUB NAME (R2 UNAFFECTED)           
         B     DLST34                                                           
*                                                                               
DLST38   CLI   SVSCRN,X'FE'        SEE IF WSJ EDITION LIST                      
         BNE   DLST40                                                           
         OC    SVINS,SVINS         SEE IF I FOUND INSERTIONS                    
         BZ    NOINS                                                            
         LA    R2,BUYTR1H                                                       
         LHI   RF,43                                                            
         BAS   R9,BUMPFLDS                                                      
         BAS   R9,FMTTOT           DISPLAY TOTAL COST OF BOOKING                
DSLT38C  XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(22),LSTMSG                                                
         FOUT  BUYMSGH                                                          
         B     DLSTX                                                            
*                                                                               
DLST40   FOUT  BUYMSGH,LSTMSG                                                   
         LH    R0,SVNFRST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUYMSG+37(3),DUB                                                 
         LH    R0,SVNPUBS                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUYMSG+44(3),DUB                                                 
         B     DLSTX                                                            
*                                                                               
LSTMSG   DC    C'ENTER TRANSACTION DATA. FIRST PUB IS NNN OF NNN'               
*                                                                               
DLSTEND  FOUT  BUYMSGH,LSTEND                                                   
         NI    BUYPBH+4,X'DF'      UNVALIDATE PUB AT END OF LIST                
*                                                                               
DLSTX    DS    0H                                                               
         LA    R2,BUYPBH                                                        
         MVI   ERRAREA,C'L'                                                     
         B     EXIT                                                             
*                                                                               
LSTEND   DC    C'ACTION COMPLETED - NO MORE ENTRIES IN LIST'                    
*                                                                               
         EJECT                                                                  
DSPLODR  DS    0H                                                               
         CLC   =C'RM',TRCODE                                                    
         BE    DSPLO2                                                           
         BAS   R9,NXTINS                                                        
         BNE   NOINS                                                            
         BAS   R9,FMTTR                                                         
         BAS   R9,FMTINS                                                        
         BAS   R9,FMTJOB                                                        
         BAS   R9,FMTSHOW                                                       
         BAS   R9,FMTREG                                                        
         BAS   R9,FMTILL                                                        
         BAS   R9,FMTRTM                                                        
         BAS   R9,FMTCL                                                         
         BAS   R9,FMTMCL           MATERIALS CLOSING DATE                       
         BAS   R9,FMTD2                                                         
         BAS   R9,BUMPFLD                                                       
         BAS   RE,FMTCOM                                                        
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   DSPL10                                                           
         BAS   R9,BUMPFLD2                                                      
         BAS   RE,FMTPRDS                                                       
         MVC   8(47,R2),DUMEL+8                                                 
         B     DSPL10                                                           
*                                                                               
DSPLO2   DS    0H                                                               
         XC    DUMEL+250(10),DUMEL+250                                          
         BAS   R9,NEXTFLD                                                       
         LA    R0,DUMEL                                                         
         ST    R0,TRADDR                                                        
         BAS   R9,NXTINS                                                        
         BNE   NOINS                                                            
*                                                                               
DSPLO4   BAS   R9,BUMPFLD                                                       
         BE    DSPLAX                                                           
         LR    R7,R2                                                            
         USING ODRLINE,R7                                                       
         XC    DUMEL(50),DUMEL                                                  
         LA    R2,DUMEL                                                         
         BAS   R9,FMTTR                                                         
         MVC   ODRTR,DUMEL+8                                                    
         BAS   R9,FMTINS                                                        
         LA    R1,ODRDT-1                                                       
         CLI   DUMEL+8,C'B'                                                     
         BE    DSPLO4B                                                          
         CLI   DUMEL+8,C'W'                                                     
         BE    DSPLO4B                                                          
         LA    R1,1(R1)                                                         
DSPLO4B  DS    0H                                                               
         MVC   0(8,R1),DUMEL+8                                                  
*                                                                               
         MVC   DUMEL+250(8),DUMEL+8 SAVE LAST DATE DISPLAYED                    
*                                                                               
         BAS   R9,FMTJOB                                                        
         MVC   ODRJB,DUMEL+8                                                    
         GOTO1 =V(OUTER),DMCB,(0,PBUYREC),(1,ODRSHOW),RR=RELO05                 
         BAS   R9,FMTRTM                                                        
         MVC   ODRRT,DUMEL+8                                                    
         BAS   R9,FMTCL                                                         
         MVC   ODRCL,DUMEL+8                                                    
         BAS   R9,FMTMCL                                                        
*                                                                               
         BAS   RE,FMTTS            FORMAT TEARSHEET STATUS                      
         CLC   DUMEL+8(3),=C'T/S'  SEE IF I HAVE DATA                           
         BNE   *+10                                                             
         MVC   ODRMCL,DUMEL+8                                                   
*                                                                               
         MVC   ODRMCL,DUMEL+8                                                   
         LR    R2,R7                                                            
         CLC   =C'TST',BUYNM                                                    
         BNE   *+8                                                              
         BAS   RE,FMTDA            DISK ADDR                                    
         CLC   =C'GST',BUYNM       GST DISPLAY FOR DDS                          
         BNE   *+8                                                              
         BAS   RE,FMTDA                                                         
         FOUT  (R2)                                                             
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   DSPLO6                                                           
         BAS   R9,BUMPFLD                                                       
         LR    R7,R2                                                            
         BAS   RE,FMTPRDS                                                       
         MVC   ODRSHOW(47),DUMEL+8                                              
         FOUT  (R2)                                                             
*                                                                               
DSPLO6   DS    0H                                                               
         BAS   R9,NXTINS                                                        
         BE    DSPLO4                                                           
         B     DSPLAX                                                           
*                                                                               
NXTINS   DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         CLI   KEY,0               TEST FIRST TIME                              
         BNE   NXTINS2                                                          
         BRAS  RE,SETBYKEY         SET BUY KEY                                  
         BAS   RE,HIGH                                                          
         CLC   BUYPB(2),=C'L='                                                  
         BE    NXTINS2+4                                                        
         CLC   BUYPB(3),=C'LW='                                                 
         BE    NXTINS2+4                                                        
         CLC   =C'RM',TRCODE                                                    
         BE    NXTINS2+4                                                        
         CLC   KEY(25),KEYSAVE                                                  
         BNE   NXTINSX                                                          
         CLI   KEY+25,X'FF'                                                     
         BNE   NXTINS2+4                                                        
         LTR   RE,RE                                                            
         B     NXTINSX                                                          
NXTINS2  BAS   RE,SEQ                                                           
         CLI   KEY+25,X'FF'                                                     
         BE    NXTINS2                                                          
         CLC   KEY(16),KEYSAVE     TEST SAME THRU PUB                           
         BNE   NXTINSX                                                          
         CLC   BUYPB(2),=C'L='     TEST LIST BUY                                
         BE    NXTINS2A                                                         
         CLC   BUYPB(3),=C'LW='    TEST LIST BUY                                
         BNE   *+14                                                             
NXTINS2A CLC   KEY(19),KEYSAVE     LIST RECALL MUST MATCH DATE                  
         BNE   NXTINSX                                                          
*                                                                               
         CLC   KEY+19(2),BEST      TEST RIGHT EST                               
         BNE   NXTINS2                                                          
         OC    KEY+21(3),KEY+21    TEST ACTIVE                                  
         BNZ   NXTINS2             NO                                           
         BAS   RE,GETREC                                                        
         CLI   SVSCRN,X'FE'        SEE IF DOING WSJ ONLY                        
         BNE   NXTINS3                                                          
*                                                                               
         ST    R9,FULL                                                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'35'        MUST FIND WSJ ELEM                           
         BAS   R9,NEXTEL                                                        
         BE    NXTINS2C                                                         
         L     R9,FULL                                                          
         B     NXTINS2             KEEP LOOKING                                 
*                                                                               
NXTINS2C L     R9,FULL                                                          
         OC    WSJJOB,WSJJOB       SEE IF JOB SPECIFIED                         
         BNZ   NXTINS2F                                                         
         MVC   WSJJOB,PBDJOB       SAVE JOB OF FIRST INS                        
         B     NXTINS2X            + USE IT AS FILTER                           
NXTINS2F CLC   PBDJOB,WSJJOB                                                    
         BNE   NXTINS2             SKIP THIS INS                                
         CLC   PBDLIST,SVLSTID     LIST CODES MUST MATCH                        
         BNE   NXTINS2             ELSE SKIP THIS BUY                           
*                                                                               
NXTINS2X DS    0H                  SAVE DATA IN SVINS LIST                      
*                                                                               
NXTINS3  LA    R0,DUMEL                                                         
         C     R0,TRADDR           TEST FOR DUMMY LINE                          
         BE    NXTINSX                                                          
         LA    R1,SVINS            FIND A SLOT                                  
         OC    0(6,R1),0(R1)                                                    
         BZ    *+12                                                             
         LA    R1,6(R1)                                                         
         B     *-14                                                             
         L     R0,TRADDR           GET REL TWA ADDR                             
         SR    R0,RA                                                            
         STH   R0,0(R1)                                                         
*                                                                               
NXTINS4  DS    0H                                                               
         MVC   2(4,R1),KEY+27      SAVE DISK ADDRESS                            
         CR    R2,R2               SET CC                                       
*                                                                               
NXTINSX  DS    0H                                                               
         LA    R1,1                PRESERVE CC                                  
         BNZ   *+6                                                              
         SR    R1,R1                                                            
         NI    DMINBTS,X'F7'       RESET DELETES                                
*                                                                               
         LTR   R1,R1                                                            
         BR    R9                                                               
*                                                                               
NEXTFLD  L     R2,TRADDR                                                        
         LH    RE,SVNTRNS                                                       
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
         ST    R2,TRADDR           SET NEW TRADDR                               
         CLI   0(R2),0             SET CC TO TEST MORE ROOM                     
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
BUMPFLDS SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,BUMPFLDS                                                      
         BR    R9                                                               
*                                                                               
BUMPFLD2 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BR    R9                                                               
*                                                                               
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                                                             
         BCTR  R1,0                                                             
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
*                                                                               
VARMOVE  MVC   WORK(0),8(R2)                                                    
*                                                                               
FMTTR    GOTO1 VFMTTR,DMCB,REC                                                  
         BR    R9                                                               
*                                                                               
FMTINS   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 VFMTINS,DMCB,REC                                                 
         OI    4(R2),X'20'         SET VALIDATED BIT                            
         BR    R9                                                               
*                                                                               
FMTSHOW  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         OC    PBDSHOW,PBDSHOW                                                  
         BCR   8,R9                                                             
         CP    PBDSHOW,=P'99999'                                                
         BNE   FMTSH1                                                           
         MVC   8(3,R2),=C'SPC'                                                  
         B     FMTSHX                                                           
*                                                                               
FMTSH1   EDIT  (P3,PBDSHOW),(3,8(R2)),0,ALIGN=LEFT                              
*                                                                               
FMTSHX   FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATE                                     
         BR    R9                                                               
*                                                                               
FMTREG   DS    0H                                                               
         LA    R3,PBDREG                                                        
         B     FMTILL5                                                          
*                                                                               
FMTILL   DS    0H                                                               
         LA    R3,PBDILLUM                                                      
FMTILL5  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         OC    0(3,R3),0(R3)                                                    
         BCR   8,R9                                                             
         EDIT  (P3,0(R3)),(4,8(R2)),0,ALIGN=LEFT                                
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATE                                     
         BR    R9                                                               
*                                                                               
FMTSP    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         FOUT  (R2),PBDSPACE                                                    
         OI    4(R2),X'20'                                                      
         BR    R9                                                               
*                                                                               
FMTJOB   DS    0H                                                               
         XC    ADIDJOB,ADIDJOB     CLEAR SAVED AD-ID ONLY JOB CODE              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(6,R2),PBDJOB                                                   
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATED                                    
         CLI   PBDJOB,X'FF'        AD-ID ONLY (JOB CODE OF X'FFNNNN') ?         
         BNE   FMTJOBX             NO                                           
         MVC   ADIDJOB,PBDJOB      SAVE JOB CODE FROM BUY                       
         MVC   8(6,R2),=C'*ADID*'  REPLACE "NON-DISPLAYABLE" X'FF' DATA         
FMTJOBX  DS    0H                                                               
         BR    R9                                                               
*                                                                               
FMTLNS   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R4,8(R2)                                                         
         XC    0(8,R4),0(R4)                                                    
         LA    R5,X                                                             
         XC    X(10),X                                                          
*                                                                               
         CLC   PBDSPACE(2),=X'7B00'                                             
         BE    FMTLNS2                                                          
         CLC   PBDSPACE(2),=C'# '                                               
         BE    FMTLNS2                                                          
         CLC   PBDSPACE(2),=C'* '  TEST SPACE BUY                               
         BH    FMTLNS4             YES                                          
*                                                                               
         CLC   PBDSPACE(2),=C'$$'  FOR JWT DELETES                              
         BE    FMTLNS4             YES                                          
*                                                                               
FMTLNS2  DS    0H                                                               
         CLI   PBDSPACE,C'*'                                                    
         BNE   *+12                                                             
         MVI   0(R5),C'*'                                                       
         LA    R5,1(R5)                                                         
         CLI   PBDSPACE,C'#'       SPECIAL FOR NO ASC CHECKING                  
         BNE   *+12                                                             
         MVI   0(R5),C'#'                                                       
         LA    R5,1(R5)                                                         
         LR    RF,R9                                                            
         ZAP   DUB,PBDUNITS                                                     
         BNZ   *+16                                                             
         MVI   0(R5),C'0'                                                       
         LA    R5,1(R5)                                                         
         B     *+10                                                             
         BAS   R9,EDALIN                                                        
         AR    R5,R0                                                            
*                                                                               
         MVC   0(1,R5),PBDUIND                                                  
         OI    0(R5),X'40'         TO SET X'89' TO X'C9'                        
         LA    R5,1(R5)                                                         
*                                                                               
         OC    PBDCLMS,PBDCLMS                                                  
         BNZ   *+10                                                             
         ZAP   PBDCLMS,=P'0'                                                    
         ZAP   DUB,PBDCLMS                                                      
         BZ    FMTLNS3                                                          
         MVI   0(R5),C'/'                                                       
         LA    R5,1(R5)                                                         
         BAS   R9,EDAL                                                          
         AR    R5,R0                                                            
FMTLNS3  LA    R0,X                                                             
         SR    R5,R0                                                            
         CHI   R5,8                                                             
         BNH   *+8                                                              
         LA    R5,8                                                             
         LR    R9,RF                                                            
         MVC   0(8,R4),X                                                        
         STC   R5,5(R2)                                                         
         FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
FMTLNS4  DS    0H                                                               
         MVC   8(8,R2),PBDSPACE                                                 
         MVI   5(R2),8                                                          
         OI    4(R2),X'20'         VALIDATED                                    
         FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
FMTPUB   GOTO1 =V(PUBED),DMCB,(R7),8(R2),RR=RELO05                              
*                                                                               
         FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTPNAME DS    0H                  FORMAT PUB NAME                              
         ST    R2,FULL             SAVE R2                                      
         ST    R9,SAVER7           (YES I KNOW I'M USING SAVER7)                
         LHI   RF,6                6 FIELDS TO PUB NAME FOR NEWS                
         CLI   BUYMD,C'N'                                                       
         BE    FMTPN0                                                           
         LHI   RF,5                FOR ALL OTHER MEDIA                          
FMTPN0   BAS   R9,BUMPFLDS         5 FIELDS TO PUB NAME FIELD                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),BUYMD                                                     
         MVC   KEY+1(6),0(R7)      PUB NUMBER FROM LIST                         
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    FMTPN2                                                           
         CLI   SVAGPROF+16,C'0'    TEST DEFAULT TO SRDS                         
         BE    FMTPN1              NO                                           
         MVC   KEYSAVE+7(2),=C'ZZ' TEST DEFAULT FOUND                           
         CLC   KEYSAVE(25),KEY                                                  
         BE    FMTPN2                                                           
*                                                                               
FMTPN1   DS    0H                                                               
         MVC   KEY,KEYSAVE         SET FOR READ                                 
         BAS   RE,READPUB                                                       
*                                                                               
FMTPN2   DS    0H                                                               
         BAS   RE,GETPUB                                                        
*                                                                               
         CLI   BYPROF+6,0                                                       
         BE    FMTPN5              NOTHING IN PROFILE, DONE                     
         CLI   BYPROF+6,C'N'                                                    
         BE    FMTPN5              NO MSG IS NEEDED                             
*                                                                               
         L     R5,APUBIO                                                        
         LA    R5,33(R5)           POINT TO ELEMENTS                            
         MVI   ELCODE,X'20'                                                     
         BAS   R9,NEXTEL           LOOKING FOR PRODUCTION ELEM                  
         BNE   FMTPN5              NOT FOUND, DONE WITH EXCL CLASS              
         USING PUBGENEL,R5                                                      
         CLI   PUBEXCL,0                                                        
         BE    FMTPN5              NO EXCL CLASS, DONE                          
         MVC   BYTE,PUBEXCL                                                     
         NC    BYTE,SVPEXCL                                                     
         BZ    FMTPN5              NO CONFLICTS IN EXCL CLASS, DONE             
         DROP  R5                                                               
*                                                                               
         CLI   BYPROF+6,C'W'       WARNING?                                     
         BNE   FMTPN2P             NO, MUST BE STOP THEN                        
*                                                                               
         MVC   8(L'EXCLWRN2,R2),EXCLWRN2                                        
         B     FMTPN7                                                           
*                                                                               
FMTPN2P  DS    0H                                                               
         MVC   8(L'EXCLMSG1,R2),EXCLMSG1                                        
         B     FMTPN7                                                           
*                                                                               
EXCLMSG1 DC    C'** EXCL CLASS CONFLICTS, BUY NOT ALLOWED'                      
EXCLWRN2 DC    C'** WARNING - EXCL CLASS CONFLICTS'                             
*                                                                               
FMTPN5   DS    0H                                                               
         L     R5,APUBIO                                                        
         USING PUBREC,R5                                                        
         MVC   8(L'PUBNAME,R2),PUBNAME                                          
         MVC   9+L'PUBZNAME(L'PUBZNAME,R2),PUBZNAME                             
FMTPN7   FOUT  (R2)                                                             
         DROP  R5                                                               
*                                                                               
FMTPNX   L     R2,FULL         MUST RESTORE R2                                  
         L     R9,SAVER7       IT'S OK TO USE SAVER7 TEMPORARILY                
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTEDT   XC    WORK(20),WORK                                                    
         MVC   DUB(6),0(R7)                                                     
         XC    DUB(4),DUB                                                       
         GOTO1 =V(PUBED),DMCB,DUB,WORK,RR=RELO05                                
         MVC   8(6,R2),WORK+1      JUST MOVE ZONE AND EDITION                   
         OC    0(6,R7),0(R7)       SEE IF PUB PRESENT                           
         BZ    FMTEDTX                                                          
         OC    4(2,R7),4(R7)       CK FOR ZONE/EDT                              
         BNZ   FMTEDTX                                                          
         MVC   8(2,R2),=C'00'      DISPLAY 00 FOR BASE PUB                      
FMTEDTX  FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
FMTINFO  DS    0H                                                               
         ST    R9,FULL                                                          
         SR    R0,R0               POINT TO NEXT FIELD                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R5,PBUYREC+33                                                    
         MVI   ELCODE,X'35'        FIND WSJ ELEM                                
         BAS   R9,NEXTEL                                                        
         BNE   FMTINFX                                                          
         USING PWSJELEM,R5                                                      
         LA    R4,8(R2)                                                         
         USING DLINE,R4                                                         
         CP    PWSJUNTS,=P'0'      SEE IF ANY COMMON UNITS                      
         BE    FMTINF10                                                         
         CLI   PBDUIND,X'89'                                                    
         BE    FMTINF5                                                          
         EDIT  (P3,PWSJUNTS),(7,NUNITS),0,TRAIL=C'L'                            
         CLI   PBDUIND,C'L'                                                     
         BE    FMTINF8                                                          
         EDIT  (P3,PWSJUNTS),(7,NUNITS),0,TRAIL=C'I'                            
         B     FMTINF8                                                          
FMTINF5  EDIT  (P3,PWSJUNTS),(7,NUNITS),2,TRAIL=C'I'                            
*                                                                               
FMTINF8  MVC   NAT,=C'AT'                                                       
*                                                                               
         EDIT  (P5,PWSJNCOS),(8,NRATE),5,ALIGN=LEFT                             
FMTINF10 ZAP   DOUBLE,PBDUNITS                                                  
         SP    DOUBLE,PWSJUNTS                                                  
         CP    DOUBLE,=P'0'     CHK FOR EXCESS UNITS                            
         BNH   FMTINF20                                                         
         CLI   PBDUIND,X'89'                                                    
         BE    FMTINF15                                                         
         EDIT  (P8,DOUBLE),(7,EUNITS),0,TRAIL=C'L'                              
         CLI   PBDUIND,C'L'                                                     
         BE    FMTINF18                                                         
         EDIT  (P8,DOUBLE),(7,EUNITS),0,TRAIL=C'I'                              
         B     FMTINF18                                                         
FMTINF15 EDIT  (P8,DOUBLE),(7,EUNITS),2,TRAIL=C'I'                              
*                                                                               
FMTINF18 MVC   EAT,=C'AT'                                                       
         EDIT  (P5,PWSJECOS),(8,ERATE),5,ALIGN=LEFT                             
*                                                                               
FMTINF20 ZAP   DOUBLE,PBDCOS                                                    
         AP    DOUBLE,PBDPRCOS     ADD PREMIUM COST                             
         EDIT  (P8,DOUBLE),(10,TCOST),2,ALIGN=LEFT                              
         AP    TOTCOST,DOUBLE                                                   
FMTINFX  L     R9,FULL                                                          
         FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
FMTTOT   DS    0H                                                               
         MVC   8(21,R2),=C'TOTAL COST OF BOOKING'                               
         EDIT  (P8,TOTCOST),(14,31(R2)),2,COMMAS=YES,ALIGN=LEFT                 
         FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
FMTRCD   DS    0H                                                               
         SR    R0,R0               POINT TO NEXT FIELD                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(4,R2),8(R2)                                                    
         MVC   8(3,R2),PBDRCODE                                                 
         FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
FMTNRCD  DS    0H                                                               
         LA    R6,REC                                                           
         ST    R9,FULL                                                          
         SR    R0,R0               POINT TO NEXT FIELD                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R5,PBUYREC+33                                                    
         MVI   ELCODE,X'35'        FIND WSJ ELEM                                
         BAS   R9,NEXTEL                                                        
         BNE   FMTNRX                                                           
         USING PWSJELEM,R5                                                      
         MVC   8(3,R2),PWSJNRTE                                                 
FMTNRX   FOUT  (R2)                                                             
         L      R9,FULL                                                         
         BR     R9                                                              
*                                                                               
         USING PJOBRECD,R6                                                      
EDALINX  CLI   PJOBUIND,X'89'                                                   
         BE    EDALIN2                                                          
         B     EDAL                                                             
         DROP  R6                                                               
*                                                                               
EDALIN   DS    0H                                                               
         CLI   PBDUIND,X'89'                                                    
         BNE   EDAL                                                             
*                                                                               
EDALIN2  DS    0H                                                               
         EDIT  (P8,DUB),(6,0(R5)),2,ALIGN=LEFT                                  
         BR    R9                                                               
*                                                                               
EDAL     DS    0H                                                               
         EDIT  (P8,DUB),(5,0(R5)),ALIGN=LEFT                                    
         BR    R9                                                               
*                                                                               
FMTD2    DS    0H                  2ND INS DATE                                 
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         ZIC   RF,0(R2)                                                         
         AHI   RF,-9                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         FOUT  (R2),=C'OPTIONAL DATA',13                                        
*                                                                               
         CLI   PBDELEM+1,X'69'     VERY OLD 'SMALL' BUYRECS                     
         BLR   R9                                                               
         CLI   PBDIDAT2,0                                                       
         BE    FMTISSNM                                                         
*                                                                               
         XC    8(13,R2),8(R2)      CLEAR 'OPTIONAL DATA'                        
         GOTO1 VDATCON,DMCB,(3,PBDIDAT2),(4,WORK)                               
         CLI   PBDFREQ,C'M'                                                     
         BNE   *+10                                                             
         MVC   WORK+3(2),=2C' '                                                 
         MVC   WORK+5(1),PBDEMIND                                               
         MVC   10(6,R2),WORK                                                    
         BR    R9                                                               
*                                                                               
FMTISSNM DS    0H                  ISSUE NAME                                   
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'A6'                                                     
         BAS   RE,NNNTEL          ISSUE NAME ELEM FOUND?                        
         BNER  R9                                                               
*                                                                               
         XC    8(13,R2),8(R2)      CLEAR 'OPTIONAL DATA'                        
*                                                                               
         MVC   10(11,R2),2(R5)                                                  
*                                                                               
         BR    R9                                                               
*                                                                               
NNNTEL   CLI   0(R5),0                                                          
         BC    8,NNNTELX                                                        
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,RE                EXIT WITH CC EQUAL                           
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
NNNTELX  LTR   R5,R5               SET CC TO NOT EQUAL                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
FMTCOM   DS    0H                                                               
         ST    RE,SAVERE           STORE RETURN REGISTER                        
         LA    R4,5                FOR BCT                                      
         MVI   ELCODE,X'66'                                                     
         LA    R5,REC+33                                                        
         LA    R6,PRVTAB                                                        
         XC    PRVTAB,PRVTAB                                                    
FMTC1    BAS   R9,NEXTEL                                                        
         BC    7,FMTC2             GO BUMP LINE                                 
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AHI   R1,-3               SET FOR EX                                   
         EX    R1,MVCOM                                                         
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
         MVI   0(R6),X'20'                                                      
*                                                                               
FMTC2    BCT   R4,FMTC3                                                         
         L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
*                                                                               
FMTC3    BAS   R9,BUMPFLD                                                       
         LA    R6,1(R6)                                                         
         B     FMTC1                                                            
*                                                                               
MVCOM    MVC   8(0,R2),2(R5)                                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTRTM   DS    0H                                                               
         ST    R7,SAVER7           SAVE R7 - USED FOR ACTION 'RM'               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   X,C' '              BLANK FILL                                   
         MVC   X+1(L'X-1),X                                                     
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         CLI   PBDCTYP,C'N'        NET INPUT SO DISPLAY AS NET                  
         BE    *+12                                                             
         CLI   PBDCOSIN,C'R'       ROADSIDE SO DISPLAY AS NET                   
         BNE   FMTRTM4                                                          
*                                                                               
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
FMTRTM4  DS    0H                                                               
         C     R1,=F'-99999999'     NEGATIVE AND OVER 999,999.99                
         BNL   FMTRTM5                                                          
         MHI   R1,-1           DROP PENNIES                                     
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MHI   R1,-1                                                            
         EDIT  (R1),(10,X+5),1,ALIGN=LEFT,FLOAT=-                               
         B     FMTRTX                                                           
*                                                                               
FMTRTM5  EDIT  (R1),(10,X+5),2,ALIGN=LEFT,FLOAT=-                               
*                                                                               
FMTRTX   DS    0H                                                               
         LA    R1,X+5                                                           
         CLI   PBDCOSIN,C' '       TEST DEFAULT COST TYPE                       
         BE    FMTRTX2                                                          
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSIN                                                 
*                                                                               
FMTRTX2  DS    0H                                                               
         CLI   PBDCTYP,C'N'        SEE IF NET INPUT                             
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCTYP                                                  
*                                                                               
         TM    PBDRLIND,X'08'      TEST FROZEN                                  
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
*                                                                               
FMTRTX4  DS    0H                                                               
         MVC   8(11,R2),0(R1)                                                   
         CP    PBDCOS,=P'0'                                                     
         BNZ   FMTRTX8                                                          
*                                                                               
         XC    8(11,R2),8(R2)                                                   
         CLI   PBDCOSIN,C'S'       TEST DEFAULT COST TYPE                       
         BE    FMTRTX5                                                          
         CLI   PBDCOSIN,C'R'                                                    
         BNE   FMTRTX6                                                          
FMTRTX5  MVC   8(1,R2),PBDCOSIN                                                 
         MVC   9(4,R2),=C'FREE'                                                 
         B     FMTRTX8                                                          
FMTRTX6  DS    0H                                                               
         MVC   8(4,R2),=C'FREE'                                                 
*                                                                               
FMTRTX8  DS    0H                                                               
         MVI   7(R2),0                                                          
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATION BIT                           
         L     R7,SAVER7           RESTORE R7 - USED FOR ACTION 'RM'            
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTTS    NTR                                                                    
         XC    DUMEL+8(10),DUMEL+8                                              
         MVI   ELCODE,X'95'                                                     
         LA    R5,REC+33                                                        
         BAS   R9,NEXTEL                                                        
         BNE   FMTTSX                                                           
         USING PTSHTELD,R5                                                      
         CLI   PTSHSTAT,C' '       SHOW NOTHING IF STATUS IS BLANK              
         BE    FMTTSX                                                           
*                                                                               
         MVC   DUMEL+8(4),=C'T/S='                                              
         MVC   DUMEL+12(1),PTSHSTAT                                             
         DROP  R5                                                               
*                                                                               
FMTTSX   B     EXITXIT                                                          
         EJECT                                                                  
*                                                                               
FMTCL    DS    0H                                                               
         LA    R3,PBDCDATE                                                      
         B     FMTDATE                                                          
*                                                                               
FMTMCL   DS    0H                                                               
         LA    R3,PBDMDATE         MATERIALS CLOSING DATE                       
         B     FMTDATE                                                          
*                                                                               
FMTSL    DS    0H                                                               
         LA    R3,PBDSDATE                                                      
*                                                                               
FMTDATE  SR    R0,R0               POINT TO NEXT FIELD                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(5,R2),8(R2)                                                    
         OC    0(3,R3),0(R3)                                                    
         BCR   8,R9                                                             
         GOTO1 VDATCON,DMCB,(3,(R3)),(7,8(R2))                                  
         MVI   7(R2),0                                                          
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATIKN BIT                           
         BR    R9                                                               
*                                                                               
EDTINS   DS    0H                                                               
         GOTO1 VEDTINS,DMCB,(RC),(RA)                                           
         CLI   ERRAREA,0                                                        
         BCR   8,R9                                                             
         B     EXXMOD                                                           
*                                                                               
FMTRTN   L     RF,VFMTRTN                                                       
         B     FMTMOVE                                                          
FMTPR    L     RF,VFMTPR                                                        
*                                                                               
FMTMOVE  LA    R0,4                                                             
         LA    R1,REC                                                           
         LA    RE,NEWREC                                                        
         MVC   0(250,RE),0(R1)     MOVE REC TO NEWREC                           
         LA    R1,250(R1)                                                       
         LA    RE,250(RE)                                                       
         BCT   R0,*-14                                                          
*                                                                               
         GOTO1 (RF),DMCB,(RC),(RA)                                              
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO FORMAT POL PRD ALLOCATIONS                                      
*                                                                               
FMTPRDS  NTR                                                                    
*                                                                               
         XC    DUMEL+8(250),DUMEL+8                                             
         SR    R0,R0                                                            
         LA    R3,DUMEL+8                                                       
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         TM    PBDWTSUM,X'80'      TEST UNEQUAL SPLIT                           
         BZ    FMTP2               NO-EQUAL                                     
         IC    R0,PBDWTSUM         GET SUM OF WEIGHTS                           
         N     R0,=F'127'                                                       
         BAS   R9,FMTPEDT                                                       
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
FMTP2    MVI   ELCODE,X'21'                                                     
         LA    R5,REC+33                                                        
         BAS   R9,NEXTEL                                                        
FMTP4    MVC   0(3,R3),2(R5)       PRD CODE                                     
         CLI   2(R3),C' '                                                       
         BNE   *+10                                                             
         MVI   2(R3),0                                                          
         BCTR  R3,0                                                             
         LA    R3,3(R3)                                                         
         TM    PBDWTSUM,X'80'      TEST UNEQUAL SPLIT                           
         BZ    FMTP6                                                            
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         SR    R0,R0                                                            
         IC    R0,3+2(R5)          COST SHARE                                   
         BAS   R9,FMTPEDT                                                       
         CLI   5(R5),0                                                          
         BNZ   *+12                                                             
         MVI   0(R3),C'0'                                                       
         LA    R3,1(R3)                                                         
         CLC   5(1,R5),6(R5)       TEST COST SHARE=SPACE SHARE                  
         BE    FMTP6                                                            
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         IC    R0,4+2(R5)          SPACE SHARE                                  
         BAS   R9,FMTPEDT                                                       
FMTP6    BAS   R9,NEXTEL                                                        
         BNE   FMTPX                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         B     FMTP4                                                            
*                                                                               
* ADD PRDS TO SVPRD LIST                                                        
*                                                                               
FMTPX    MVI   ELCODE,X'21'                                                     
         LA    R5,REC+33                                                        
FMTPX2   BAS   R9,NEXTEL                                                        
         BNE   EXXMOD                                                           
         LA    R1,SVPRDS                                                        
         LA    R0,L'SVPRDS/3                                                    
FMTPX4   CLI   0(R1),0             SLOT EMPTY                                   
         BE    FMTPX6                                                           
         CLC   0(3,R1),2(R5)       PRD MATCH                                    
         BE    FMTPX2                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,FMTPX4                                                        
         B     EXXMOD              LIST FULL                                    
FMTPX6   MVC   0(3,R1),2(R5)       ADD TO LIST                                  
         B     EXXMOD                                                           
*                                                                               
FMTDA    DS    0H                                                               
         CLI   T411FFD+1,C'*'      SEE IF DDS TERMINAL                          
         BNER  RE                  IF NOT JUST RETURN                           
*                                                                               
         CLC   =C'GST',BUYNM       SEE IF DISPLAY GST                           
         BNE   FMTDA1                                                           
         LA    R4,78(R2)                                                        
         MVC   0(4,R4),=C'GST='                                                 
         MVC   4(1,R4),PBDGST                                                   
         TM    PBDCNDA,X'01'       SEE IF PAID WITH GST                         
         BNOR  RE                                                               
         MVC   6(2,R4),=C'PD'                                                   
         BR    RE                  RETURN                                       
*                                                                               
FMTDA1   DS    0H                                                               
         MVC   FULL,KEY+27                                                      
         L     R0,FULL                                                          
         LA    R4,78(R2)                                                        
         MVC   0(8,R4),=8C'0'                                                   
         LA    R4,7(R4)                                                         
FMTDA2   LTR   R0,R0                                                            
         BCR   8,RE                                                             
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         IC    R1,HEXTAB(R1)                                                    
         STC   R1,0(R4)                                                         
         BCTR  R4,0                                                             
         B     FMTDA2                                                           
HEXTAB   DC    C'0123456789ABCDEF'                                              
         EJECT                                                                  
*                                                                               
* ON ENTRY R0 HAS INPUT, R3 HAS OUTPUT ADDRESS                                  
*                                                                               
FMTPEDT  EDIT  (R0),(3,0(R3)),ALIGN=LEFT                                        
         AR    R3,R0               POINT TO NEXT OUTPUT POSITION                
         BR    R9                                                               
*                                                                               
NEXTEL   CLI   0(R5),0                                                          
         BC    8,NEXTELX                                                        
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                EXIT WITH CC EQUAL                           
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
NEXTELX  LTR   R5,R5               SET CC TO NOT EQUAL                          
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
* FETCH ADDITIONAL DISPLAY LOGIC                                                
* NOTE - HERE ONLY AFTER SINGLE DISPLAY                                         
*                                                                               
DSPL10   TM    PBUYCNTL,X'80'      TEST DELETED                                 
         BZ    DSPL10B                                                          
         OI    WARN,X'20'          SET DELETED WARNING                          
         B     DSPL10D                                                          
DSPL10B  DS    0H                                                               
         OI    TRCODE+1,C' '                                                    
*                                                                               
DSPL10D  CLI   DDLINKSW,0          ADBUYER?                                     
         BNE   EXXMOD                                                           
         GOTO1 VCALLOV,DMCB,(6,0),(RA)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
         B     EXXMOD                                                           
*                                                                               
ALLDONE  FOUT  BUYMSGH,CMPMSG                                                   
         LA    R2,BUYPBH           PUT CURSOR TO PUB                            
         MVI   ERRAREA,C'M'        FAKE ERROR                                   
         B     EXIT                                                             
*                                                                               
CMPMSG   DC    C'REQUESTED INSERTIONS DISPLAYED'                                
*                                                                               
NOINS    FOUT  BUYMSGH,NOINSMSG                                                 
         LA    R2,BUYPBH                                                        
         MVI   ERRAREA,C'M'        FAKE ERROR                                   
         B     EXIT                                                             
*                                                                               
NOINSMSG DC    C'NO INSERTIONS ON FILE'                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DS    F                                                                
EDTJOB   DS    0H                                                               
         ST    R9,EDTJOB-4                                                      
         CLI   5(R2),0                                                          
         BE    EDTJOBX                                                          
         BAS   RE,MOVE                                                          
*                                  BUILD JOB KEY                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),BUYCL                                                   
         MVC   KEY+7(3),BUYPR                                                   
         MVC   KEY+10(6),WORK                                                   
         CLC   SVJOB,WORK                                                       
         BE    EDJ4                                                             
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+12                                                             
         LA    R3,NFNDERR                                                       
         B     ERROR                                                            
*                                                                               
         MVC   SVJOB,WORK                                                       
         MVC   SVJOBDA,KEY+27      SAVE DA                                      
         MVI   KEY+16,X'FF'                                                     
         BAS   RE,HIGH                                                          
         CLC   KEY(16),KEYSAVE                                                  
         BE    EDJ4                                                             
         XC    SVJOB(10),SVJOB     CLEAR SVJOB AND SVJOBDA                      
         LA    R3,JOBERR2          NO INSTRUCTION RECORD                        
         B     ERROR                                                            
*                                                                               
EDJ4     DS    0H                                                               
         L     R6,AJOBIO                                                        
         USING PJOBRECD,R6                                                      
*                                                                               
         CLC   KEY(16),PJOBKEY                                                  
         BE    EDJ7                ALREADY HAVE RECORD                          
         MVC   KEY+27(4),SVJOBDA                                                
         MVC   AREC,AJOBIO         READ INTO JOBIO                              
         BAS   RE,GETREC                                                        
         LA    RF,REC                                                           
         ST    RF,AREC             RESTORE AREC                                 
*                                                                               
         CLC   BINSDT,PJOBSTA      CHECK JOB DATES VS INS DATE                  
         BL    EDJ6B                                                            
         OC    PJOBEND,PJOBEND                                                  
         BZ    EDJ7                                                             
         CLC   BINSDT,PJOBEND                                                   
         BNH   EDJ7                                                             
*                                                                               
EDJ6B    DS    0H                                                               
         LA    R3,JOBERR3                                                       
         B     ERROR                                                            
*                                                                               
EDJ7     DS    0H                                                               
         ST    R2,SAVR2                                                         
         CLC   BUYPR,=C'ZZZ'                                                    
         BNE   EDJ10                                                            
         LHI   RF,7                                                             
         BAS   R9,BUMPFLDS                                                      
*                                  POINT TO ALLOC FIELD                         
         CLI   5(R2),0             CHK FOR INPUT                                
         BNE   EDJ10                                                            
         CLI   PJOBALO,C' '                                                     
         BNH   EDJ10                                                            
         MVC   8(47,R2),PJOBALO                                                 
         FOUT  (R2)                                                             
         LA    R5,47(R2)                                                        
         CLI   7(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         SR    R5,R2                                                            
         STC   R5,5(R2)            LENGTH                                       
*                                                                               
EDJ10    DS    0H                                                               
         L     R2,SAVR2            RESTORE TO JOB FIELD                         
         MVC   WSJJOB,PJOBKJOB     SAVE JOB CODE                                
         XC    WSJPREM,WSJPREM                                                  
         XC    WSJSPC,WSJSPC                                                    
         MVI   WSJSLEN,0                                                        
         CLI   PJOBPRM,C' '                                                     
         BNH   *+10                                                             
         MVC   WSJPREM,PJOBPRM     SAVE PREMIUM CODE                            
         CLI   PJOBSPC,C' '        TEST HAVE SPACE DESC                         
         BNH   EDJ26A                                                           
*                                                                               
         MVC   WSJSPC(8),PJOBSPC   YES - USE IT AND IGNORE LINES/CLMS           
         LA    R5,8                CAN NOW USE 8 CHARS                          
         LA    R1,WSJSPC+7                                                      
EDJ260   CLI   0(R1),C' '          SCAN BACKWARDS FOR NON SPACE                 
         BH    EDJ26C              SO INPUT LENGTH WILL                         
         MVI   0(R1),0             CHG SPACE TO 0                               
         BCTR  R5,0                BE SET PROPERLY                              
         BCTR  R1,0                                                             
         B     EDJ260                                                           
*                                                                               
EDJ26A   DS    0H                                                               
         LA    R5,X                WAS WSJSPC                                   
         XC    X,X                                                              
         ZAP   DUB,PJOBTUNS                                                     
         BAS   R9,EDALINX                                                       
*                                                                               
         AR    R5,R0                                                            
         CLI   PJOBUIND,X'89'                                                   
         BE    *+12                                                             
         CLI   PJOBUIND,C'I'                                                    
         BNE   *+12                                                             
         MVI   0(R5),C'I'                                                       
         LA    R5,1(R5)                                                         
         CP    PJOBCOLS,=P'0'                                                   
         BE    EDJ26B                                                           
         MVI   0(R5),C'/'                                                       
         LA    R5,1(R5)                                                         
         ZAP   DUB,PJOBCOLS                                                     
         BAS   R9,EDAL                                                          
         AR    R5,R0                                                            
*                                                                               
* IF LENGTH EXCEEDES 8 -  JUST PUT OUT TOTAL INCHES/LINES                       
*                                                                               
         LA    R0,X                                                             
         SR    R5,R0                                                            
         CHI   R5,9                                                             
         BL    EDJ26B                                                           
         XC    X,X                                                              
         LA    R5,X                                                             
         ZAP   DUB,PJOBTUNS                                                     
         BAS   R9,EDALINX                                                       
*                                                                               
         AR    R5,R0                                                            
         CLI   PJOBUIND,X'89'      LOWER CASE  I                                
         BE    *+12                                                             
         CLI   PJOBUIND,C'I'                                                    
         BNE   *+8                                                              
         MVI   0(R5),C'I'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
EDJ26B   DS    0H                                                               
         MVC   WSJSPC(8),X         MOVE EDITED SPACE                            
         LA    R0,WSJSPC                                                        
         SR    R5,R0                                                            
EDJ26C   STC   R5,WSJSLEN                                                       
*                                  NOW DO PREMIUMS                              
EDTJOBX  L     R9,EDTJOB-4                                                      
         BR    R9                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
*                                                                               
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBDIR)                                      
*                                                                               
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
*                                                                               
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
*                                                                               
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
*                                                                               
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBFILE)                                     
*                                                                               
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
*                                                                               
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),APUBIO,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
EXITXIT  XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD)            UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3               LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETBYKEY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,KEY                                                           
         MVC   0(2,R6),AGYALPHA                                                 
         MVC   2(1,R6),BUYMD                                                    
         MVI   3(R6),X'20'                                                      
         MVC   4(3,R6),BUYCL                                                    
         MVC   7(3,R6),BUYPR                                                    
         MVC   10(6,R6),BPUB                                                    
         MVC   16(3,R6),BINSDT                                                  
         MVC   19(2,R6),BEST                                                    
         MVC   24(1,R6),BSUBLN                                                  
         CLI   24(R6),0                                                         
         BNE   *+8                                                              
         MVI   24(R6),1                                                         
*                                                                               
         CLI   MADSW,C'Y'          SCRIPT UPLOAD?                               
         JNE   X_XIT1                                                           
         L     RE,ATHISTMP         POINT TO UPLOAD OBJECT                       
         LA    RE,2(RE)            POINT PASS LENGTH                            
         USING PINSD,RE                                                         
         CLC   =C'DEL',8(RE)       DELETE OBJECT?                               
         JE    *+10                                                             
         MVC   24(1,R6),PINSLINE   USE LINE NUMBER FROM UPLOAD OBJECT           
         DROP  RE                                                               
*                                                                               
X_XIT1   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DLINE    DSECT                     FOR DISPLAYING WSJ BUY INFO                  
NUNITS   DS    CL7                                                              
         DS    CL1                                                              
NAT      DS    CL2                                                              
         DS    CL1                                                              
NRATE    DS    CL8                                                              
         DS    CL1                                                              
EUNITS   DS    CL7                                                              
         DS    CL1                                                              
EAT      DS    CL2                                                              
         DS    CL1                                                              
ERATE    DS    CL8                                                              
         DS    CL1                                                              
TCOST    DS    CL10                                                             
*                                                                               
MAGLINE  DSECT                                                                  
         DS    CL8                                                              
         DS    C                                                                
MAGTR    DS    CL2                                                              
         DS    CL2                                                              
MAGDT    DS    CL8                                                              
         DS    CL2                                                              
MAGJB    DS    CL6                                                              
         DS    CL3                                                              
MAGSP    DS    CL17                                                             
         DS    CL2                                                              
MAGRT    DS    CL11                                                             
         DS    CL2                                                              
MAGCL    DS    CL5                                                              
         DS    CL3                                                              
MAGSL    DS    CL5                                                              
         DS    CL4                                                              
MAGMCL   DS    CL5                                                              
*                                                                               
NWSLINE  DSECT                                                                  
         DS    CL8                                                              
         DS    C                                                                
NWSTR    DS    CL2                                                              
         DS    CL2                                                              
NWSDT    DS    CL8                                                              
         DS    CL2                                                              
NWSJB    DS    CL6                                                              
         DS    CL3                                                              
NWSLNS   DS    CL8                                                              
         DS    CL7                                                              
NWSRT    DS    CL10                                                             
         DS    CL5                                                              
NWSPR    DS    CL11                                                             
         DS    CL8                                                              
NWSMCL   DS    CL5                                                              
*                                                                               
ODRLINE  DSECT                                                                  
         DS    CL8                                                              
         DS    C                                                                
ODRTR    DS    CL2                                                              
         DS    CL2                                                              
ODRDT    DS    CL8                                                              
         DS    CL2                                                              
ODRJB    DS    CL6                                                              
         DS    CL5                                                              
ODRSHOW  DS    CL3                                                              
         DS    CL3                                                              
ODRREG   DS    CL4                                                              
         DS    CL3                                                              
ODRILL   DS    CL4                                                              
         DS    CL5                                                              
ODRRT    DS    CL11                                                             
         DS    CL3                                                              
ODRCL    DS    CL5                                                              
         DS    CL6                                                              
ODRMCL   DS    CL5                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   REC                 MAP BUY RECORD TO REC                        
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059PPBUY05   02/26/20'                                      
         END                                                                    
