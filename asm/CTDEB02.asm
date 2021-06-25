*          DATA SET CTDEB02    AT LEVEL 003 AS OF 01/09/13                      
*&&      SET   NOP=N                                                            
*PHASE TA0F02A                                                                  
         TITLE 'CTDEB02 - DEBUG MODULE'                                         
CTDEB02  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DB2**,RA                                                     
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         USING CTDEBFFD,R8         R8=A(TWA)                                    
         USING SAVED,R7            R7=A(SAVED)                                  
*                                                                               
         BAS   RE,MAIN                                                          
*                                                                               
XMOD1    XMOD1 1                   EXIT PROGRAM                                 
*                                                                               
XITEQU   CR    RB,RB               EXIT CC EQU                                  
         B     XIT1                                                             
XITNEQ   LTR   RB,RB               EXIT CC NEQ                                  
*                                                                               
XIT1     XIT1                      EXIT                                         
*                                                                               
AREGON   CLI   ADDFLAG,0           CHECK FOR DATASPACE                          
         BER   RE                                                               
         SAC   512                                                              
         BR    RE                                                               
AREGOFF  CLI   ADDFLAG,0           CHECK FOR DATASPACE                          
         BER   RE                                                               
         SAC   0                                                                
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM                                       *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
*                                                                               
MAIN001  CLI   FLAG1,FLNEWQ        RESET PAGES ON NEW ACTION                    
         BNE   *+12                                                             
         MVI   PAGES,0                                                          
         B     MAIN002             AND CLEAR SCREEN                             
*                                                                               
         CLI   PFKEY,0             IF NOT ENTER                                 
         BE    MAIN003                                                          
MAIN002  TWAXC DEBSELH,PROT=Y      CLEAR SCREEN                                 
*                                                                               
MAIN003  CLI   PFKEY,7             UP PFKEY                                     
         BNE   MAIN004                                                          
         SR    R1,R1                                                            
         IC    R1,PAGES                                                         
         BCTR  R1,0                                                             
         STC   R1,PAGES                                                         
         CLI   PAGES,X'FF'        CHECK FOR NEGATIVE                            
         BNE   *+8                                                              
         MVI   PAGES,0                                                          
*                                                                               
MAIN004  CLI   PFKEY,8            DOWN PFKEY                                    
         BNE   MAIN005                                                          
         TM    PAGEFLG,PAGENDQ    NOT VALID ON THIS PAGE                        
         BO    MAIN005                                                          
         SR    R1,R1                                                            
         IC    R1,PAGES                                                         
         LA    R1,1(R1)                                                         
         STC   R1,PAGES                                                         
*                                                                               
MAIN005  NI    PAGEFLG,255-PAGENDQ                                              
*                                                                               
         CLI   ACTION,7                                                         
         BNE   MAIN010                                                          
         BAS   RE,LISTMOD                                                       
         B     MAIN990                                                          
*                                                                               
MAIN010  CLI   ACTION,8                                                         
         BNE   MAIN020                                                          
         BAS   RE,LISTUSE                                                       
         B     MAIN990                                                          
*                                                                               
MAIN020  CLI   ACTION,9                                                         
         BNE   MAIN030                                                          
         BAS   RE,LISTADD                                                       
         B     MAIN990                                                          
*                                                                               
MAIN030  CLI   ACTION,10                                                        
         BNE   MAIN040                                                          
         BAS   RE,LISTREG                                                       
         B     MAIN990                                                          
*                                                                               
MAIN040  CLI   ACTION,11                                                        
         BNE   MAIN050                                                          
         BAS   RE,LISTEQU                                                       
         B     MAIN990                                                          
*                                                                               
MAIN050  CLI   ACTION,12                                                        
         BNE   MAIN060                                                          
         BAS   RE,LISTRAP                                                       
         B     MAIN990                                                          
*                                                                               
MAIN060  CLI   ACTION,13                                                        
         BNE   MAIN990                                                          
         BAS   RE,LISTPAT                                                       
         B     MAIN990                                                          
*                                                                               
MAIN990  EQU   *                                                                
*                                                                               
MAINX    B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        LIST MODULES                                       *                   
*************************************************************                   
         SPACE 1                                                                
LISTMOD  NTR1                                                                   
*                                                                               
         MVI   TWAPAGE,2           GET MODULE DETAILS                           
         GOTO1 AREADTWA                                                         
*                                                                               
         LA    R3,DEBLIN                                                        
         USING SELINED,R3                                                       
*                                                                               
         L     R2,ATIA                                                          
         CLC   0(8,R2),=C'**MODS**'                                             
         BNE   XITNEQ                                                           
         LA    R2,8(R2)                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PAGES                                                         
         MH    R1,=H'20'                                                        
         MH    R1,=H'20'                                                        
         AR    R2,R1                                                            
*                                                                               
LMOD010  CLC   0(8,R2),=C'**ENDM**'                                             
         BNE   LMOD020                                                          
         OI    PAGEFLG,PAGENDQ     SET LAST PAGE FLAG                           
         B     LMODX                                                            
*                                                                               
LMOD020  MVC   0(8,R3),0(R2)                                                    
         MVC   9(1,R3),14(R2)                                                   
         MVC   10(4,R3),=C'SECT'                                                
         LA    R1,16(R2)                                                        
         ST    R1,DMCB                                                          
         LA    R1,15(R3)                                                        
         ST    R1,DMCB+4                                                        
         GOTO1 AHEXOUT,DMCB,,,4                                                 
*                                                                               
         LA    R2,20(R2)                                                        
         OI    LINHDR+6,X'80'                                                   
         LA    R3,8+3+8+75(R3)                                                  
         LA    RE,DEBSXXH                                                       
         CR    R3,RE                                                            
         BNH   LMOD010                                                          
*                                                                               
LMODX    B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        LIST USINGS                                        *                   
*************************************************************                   
         SPACE 1                                                                
LISTUSE  NTR1                                                                   
*                                                                               
         LA    R3,DEBLIN                                                        
         USING SELINED,R3                                                       
         LA    R2,USINGS                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PAGES                                                         
         MH    R1,=H'20'                                                        
         MH    R1,=H'16'                                                        
         AR    R2,R1                                                            
*                                                                               
LUSE010  LA    RF,USINGS                                                        
         AH    RF,=Y(USINGX-USINGS)                                             
         CR    R2,RF                                                            
         BNH   LUSE015                                                          
         OI    PAGEFLG,PAGENDQ     SET LAST PAGE FLAG                           
         B     LUSEX                                                            
*                                                                               
LUSE015  OC    0(16,R2),0(R2)                                                   
         BNZ   LUSE020                                                          
         LA    R2,16(R2)                                                        
         B     LUSE010                                                          
*                                                                               
LUSE020  MVC   0(8,R3),0(R2)                                                    
         CLI   8(R2),0                                                          
         BE    LUSE025                                                          
         MVC   15(8,R3),8(R2)                                                   
         B     LUSE030                                                          
*                                                                               
LUSE025  LA    R1,12(R2)                                                        
         ST    R1,DMCB                                                          
         LA    R1,15(R3)                                                        
         ST    R1,DMCB+4                                                        
         GOTO1 AHEXOUT,DMCB,,,4                                                 
*                                                                               
LUSE030  LA    R2,16(R2)                                                        
         OI    LINHDR+6,X'80'                                                   
         LA    R3,8+3+8+75(R3)                                                  
         LA    RE,DEBSXXH                                                       
         CR    R3,RE                                                            
         BNH   LUSE010                                                          
*                                                                               
LUSEX    B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        LIST ADDRESSES                                     *                   
*************************************************************                   
         SPACE 1                                                                
LISTADD  NTR1                                                                   
         LA    R3,DEBSELH                                                       
         USING SELINED,R3                                                       
*                                                                               
         MVI   TWAPAGE,1                                                        
         GOTO1 AREADTWA                                                         
         L     R2,ATIA                                                          
         CLC   0(8,R2),=C'**ADDR**'                                             
         BNE   LADDX                                                            
         LA    R2,8(R2)                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PAGES                                                         
         MH    R1,=H'12'                                                        
         MH    R1,=H'20'                                                        
         AR    R2,R1                                                            
*                                                                               
LADD010  CLC   0(8,R2),=C'**ENDA**'                                             
         BNE   LADD020                                                          
         OI    PAGEFLG,PAGENDQ     SET LAST PAGE FLAG                           
         B     LADDX                                                            
*                                                                               
LADD020  CLI   SELHDR+5,0          ANY INPUT                                    
         BE    LADD030                                                          
         CLI   SELSEL,C'S'         MUST BE S FOR SEL                            
         BNE   LADD030                                                          
         MVC   ADDRESS,8(R2)       SET ADDRESS                                  
         MVI   ACTION,4            ACTION DISPLAY                               
         OI    FLAG1,FLRETQ        FLAG RETURN                                  
         B     LADDX                                                            
*                                                                               
LADD030  MVC   LINLABL,0(R2)       SHOW NAME                                    
         LA    R1,8(R2)                                                         
         ST    R1,DMCB                                                          
         LA    R1,LINADDR                                                       
         ST    R1,DMCB+4                                                        
         GOTO1 AHEXOUT,DMCB,,,4    SHOW ADDRESS                                 
*                                                                               
         LA    R2,12(R2)           NEXT                                         
         OI    LINHDR+6,X'80'                                                   
         LA    R3,8+3+8+75(R3)                                                  
         LA    RE,DEBSXXH                                                       
         CR    R3,RE                                                            
         BNH   LADD010                                                          
*                                                                               
LADDX    B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        LIST EQUS                                          *                   
*************************************************************                   
         SPACE 1                                                                
LISTEQU  NTR1                                                                   
         LA    R3,DEBSELH                                                       
         USING SELINED,R3                                                       
*                                                                               
         LA    R2,USERADS                                                       
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PAGES                                                         
         MH    R1,=H'12'                                                        
         MH    R1,=H'20'                                                        
         AR    R2,R1                                                            
*                                                                               
LEQU010  LA    RF,USERADX                                                       
         CR    R2,RF                                                            
         BNE   LEQU020                                                          
         OI    PAGEFLG,PAGENDQ     SET LAST PAGE FLAG                           
         B     LEQUX                                                            
*                                                                               
LEQU020  CLI   SELHDR+5,0          ANY INPUT                                    
         BE    LEQU030                                                          
         CLI   SELSEL,C'S'         MUST BE S FOR SEL                            
         BNE   LEQU030                                                          
         MVC   ADDRESS,8(R2)       SET ADDRESS                                  
         MVI   ACTION,4            ACTION DISPLAY                               
         OI    FLAG1,FLRETQ        FLAG RETURN                                  
         B     LEQUX                                                            
*                                                                               
LEQU030  MVC   LINLABL,0(R2)       SHOW NAME                                    
         LA    R1,8(R2)                                                         
         ST    R1,DMCB                                                          
         LA    R1,LINADDR                                                       
         ST    R1,DMCB+4                                                        
         GOTO1 AHEXOUT,DMCB,,,4    SHOW ADDRESS                                 
*                                                                               
         LA    R2,12(R2)           NEXT                                         
         OI    LINHDR+6,X'80'                                                   
         LA    R3,8+3+8+75(R3)                                                  
         LA    RE,DEBSXXH                                                       
         CR    R3,RE                                                            
         BNH   LEQU010                                                          
*                                                                               
LEQUX    B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        LIST REGS                                          *                   
*************************************************************                   
         SPACE 1                                                                
LISTREG  NTR1                                                                   
         LA    R3,DEBSELH                                                       
         USING SELINED,R3                                                       
*                                                                               
         L     R2,DBTCB            FIND DEBUG TCB                               
LREG010  LA    R2,TCBPSW+8-TCBD(R2)                                             
*                                                                               
LREG015  OI    PAGEFLG,PAGENDQ     SET LAST PAGE FLAG                           
         MVC   LINLABL(16),=C'REGISTERS       '                                 
         MVI   BYTE,0                                                           
*                                                                               
LREG030  CLI   BYTE,4                                                           
         BE    LREG050                                                          
         CLI   BYTE,3                                                           
         BNE   *+10                                                             
         MVC   LINLABL+13(3),=C'C-F'                                            
         CLI   BYTE,2                                                           
         BNE   *+10                                                             
         MVC   LINLABL+13(3),=C'8-B'                                            
         CLI   BYTE,1                                                           
         BNE   *+10                                                             
         MVC   LINLABL+13(3),=C'4-7'                                            
         CLI   BYTE,0                                                           
         BNE   *+10                                                             
         MVC   LINLABL+13(3),=C'0-3'                                            
*                                                                               
         LA    R4,LINLABL+20                                                    
         LA    R0,4                                                             
LREG040  GOTO1 AHEXOUT,DMCB,(R2),(R4),4    HEXOUT REGISTER                      
*                                                                               
         LA    R2,4(R2)            NEXT                                         
         LA    R4,12(R4)                                                        
         BCT   R0,LREG040                                                       
         OI    LINHDR+6,X'80'                                                   
         LA    R3,8+3+8+75(R3)                                                  
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         B     LREG030                                                          
*                                                                               
LREG050  OI    LINHDR+6,X'80'                                                   
         LA    R3,8+3+8+75(R3)                                                  
         MVC   LINLABL+13(7),=C'PSW=   '                                        
         LA    R4,LINLABL+20                                                    
         L     R2,DBTCB                                                         
         LA    R2,TCBPSW-TCBD(R2)                                               
         GOTO1 AHEXOUT,DMCB,(R2),(R4),4    HEXOUT PSW                           
         LA    R2,4(R2)                                                         
         LA    R4,9(R4)                                                         
         GOTO1 AHEXOUT,DMCB,(R2),(R4),4    HEXOUT PSW                           
*                                                                               
         OI    LINHDR+6,X'80'                                                   
         LA    R3,8+3+8+75(R3)                                                  
         OI    LINHDR+6,X'80'                                                   
         LA    R3,8+3+8+75(R3)                                                  
         L     R2,DBTCB                                                         
         MVC   DUB+0(4),TCBPSW+4-TCBD(R2)                                       
         MVC   DUB+4(4),TCBPSW+52-TCBD(R2)                                      
         L     R1,DUB+0                                                         
         S     R1,DUB+4                                                         
         ST    R1,FULL                     CALC PSW-RB                          
*                                                                               
         MVC   LINLABL+13(33),=C'YOU STOPPED IN ******** AT ******'             
         GOTO1 AHEXOUT,DMCB,FULL+1,LINLABL+40,3                                 
         L     R1,DUB+4                                                         
         MVC   LINLABL+28(8),22(R1)                                             
*                                                                               
LREGX    B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        TRAPS                                              *                   
*************************************************************                   
         SPACE 1                                                                
LISTRAP  NTR1                                                                   
*&&NOP                                                                          
         LA    R3,DEBSE2H                                                       
         USING TRAPLINE,R3                                                      
         L     R4,=A(TRAPS-SAVED)                                               
         AR    R4,R7                                                            
         USING TRAPSD,R4                                                        
*                                                                               
TRAPS010 CLI   TRAPLABH+5,0        CHECK REPLACE LABLE                          
         BE    TRAPS011                                                         
         MVC   TRAPSLAB,TRAPLAB                                                 
         MVC   INF01(8),TRAPLAB                                                 
*        GOTO1 ALOCATE                                                          
*        ??                                                                     
*                                                                               
TRAPS011 CLI   TRAPTYPH+5,0        CHECK REPLACE TYPE                           
         BE    *+10                                                             
         MVC   TRAPSTYP(3),TRAPTYP                                              
*                                                                               
         CLI   TRAPLENH+5,0        CHECK REPLACE LENGTH                         
         BE    TRAPS012                                                         
         LA    R1,TRAPLEN                                                       
         GOTO1 AVALNUM                                                          
         STC   R1,TRAPSLEN                                                      
*                                                                               
TRAPS012 CLI   TRAPADRH+5,0        CHECK REPLACE ADR                            
         BE    TRAPS015                                                         
         TM    TRAPADRH+4,X'02'    CHECK VALID HEX                              
         BO    TRAPS014                                                         
*                                                                               
         LA    R1,TRAPADR                                                       
         GOTO1 AVALNUM                                                          
         LA    R0,4                                                             
         LA    RF,TRAPADR                                                       
TRAPS12A CLC   0(2,RF),=C'(R'                                                   
         BE    TRAPS12B                                                         
         CLC   0(4,RF),=C'(PSW'                                                 
         BE    TRAPS12C                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,TRAPS12A                                                      
         B     TRAPS015                                                         
*                                                                               
TRAPS12B IC    RE,2(RF)                                                         
         N     RE,=X'0000000F'                                                  
         CLI   2(RF),X'C6'                                                      
         BH    *+8                                                              
         LA    RE,9(RE)                                                         
         STH   R1,14(R4)                                                        
         STC   RE,12(R4)                                                        
         MVI   16(R4),C'R'                                                      
         B     TRAPS015                                                         
*                                                                               
TRAPS12C STH   R1,14(R4)                                                        
         MVI   12(R4),C'P'                                                      
         MVI   16(R4),C'P'                                                      
         B     TRAPS015                                                         
*                                                                               
TRAPS014 GOTO1 AHEXIN,DMCB,TRAPADR,TRAPSADR,8                                   
         MVI   16(R4),C'A'                                                      
*                                                                               
TRAPS015 CLI   TRAPDATH+5,0        ANY DATA INPUT                               
         BE    TRAPS050                                                         
         CLI   8(R4),C'A'                                                       
         BNE   TRAPS020                                                         
         MVC   TRAPSDAT,TRAPDAT                                                 
         B     TRAPS050                                                         
*                                                                               
TRAPS020 GOTO1 AHEXIN,DMCB,TRAPDAT,TRAPSDAT,24                                  
*                                                                               
TRAPS050 LA    R3,TRAPLLEN(R3)                                                  
         LA    R4,L'TRAPSLIN(R4)                                                
         L     RF,=A(TRAPSX-SAVED)                                              
         AR    RF,R7                                                            
         CR    R4,RF                                                            
         BL    TRAPS010                                                         
*                                                                               
         L     R4,=A(TRAPS-SAVED)                                               
         AR    R4,R7                                                            
         LA    R3,DEBSE2H                                                       
TRAPS110 OC    TRAPSLIN,TRAPSLIN                                                
         BZ    TRAPS150                                                         
         MVC   TRAPLAB,TRAPSLAB                                                 
         OI    TRAPLABH+6,X'80'                                                 
*                                                                               
         MVC   TRAPTYP(3),TRAPSTYP                                              
         OI    TRAPTYPH+6,X'80'                                                 
*                                                                               
         EDIT  (B1,TRAPSLEN),(3,TRAPLEN),FILL=0                                 
         OI    TRAPLENH+6,X'80'                                                 
*                                                                               
         CLI   TRAPSATY,C'R'                                                    
         BNE   TRAPS120                                                         
         EDIT  (B1,TRAPSADR+3),(3,TRAPADR),FILL=0                               
         MVC   TRAPADR+3(2),=C'(R'                                              
         GOTO1 AHEXOUT,DMCB,TRAPSADR,TRAPADR+5,1                                
         MVC   TRAPADR+5(1),TRAPADR+6                                           
         MVI   TRAPADR+6,C')'                                                   
         OI    TRAPADRH+6,X'80'                                                 
         B     TRAPS125                                                         
*                                                                               
TRAPS120 GOTO1 AHEXOUT,DMCB,TRAPSADR,TRAPADR,4                                  
         OI    TRAPADRH+6,X'80'                                                 
*                                                                               
TRAPS125 CLI   TRAPTYP,C'A'                                                     
         BNE   TRAPS130                                                         
         MVC   TRAPDAT,TRAPSDAT                                                 
         B     TRAPS150                                                         
*                                                                               
TRAPS130 GOTO1 AHEXOUT,DMCB,TRAPSDAT,TRAPDAT,24                                 
*                                                                               
TRAPS150 OI    TRAPDATH+6,X'80'                                                 
         LA    R3,TRAPLLEN(R3)                                                  
         LA    R4,L'TRAPSLIN(R4)                                                
         L     RF,=A(TRAPSX-SAVED)                                              
         AR    RF,R7                                                            
         CR    R4,RF                                                            
         BL    TRAPS110                                                         
*                                                                               
*&&                                                                             
LISTRAPX B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        LIST PATCH AREA                                    *                   
*************************************************************                   
         SPACE 1                                                                
LISTPAT  NTR1                                                                   
         LA    R3,DEBSELH                                                       
         USING SELINED,R3                                                       
         MVC   DEBHDR1,DASHDET                                                  
         OI    DEBHDR1H+6,X'80'    TRANSMIT                                     
*                                                                               
         ICM   R2,15,APATCHAR                                                   
         BZ    LISTPATX                                                         
         USING PATCHWKD,R2                                                      
*                                                                               
         L     R1,ASSB                                                          
         LAM   AR2,AR2,SSBPGMTA-SSBD(R1)                                        
         LAM   AR4,AR4,SSBPGMTA-SSBD(R1)                                        
         LAM   AR5,AR5,SSBPGMTA-SSBD(R1)                                        
         SAM31                                                                  
         SAC   512                                                              
*                                                                               
         ICM   R4,15,PATCHPHS      SET R4 TO PHASE                              
         BZ    LISTPATX                                                         
*                                                                               
         MVC   LINLABL,22(R4)      SET LABLE                                    
         A     R4,PATCHOFS                                                      
         MVC   LINMAIN(40),0(R4)   AND LEVEL                                    
         S     R4,PATCHOFS                                                      
         LA    R3,8+3+8+75(R3)                                                  
*                                                                               
         ICM   R5,15,BASEADDR      SET R5 TO PATCHWORK                          
         BZ    LISTPATX                                                         
*                                                                               
         LA    R2,PATCHDET         PATCH DETAIL                                 
         LR    RF,R2                                                            
LPAT005  OC    0(4,R2),0(R2)                                                    
         BZ    LPAT006             FIND LAST ENTRY                              
         AHI   R2,4                                                             
         B     LPAT005                                                          
*                                                                               
LPAT006  SR    R2,RF               CALCULATE PAGES OF PATCHES                   
         LR    R1,R2                                                            
         SRL   R1,2                4 BYTES PER PATCH                            
         SRL   R1,3                8 PATCHES PER PAGE                           
         LA    R1,1(R1)                                                         
         STC   R1,BYTE             SAVE IT IN BYTE                              
         LR    R2,RF                                                            
*                                                                               
         CLC   PAGES,BYTE                                                       
         BH    LPAT020                                                          
*                                                                               
         SR    R1,R1               INDEX INTO PAGED DISPLAY                     
         IC    R1,PAGES                                                         
         SLL   R1,2                PAGES * 4                                    
         SLL   R1,3                8 PER PAGE                                   
         AR    R2,R1                                                            
         OC    0(4,R2),0(R2)                                                    
         BZ    LPAT020                                                          
*                                                                               
LPAT010  OI    LINHDR+6,X'80'                                                   
         XC    WORK(4),WORK        HEXOUT FROM DETAILS                          
         MVC   WORK+2(2),0(R2)                                                  
         SAC   0                                                                
         GOTO1 AHEXOUT,DMCB,WORK,LINLABL,4                                      
         SAC   512                                                              
         MVC   LINLABL(4),=C'FRM '                                              
*                                                                               
         SR    R1,R1               HEXOUT TO DETAILS                            
         SR    RF,RF                                                            
         ICM   R1,3,0(R2)                                                       
         BZ    LPAT020                                                          
         ICM   RF,3,2(R2)                                                       
         AR    R4,R1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK,0(R4)                                                       
         SR    R4,R1                                                            
         AR    R5,R1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK1,0(R5)                                                      
         SR    R5,R1                                                            
         SAC   0                                                                
         GOTO1 AHEXOUT,DMCB,WORK,LINMAIN,(RF)                                   
         SAC   512                                                              
         MVC   DUB,LINLABL                                                      
         LA    R3,8+3+8+75(R3)                                                  
         MVC   LINLABL,DUB                                                      
         MVC   LINLABL(4),=C'TO  '                                              
         SAC   0                                                                
         GOTO1 AHEXOUT,DMCB,WORK1,LINMAIN                                       
         SAC   512                                                              
*                                                                               
         LA    R2,4(,R2)           NEXT LINE                                    
         LA    R3,8+3+8+75(R3)                                                  
         LA    RE,DEBSXXH                                                       
         CR    R3,RE                                                            
         BNH   LPAT010                                                          
*                                                                               
LPAT020  ICM   R2,15,APATCHAR      NO SHOW SYSTEM RETURNS                       
         LA    R2,PATCHSYS                                                      
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ICM   R1,1,PAGES                                                       
         ICM   R0,1,BYTE           BYTE IS PAGES FOR PATCH DETAIL               
         SR    R1,R0                                                            
         BP    *+6                                                              
         XR    R1,R1                                                            
         SLL   R1,8                16 * 16 PER PAGE                             
         AR    R2,R1                                                            
*                                                                               
         MVC   LINLABL,DASHES                                                   
         MVC   LINMAIN,DASHPAT                                                  
         OI    LINHDR+1,X'08'      HIGH INTENSITY                               
         B     LPAT026                                                          
*                                                                               
LPAT025  MVC   LINLABL,=C'....'                                                 
         OI    LINHDR+6,X'80'      TRANSMIT                                     
         OI    SELHDR+6,X'80'                                                   
*                                                                               
         OC    0(4,R2),0(R2)                                                    
         BZ    LPAT025B                                                         
         MVC   LINLABL(4),0(R2)    ADV                                          
         MVC   WORK1(4),4(R2)      ADDRESS                                      
         SAC   0                                                                
         GOTO1 AHEXOUT,DMCB,WORK1,LINMAIN,4                                     
         SAC   512                                                              
*                                                                               
         CLI   PFKEY,0             WAS ENTER HIT                                
         BNE   LPAT025A                                                         
         CLI   SELHDR+5,0          ANY INPUT                                    
         BE    LPAT025A                                                         
         CLI   SELSEL,C'S'         MUST BE S FOR SEL                            
         BNE   *+12                                                             
         OI    12(R2),X'80'        FLAG TO DO THIS PATCH                        
         B     LPAT025A                                                         
*                                                                               
         CLI   SELSEL,C'X'         OR X TO REMOVE                               
         BNE   LPAT025A                                                         
         NI    12(R2),255-X'80'    REMOVE PATCH FLAG                            
*                                                                               
LPAT025A MVC   SELSEL,SPACES                                                    
         MVC   LINMAIN+10(5),SPACES                                             
         TM    12(R2),X'80'                                                     
         BZ    *+10                                                             
         MVC   LINMAIN+10(5),=C'PATCH'                                          
*                                                                               
LPAT025B LA    R2,16(R2)                                                        
LPAT026  LA    R3,8+3+8+75(R3)     NEXT LINE                                    
         LA    RE,DEBSXXH                                                       
         CR    R3,RE                                                            
         BNH   LPAT025                                                          
*                                                                               
LPAT990  SAM24                                                                  
         SAC   0                                                                
         MVC   MYINFO,=H'2'        SELECT PATCHES THEN ENTER PATCH              
*                                                                               
LISTPATX B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        ERROR EXITS                                        *                   
*************************************************************                   
         SPACE 1                                                                
ERR1     MVC   ERROR,=H'11'                                                     
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LITERALS                               *                   
*************************************************************                   
         SPACE 1                                                                
DASHES   DC    70C'-'                                                           
SPACES   DC    70C' '                                                           
DASHDET  DC    C'------------- Patch detail ----------'                         
         DC    40C'-'                                                           
DASHPAT  DC    C'Select patches to apply then enter PATCH --'                   
         DC    20C'-'                                                           
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DSECTS                                             *                   
*************************************************************                   
         SPACE 1                                                                
SELINED  DSECT                                                                  
SELHDR   DS    XL8                                                              
SELSEL   DS    CL3                                                              
LINHDR   DS    CL8                                                              
LINLIN   DS    0CL75                                                            
LINLABL  DS    CL8                                                              
         DS    CL2                                                              
LINMAIN  DS    0CL65                                                            
LINADDR  DS    CL8                                                              
         DS    CL8                                                              
         EJECT                                                                  
TRAPLINE DSECT                                                                  
TRAPSELH DS    XL8                                                              
TRAPSEL  DS    CL3                                                              
TRAPLABH DS    XL8                                                              
TRAPLAB  DS    CL8                                                              
TRAPTYPH DS    XL8                                                              
TRAPTYP  DS    CL3                                                              
TRAPLENH DS    XL8                                                              
TRAPLEN  DS    CL3                                                              
TRAPADRH DS    XL8                                                              
TRAPADR  DS    CL8                                                              
TRAPDATH DS    XL8                                                              
TRAPDAT  DS    CL48                                                             
TRAPLLEN EQU   *-TRAPLINE                                                       
         EJECT                                                                  
*CTDEBWORK                                                                      
       ++INCLUDE CTDEBWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTDEB02   01/09/13'                                      
         END                                                                    
