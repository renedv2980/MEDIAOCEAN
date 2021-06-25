*          DATA SET SPOMS21    AT LEVEL 020 AS OF 01/16/07                      
*PHASE T23421A                                                                  
T23421   TITLE 'SPOMS21 - CLIENT ORDER REPORT'                                  
*                                                                               
T23421   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T23421*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING SAVED,R5                                                         
         ST    R3,RELO                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         GOTO1 ADDAY,DMCB,WORK,DUB,-14                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,TDYLSS2W)                                 
         XC    TDYLSS2W,=X'FFFF'    **MUST COMPLEMENT!!                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         MVI   MISCFLG1,0                                                       
         MVI   MISCFLG2,0                                                       
         MVI   MISCFLG3,0                                                       
         MVI   FILTFLG1,0                                                       
         MVI   FILTFLG2,0                                                       
         MVI   CGFLAG,0                                                         
         LA    R2,CONWHENH                                                      
         CLI   CONWHENH+5,0        MUST HAVE REPORT ID                          
         BE    MISSFLD                                                          
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE              
         BNZ   *+8                                                              
*                                                                               
         L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
*                                                                               
         MVC   CMVMEDN,SPACES      CLEAR MEDIA NAME                             
         OI    CMVMEDNH+6,X'80'                                                 
***************                                                                 
* VALIDATE THE OPTIONS                                                          
***************                                                                 
VKOPT00  DS    0H                                                               
*                                                                               
         LA    R2,CMVOPTSH                                                      
         CLI   5(R2),0             LENGTH OF INPUT = 0?                         
*        BE    VKOPTX               - YEA, NO INPUT, END IT                     
         B     VKOPTX              SKIP THIS NO MATTER WHAT                     
*&&DO                                                                           
         OI    6(R2),X'80'         TRANSMIT                                     
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE              
         BNZ   VKOPT50                                                          
         OI    TIOBINDS,TIOBSETC   SET CURSOR                                   
*                                                                               
         SR    R2,RA                                                            
         STH   R2,TIOBCURD         SAVE DISPLACEMENT OF FIELD FROM TWA          
         LA    R2,CMVOPTSH         PUT R2 BACK AT THE FIELD                     
*                                                                               
VKOPT50  CLI   5(R2),0             IS THERE ANY INPUT?                          
         BE    VKOPTX               - NO, END IT                                
         CLC   =C'ESTDATE=',CMVOPTS                                             
         BE    VKOPTEST                                                         
         BNE   INVLOPT                                                          
*                                                                               
VKOPTEST OI    FILTFLG1,FF1DATE1   WE HAVE A DATE FILTER                        
         BAS   RE,DSETUP           SETUP UP DATES FILTER FOR COMPARE            
         BE    VKOPTX               - NO PROBLEMS                               
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE(S)           
         BNZ   VKOPTX                                                           
         MVC   TIOBCURI,THISDIST                                                
         B     INVLDATE                                                         
*&&                                                                             
VKOPTX   DS    0H                                                               
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
***************                                                                 
* VALIDATE THE MEDIA                                                            
***************                                                                 
VKMED00  DS    0H                                                               
*                                                                               
         LA    R2,CMVMEDKH                                                      
         CLI   5(R2),0             LENGTH OF INPUT = 0?                         
         BE    NEEDFLDS                                                         
         GOTO1 VALIMED             VALIDATE MEDIA                               
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
VKMEDX   LA    R2,CMVMEDNH         DISPLAY MEDIA NAME                           
         MVC   CMVMEDN(L'MEDNM),MEDNM                                           
***************                                                                 
* VALIDATE THE CLIENT                                                           
***************                                                                 
VKCLT00  DS    0H                                                               
*                                                                               
         BAS   RE,CLRSCBLK                                                      
*                                                                               
         LA    R2,CMVCLTKH                                                      
         LA    R3,CLTTAB                                                        
         USING CLTTABD,R3                                                       
*                                                                               
         CLI   5(R2),0             ANY CLIENTS?                                 
         BE    NEEDFLDS             - NO, ERROR                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINES             
         BNZ   VKCLT40                                                          
         OI    TIOBINDS,TIOBSETC   SET CURSOR                                   
*                                                                               
         SR    R2,RA                                                            
         STH   R2,TIOBCURD         SAVE DISPLACEMENT OF FIELD FROM TWA          
         LA    R2,CMVCLTKH         PUT R2 BACK AT THE FIELD                     
*                                                                               
VKCLT40  GOTO1 SCANNER,DMCB,CMVCLTKH,(X'80',SCANBLK)                            
*                                                                               
         LA    R6,SCANBLK                                                       
         USING SCANBLKD,R6                                                      
VKCLT50  CLI   SC1STLEN,0          IS THERE ANY INPUT?                          
         BE    VKCLTX               - NO, END IT                                
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE(S)           
         BNZ   *+10                                                             
         MVC   TIOBCURI,SC1STNUM   TO PLACE CURSOR AT THE RIGHT PLACE           
         CLI   SC1STLEN,3          IS IT ONLY 3 BYTES OF INPUT?                 
         BH    INVLFLD                                                          
         CLC   SC1STFLD(3),=C'ALL'   ARE WE DOING ALL CLIENTS?                  
         BNE   VKCLT55              - NOPES, WE'RE NOT                          
         CLC   =C'OV',CONWHEN      LET'S SEE IF THIS IS OVERNIGHT               
         BE    VKCLT53              - IT IS, CONTINUE NORMALLY                  
*                                                                               
VKCLT52  OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE(S)           
         BNZ   *+8                                                              
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         LA    R2,CONWHENH         MAKE R2 POINT TO "PRINT" OPTION              
         B     NOTOVREQ                                                         
*                                                                               
VKCLT53  OI    MISCFLG3,MF3CLTAL   TURN ON ALL CLIENT FLAG                      
         B     VKCLTX                                                           
*                                                                               
VKCLT55  CLC   SC1STFLD(3),=C'CGR'   ARE WE DOING CLIENT GROUP?                 
         BNE   VKCLT60              - NOPE, WE'RE NOT                           
         CLC   =C'OV',CONWHEN      LET'S SEE IF THIS IS OVERNIGHT               
         BNE   VKCLT52              - NOPE, GO TO ERROR                         
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE(S)           
         BNZ   *+10                                                             
         MVC   TIOBCURI,SC2NDNUM   TO PLACE CURSOR AT THE RIGHT PLACE           
*****  NOTE:  EXAMPLE OF A VALID CLIENT GROUP  -->  D1234                       
         CLI   SC2NDLEN,0                                                       
         BE    NEEDFLDS                                                         
         CLI   SC2NDLEN,5                                                       
         BH    INVLFLD                                                          
         CLI   SC2NDFLD,C'A'       IS THE FIRST CHAR ALPHABETIC?                
         BL    INVLFLD                                                          
         CLI   SC2NDFLD,C'Z'       ....                                         
         BH    INVLFLD                                                          
*                                                                               
         BAS   RE,CGRPCHK          LET'S CHECK THE CLIENT GROUP                 
         BM    NOCGDEF             NO SUCH CLIENT GROUP DEFINITION              
         BP    DEFWRONG            CG DON'T AGREE WITH DEFINITIONS              
         OI    MISCFLG3,MF3CGRP    TURN ON THE CLIENT GROUP FLAG                
         B     VKCLTX                                                           
*                                                                               
VKCLT60  OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE(S)           
         BNZ   *+10                                                             
         MVC   TIOBCURI,SC1STNUM   TO PLACE CURSOR AT THE RIGHT PLACE           
         MVC   FAKEFLDH,CMVCLTKH                                                
         MVI   FAKEFLDH,88         HEADER + MAXDATA                             
         MVI   FAKEFLDH+5,3                                                     
         XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLD(3),SC1STFLD                                              
         ST    R2,SAVEDR2          SAVE OFF R2 TO USE FOR FAKEFLDH              
         LA    R2,FAKEFLDH                                                      
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         L     R2,SAVEDR2          RESTORE R2                                   
         MVC   CLTCODE,SC1STFLD    MOVE THE CLIENT EBCDIC CODE IN               
         LA    R6,SCBLKLQ(R6)      BUMP SCANBLOCK                               
         LA    R3,CLTLNQ(R3)       BUMP CLTTAB                                  
         B     VKCLT50                                                          
*                                                                               
VKCLTX   DS    0H                                                               
         MVI   CLTCODE,X'FF'       MARK END OF TABLE                            
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE(S)           
         BNZ   *+8                                                              
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R3                                                               
***************                                                                 
* VALIDATE THE PRODUCT(S) AND ESTIMATE                                          
***************                                                                 
VKPRD00  DS    0H                                                               
*                                                                               
         BAS   RE,CLRSCBLK                                                      
*                                                                               
         LA    R2,CMVPRDKH                                                      
         LA    R3,PRDTAB                                                        
         USING PRDTABD,R3                                                       
*                                                                               
         CLI   5(R2),0             ARE THERE ANY PRODUCTS AT ALL?               
         BE    NEEDFLDS             - NO, ERROR                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE(S)           
         BNZ   VKPRD30                                                          
         OI    TIOBINDS,TIOBSETC   SET CURSOR                                   
*                                                                               
         SR    R2,RA                                                            
         STH   R2,TIOBCURD         SAVE DISPLACEMENT OF FIELD FROM TWA          
         LA    R2,CMVPRDKH         PUT R2 BACK IN THE RIGHT PLACE               
*                                                                               
VKPRD30  CLC   =CL3'ALL',CMVPRDK   ALL PRODUCTS?                                
         BNE   VKPRD40              - NO, SKIP                                  
         CLI   5(R2),3             IS IT 3 CHARACTERS LONG?                     
         BNE   INVLFLD              - NOPED, MUST BE =C'ALL' ONLY               
         OI    MISCFLG3,MF3PRDAL    - YEA, TURN ON ALL PRD FLAG (X'10')         
         B     VKPRDX              DONE WITH PRD, LET'S CHECK ESTIMATE          
***                                                                             
         CLC   =C'SOON',CONWHEN    IS IT A SOON JOB?                            
         BNE   *+8                                                              
         MVI   REQSML,C'L'          - YUP, IT'S SOON, MAKE IT LONGJOB           
***                                                                             
*                                                                               
VKPRD40  TM    MISCFLG3,MF3CLTAL    ARE WE DOING CLIENT = ALL?                  
         BO    PRDALL               - YEAH, PRODUCT MUST BE ALL AS WELL         
*                                                                               
         GOTO1 SCANNER,DMCB,CMVPRDKH,(X'80',SCANBLK)                            
*                                                                               
         LA    R6,SCANBLK                                                       
         USING SCANBLKD,R6                                                      
         LR    R0,R3               SAVE OFF PRDTAB ADDRESS TO COMPARE           
VKPRD50  CLI   SC1STLEN,0          IS THERE ANY INPUT?                          
         BE    VKPRDX               - NO, END IT                                
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE              
         BNZ   *+10                                                             
         MVC   TIOBCURI,SC1STNUM   TO PLACE CURSOR AT THE RIGHT PLACE           
*                                                                               
         CLI   SC1STLEN,1          DOES PRODUCT HAVE MORE THAN 1 CHAR?          
         BNH   INVLFLD              - NOPE, INVALID                             
         CLI   SC1STLEN,3          IS IT MORE THAN 3 CHARS?                     
         BH    INVLFLD              - YEAH, NO SUCH PRODUCT                     
*                                                                               
***                                                                             
         MVC   PRDCODE,SC1STFLD    MOVE THE PRODUCT EBCDIC CODE IN              
         CR    R3,R0                                                            
         BNH   VKPRD60                                                          
         CLC   =C'SOON',CONWHEN    IS IT A SOON JOB?                            
         BNE   *+8                                                              
         MVI   REQSML,C'L'          - YUP, IT'S SOON, MAKE IT LONGJOB           
***                                                                             
VKPRD60  LA    R6,SCBLKLQ(R6)      BUMP THE SCANNER BLOCK                       
         LA    R3,L'PRDTAB(R3)     BUMP PRODUCT TABLE                           
         B     VKPRD50                                                          
*                                                                               
VKPRDX   DS    0H                                                               
         MVI   PRDCODE,X'FF'       MARK END OF TABLE                            
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R3                                                               
***************                                                                 
* VALIDATE ESTIMATE NUMBER                                                      
***************                                                                 
VKEST00  DS    0H                                                               
*                                                                               
         BAS   RE,CLRSCBLK                                                      
*                                                                               
         LA    R2,CMVESTKH                                                      
         LA    R3,ESTTAB                                                        
         USING ESTTABD,R3                                                       
*                                                                               
         CLI   5(R2),0             ARE THERE ANY ESTIMATES AT ALL?              
         BE    NEEDFLDS             - NO, ERROR                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE              
         BNZ   VKEST30                                                          
         OI    TIOBINDS,TIOBSETC   SET CURSOR                                   
*                                                                               
         SR    R2,RA                                                            
         STH   R2,TIOBCURD         SAVE DISPLACEMENT OF FIELD FROM TWA          
         LA    R2,CMVESTKH                                                      
*                                                                               
VKEST30  CLC   =CL3'ALL',CMVESTK   ALL ESTIMATES?                               
         BNE   VKEST40              - NO, SKIP                                  
         CLI   5(R2),3             IS IT =C'ALL' EXACTLY?                       
         BNE   INVLFLD              - NOPE, SEE YA LATER                        
         OI    MISCFLG3,MF3ESTAL    - YEA, TURN ON ALL EST FLAG (X'10')         
         MVI   ESTNUM,0            NEED TO MAKE SURE WE HAVE A 0                
***                                                                             
         CLC   =C'SOON',CONWHEN    IS IT A SOON JOB?                            
         BNE   *+8                                                              
         MVI   REQSML,C'L'          - YUP, IT'S SOON, MAKE IT LONGJOB           
***                                                                             
         LA    R3,ESTLNQ(R3)                                                    
         B     VKESTX              DONE WITH EST, LET'S CHECK FILTERS           
*                                                                               
VKEST40  TM    MISCFLG3,MF3CLTAL   ARE WE DOING CLIENT = ALL?                   
         BO    ESTALL               - YUP, ESTIMATE MUST BE ALL AS WELL         
*                                                                               
         GOTO1 SCANNER,DMCB,CMVESTKH,(X'80',SCANBLK)                            
*                                                                               
         LA    R6,SCANBLK                                                       
         USING SCANBLKD,R6                                                      
         LR    R0,R3               SAVE OFF ESTTAB ADDRESS TO COMPARE           
VKEST50  CLI   SC1STLEN,0                                                       
         BE    VKESTX                                                           
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE              
         BNZ   *+10                                                             
         MVC   TIOBCURI,SC1STNUM   TO PLACE CURSOR AT THE RIGHT PLACE           
         TM    SC1STVAL,SCNUMQ     IS IT VALID NUMERIC??                        
         BNO   INVLFLD              - NOPE, INVALID                             
         MVC   ESTNUM,SC1STNUM+3   STORE THE ESTIMATE NUMBER IN BINARY          
*                                                                               
         CLI   SC1STLEN,3          IS IT MORE THAN 3 CHARS?                     
         BH    INVLFLD              - YEAH, NO SUCH ESTIMATE                    
*                                                                               
***                                                                             
         CR    R3,R0                                                            
         BNH   VKEST60                                                          
         CLC   =C'SOON',CONWHEN    IS IT A SOON JOB?                            
         BNE   *+8                                                              
         MVI   REQSML,C'L'          - YUP, IT'S SOON, MAKE IT LONGJOB           
***                                                                             
VKEST60  LA    R6,SCBLKLQ(R6)                                                   
         LA    R3,ESTLNQ(R3)                                                    
         B     VKEST50                                                          
*                                                                               
VKESTX   DS    0H                                                               
         MVI   ESTNUM,X'FF'        MARK END OF TABLE                            
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE, SKIP FOLLOWING LINE              
         BNZ   VKMKT00                                                          
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
***************                                                                 
* VALIDATE MARKET FILTER NUMBER                                                 
***************                                                                 
VKMKT00  DS    0H                                                               
*                                                                               
         LA    R2,CMVMKTKH                                                      
         CLI   5(R2),0             IS THERE ANY MARKET FILTER INPUT?            
         BE    VKMKTX               - NO INPUT, CONTINUE                        
         TM    4(R2),X'08'         IS THE MARKET VALID NUMERIC?                 
         BNO   INVLMKT              - NO, INVALID                               
*  WE NEED TO CONVERT CMVMKTK INTO 4 DIGIT MARKET                               
*  CLIENT MIGHT NOT HAVE TYPED IT IN 4 DIGITS IF MARKET IS 3 OR LESS            
         XR    RE,RE                                                            
         IC    RE,5(R2)            LET'S GET THE LENGTH                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  WORK(3),CMVMKTK(0)                                               
         UNPK  CMVMKTK(4),WORK(3)                                               
         OI    CMVMKTK+3,X'F0'     TURN ON FIRST NIBBLE (NOW EBCDIC)            
         MVI   5(R2),4             NEW LENGTH IS NOW 4!! (LEADING ZERO)         
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         OI    FILTFLG1,FF1MKT     WE'RE FILTERING FOR MARKET                   
*                                                                               
VKMKTX   DS    0H                                                               
***************                                                                 
* VALIDATE STATION FILTER                                                       
***************                                                                 
VKSTA00  DS    0H                                                               
*                                                                               
         LA    R2,CMVSTAKH                                                      
         CLI   5(R2),0             ANY STATION TO FILTER?                       
         BE    VKSTAX               - NO INPUT, CONTINUE                        
         MVC   WORK(4),=C'0000'                                                 
         MVC   WORK+10(4),CMVSTAK                                               
         CLI   CMVMEDK,C'T'        IS IT MEDIA T?                               
         BNE   *+10                                                             
         MVC   WORK+14(1),CMVMEDK                                               
         OC    WORK+10(5),SPACES                                                
         GOTO1 MSPACK,DMCB,WORK,WORK+10,WORK+15                                 
*                                                                               
         MVC   SVSTA,WORK+17                                                    
         OI    FILTFLG1,FF1STA     WE'RE FILTERING FOR MARKET                   
*                                                                               
VKSTAX   DS    0H                                                               
***************                                                                 
* VALIDATE DATE FILTER                                                          
***************                                                                 
VKDAT00  DS    0H                                                               
*                                                                               
         LA    R2,CMVDAT1H                                                      
         CLI   5(R2),0             IS THERE ANY "FROM" DATE INPUT?              
         BE    VKDAT20              - NO INPUT, CHECK 2ND DATE                  
         OI    FILTFLG1,FF1DATE1   "FROM" DATE EXISTS                           
*                                                                               
VKDAT20  LA    R2,CMVDAT2H                                                      
         CLI   5(R2),0             IS THERE ANY                                 
         BE    VKDATX                                                           
         OI    FILTFLG1,FF1DATE2   "TO" DATE EXISTS                             
*                                                                               
VKDATX   DS    0H                                                               
         TM    FILTFLG1,FF1DATE1+FF1DATE2                                       
         BZ    VKTRD00             NO DATE FILTERS, LET'S GET OUT               
         BO    VKDATXX                                                          
         TM    FILTFLG1,FF1DATE1                                                
         BO    *+8                 "TO" FILTER DATE IS MISSING                  
         LA    R2,CMVDAT1H         "FROM" FILTER DATE IS MISSING                
         B     DATRANGE                                                         
*                                                                               
VKDATXX  BRAS  RE,DSETUP                                                        
         BE    VKTRD00                                                          
         TM    FILTFLG1,FF1D1BAD                                                
         BNO   *+8                 "TO" FILTER DATE IS BAD                      
         LA    R2,CMVDAT1H         "FROM" FILTER DATE IS BAD                    
         B     INVLDATE                                                         
***************                                                                 
* VALIDATE DATE FILTER                                                          
***************                                                                 
VKTRD00  DS    0H                                                               
*                                                                               
         LA    R2,CMVTRDH                                                       
         CLI   5(R2),0             ASKING FOR TRADE BIT?                        
         BE    VKTRDX               - NO INPUT                                  
         CLI   CMVTRD,C'N'         IS IT A C'N'?                                
         BE    VKTRD20              - YUP!  TURN ON CASH FLAG!                  
         CLI   CMVTRD,C'Y'         IS IT A C'Y'?                                
         BNE   INVLFLD              - NOPE, ERROR, INVALID FIELD                
*                                                                               
VKTRD10  OI    FILTFLG2,FF2TRD     THEY BE ASKIN' FOR TRADE BIT YO              
         B     VKTRDX                                                           
*                                                                               
VKTRD20  OI    FILTFLG2,FF2NOTRD                                                
*                                                                               
VKTRDX   DS    0H                                                               
*                                                                               
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       DS    0H                                                               
         LA    R1,HDSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
*                                                                               
         TM    MISCFLG3,MF3CGRP    ARE WE DOING CLIENT GROUP?                   
         BNO   PR04                 - NOPE, CONTINUE AS USUAL                   
*                                                                               
*****  THE FOLLOWING BUNCH OF CODE IS FOR CLIENT GROUP ONLY!!                   
         LA    R4,KEY                                                           
         USING GRPRECD,R4                                                       
         MVI   GRPKTYP,GRPKTYPQ    TYPE X'0D'                                   
         MVI   GRPKSTYP,GRPKCTYQ   SUBTYPE X'04' CLIENT GROUP                   
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,CGRPID                                                    
         MVC   GRPKCODE,CGRPPCOD                                                
         MVC   CGRPSKEY,KEY        SAVE THIS, THIS IS INITIAL CG KEY            
*                                                                               
         GOTO1 HIGH                                                             
         B     PRCG10                                                           
*                                                                               
PRCG05   GOTO1 SEQ                                                              
PRCG10   CLC   CGRPSKEY(GRPKCODE-GRPKEY),KEY   COMPARE UP TO GROUP CODE         
         BNE   PRX                                                              
*                                                                               
**** THE FOLLOWING IS TAKEN FROM SPSFM26 CLIENT GROUP MAINT/LIST                
         ICM   R1,B'1100',GRPKCODE   WE NEED THE GROUP CODE IN...               
*                                  ...CHARACTER FORM FOR COMPARING              
         SRL   R1,12               DD DD ?? ??  =>  00 0D DD D?                 
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'        00 0D DD DS                                  
         UNPK  CODECHAR(5),FULL+1(3)            =>  Z0 ZD ZD ZD ZD              
         MVC   CODECHAR(1),GRPKID                                               
****                                                                            
         ICM   R1,B'1100',CGRPPCOD   WE NEED THE GROUP CODE IN...               
*                                  ...CHARACTER FORM FOR COMPARING              
         SRL   R1,12               DD DD ?? ??  =>  00 0D DD D?                 
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'        00 0D DD DS                                  
         UNPK  CGRPCCOD(5),FULL+1(3)            =>  Z0 ZD ZD ZD ZD              
         MVC   CGRPCCOD(1),CGRPID                                               
****                                                                            
         ZIC   RE,CGRPCOMP         WE NEED TO COMPARE GROUP CODE NOW            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CODECHAR(0),CGRPCCOD   COMPARE CHARACTER CODE                    
         BNE   PRX                                                              
         MVC   CGRPKEY,KEY         SAVE OFF KEY FOR LATER TO GET BACK..         
*                                  ..INTO SEQUENCE                              
         MVC   AIO,AIO3            SAVE THE CGROUP RECORD IN AIO3               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R3,CLTTAB                                                        
         USING CLTTABD,R3                                                       
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,GRPVALCQ                                                  
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PRCG20   BRAS  RE,NEXTEL                                                        
         BNE   PRCGX               NO MORE X'30' ELEMENTS                       
         USING GRPVALD,R6                                                       
         MVC   CLTCODE,GRPVALUE    SAVE OFF THE CLIENT CODE                     
         LA    R3,CLTLNQ(R3)       BUMP CLTTAB                                  
         B     PRCG20              LET'S LOOK FOR MORE X'30' ELEMENTS           
*                                                                               
PRCGX    MVI   0(R3),X'FF'                                                      
         DROP  R3                                                               
*****  ABOVE IS CODE FOR CLIENT GROUP STUFF                                     
*                                                                               
PR04     LA    R2,CLTTAB           CLIENT TABLE!!                               
         USING CLTTABD,R2                                                       
         LA    R3,PRDTAB           PRODUCT TABLE!!                              
         USING PRDTABD,R3                                                       
         LA    R6,ESTTAB           ESTIMATE TABLE!!                             
         USING ESTTABD,R6                                                       
*                                                                               
PR05     TM    MISCFLG3,MF3CLTAL   WAS CLIENT = ALL?                            
         BNO   PR06                 - NOPE, CHECK FOR END OF CLTTAB             
*                                                                               
         OC    SAVEKEY2,SAVEKEY2   IS THERE ANYTHING HERE?                      
         BZ    PRCLT10              - NOPE, FIRST TIME THROUGH                  
         MVC   KEY,SAVEKEY2        RESTORE PASSIVE CLIENT ORDER KEY             
*                                  ...TO CONTINUE WITH SEQUENTIAL               
         GOTO1 HIGH                                                             
         CLC   KEY(13),SAVEKEY2                                                 
         BE    *+6                                                              
         DC    H'0'                SHOULD BE EQUAL                              
*                                                                               
         B     PRCLT20                                                          
*                                                                               
PRCLT10  XC    KEY,KEY             WE'RE DOING ALL CLIENTS                      
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DCKTYPE,DCKTYPQ     X'0D' TYPE                                   
         MVI   DCKSUBTY,DCKSTYPQ   X'B5' SUBTYPE                                
         MVC   DCKAGMD,BAGYMD                                                   
*        MVC   DCKCLT,BCLTCODE                                                  
*        MVC   DCKPRD,THISBPRD                                                  
*        MVC   DCKEST,ESTNUM                                                    
*                                                                               
         GOTO1 HIGH                                                             
         B     PRCLT30                                                          
*                                                                               
PRCLT20  GOTO1 SEQ                                                              
*                                                                               
PRCLT30  CLC   KEY(3),KEYSAVE      SAME 0D/B5/AM?                               
         BNE   PRX                  - NOPE, WE DONE                             
         OC    SAVEKEY2,SAVEKEY2   FIRST TIME THROUGH?                          
         BZ    PRCLT40              - YUP                                       
         CLC   KEY(5),KEYSAVE      SAME 0D/B5/AM/BCLT?                          
         BE    PRCLT20              - YEAH, KEEP DOING SEQ                      
*                                                                               
PRCLT40  MVC   SAVEKEY2,KEY        SAVE OFF THE PASSIVE CLIENT ORDER            
         XC    KEY,KEY                                                          
         USING CLTHDRD,KEY                                                      
         LA    R4,KEY              JUST IN CASE                                 
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,SAVEKEY2+3                                               
*                                                                               
         GOTO1 HIGH                                                             
         BE    *+6                 CLIENT RECORD BETTER BE THERE                
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3            PUT THE CLIENT RECORD IN AIO3                
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R4,AIO3                                                          
         USING CLTHDRD,R4                                                       
*                                                                               
         ST    R2,SAVEDR2          SAVE OFF R2 AND R3                           
         ST    R3,SAVEDR3                                                       
*                                                                               
         LA    R2,CLIST            SAVE OFF THE PRODUCT CODE LIST               
         LA    R3,880                                                           
         LA    RE,SVCLIST                                                       
         LA    RF,880                                                           
         MVCL  RE,R2                                                            
*                                                                               
         L     R2,SAVEDR2          RESTORE R2 AND R3                            
         L     R3,SAVEDR3                                                       
*                                                                               
         MVC   BCLTCODE,CKEYCLT                                                 
         MVC   BCLT,CKEYCLT                                                     
*                                                                               
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),CLTCODE                            
*                                                                               
         B     PR06A                                                            
*                                                                               
PR06     CLI   CLTCODE,X'FF'       CHECK END OF CLIENT TABLE                    
         BE    PRCLTX               - YUP, WE REACHED THE END                   
*                                                                               
         MVC   FAKEFLDH,CMVCLTKH                                                
         MVI   FAKEFLDH,88         HEADER + MAXDATA                             
         MVI   FAKEFLDH+5,3                                                     
         XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLD(3),CLTCODE                                               
         ST    R2,SAVEDR2          SAVE OFF R2 TO USE FOR FAKEFLDH              
         LA    R2,FAKEFLDH                                                      
*                                                                               
         GOTO1 VALICLT             GOING THROUGH VALICLT FOR SAVE CLIST         
*                                                                               
         L     R2,SAVEDR2          RESTORE R2                                   
         MVC   BCLTCODE,BCLT       SAVE THE 2 BYTE BINARY CLIENT                
PR06A    MVC   THISCLT,CLTCODE                                                  
         MVC   THISBCLT,BCLTCODE   SAVE OFF THE BINARY CLIENT                   
         TM    MISCFLG3,MF3PRDAL   ARE WE DOING ALL PRODUCTS?                   
         BO    *+8                  - YUP, SKIP PSETUP                          
***************                                                                 
* LET'S SETUP THE PRODUCT TABLE FOR THIS CLIENT                                 
***************                                                                 
*                                                                               
         BAS   RE,PSETUP           ROUTINE TO SET UP PRODUCT TABLE              
*                                                                               
         MVC   THISBPRD,BPRDCODE   SAVE OFF THE BINARY PRODUCT                  
***************                                                                 
* LET'S DO THE MOVE  --  USE PASSIVE BY CLIENT                                  
***************                                                                 
** NOTE: PRNXTEST, PRNXTPRD, PRNXTCLT CHECKS FOR X'FF'                          
**       MIGHT NOT BE NECESSARY HERE       MHC  05/22/03                        
PR07     TM    MISCFLG3,MF3PRDAL   ALL PRODUCTS?                                
         BO    PR09                 - YEAH, SKIP THE NEXT CHECKS                
*                                                                               
         CLI   PRDCODE,X'FF'       END OF PRODUCT TABLE?                        
         BE    PRNXTCLT             - YUP, NEXT CLIENT                          
*                                                                               
         CLI   BPRDCODE,0          DOES THIS CLIENT HAVE THIS PRODUCT?          
         BE    PRNXTPRD             - NOPE, NEXT PRODUCT ON LIST PLZ!           
*                                                                               
PR09     TM    MISCFLG3,MF3ESTAL   ALL ESTIMATES?                               
         BO    PR10                 - YEAH, SKIP THE NEXT CHECK                 
*                                                                               
         CLI   ESTNUM,X'FF'        END OF ESTIMATE TABLE?                       
         BE    PRNXTPRD                                                         
*                                                                               
         USING DOKEY,R4                                                         
PR10     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   DCKTYPE,DCKTYPQ     X'0D' TYPE                                   
         MVI   DCKSUBTY,DCKSTYPQ   X'B5' SUBTYPE                                
         MVC   DCKAGMD,BAGYMD                                                   
         MVC   DCKCLT,BCLTCODE                                                  
         MVC   DCKPRD,THISBPRD                                                  
         MVC   DCKEST,ESTNUM                                                    
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
PR13     LA    R4,KEY              MAKE SURE R4 POINTING AT PASSIVE KEY         
         NI    MISCFLG2,X'FF'-MF2SENT-MF2FIRST   RESET SENT+FIRST FLAGS         
         NI    MISCFLG2,X'FF'-MF2DLFND   RESET DELIVERY FOUND FLAG              
         NI    MISCFLG2,X'FF'-MF2FAXDL   RESET FAX DELIVERED FLAG               
         NI    FILTFLG1,X'FF'-FFMKTBAD   RESET ORDER NOT IN MARKET FLAG         
*                                                                               
***********************************************************************         
*  RULES:      A/M  CLT  PRD  EST                                     *         
*  OK           T   ALL  ALL  ALL                                     *         
*  OK           T   XXX  ALL  ALL                                     *         
*  OK           T   XXX  XXX  ALL                                     *         
*  OK           T   XXX  ALL  XXX                                     *         
*  OK           T   XXX  XXX  XXX                                     *         
*  THE OTHER POSSIBILITIES AREN'T OK!!!!                              *         
***********************************************************************         
         CLC   KEY(DCKCLT-DOKEY),KEYSAVE   SAME A/M?                            
         BE    PR15                 - YES, CONTINUE                             
         TM    MISCFLG3,MF3CLTAL   ALL CLIENTS?                                 
         BO    PRCLTX               - YUP, GET OUTTA HERE                       
         B     PRNXTEST                                                         
*                                                                               
PR15     CLC   KEY(DCKPRD-DOKEY),KEYSAVE   SAME CLT?                            
         BE    PR16                 - YUP, CONTINUE                             
         TM    MISCFLG3,MF3PRDAL+MF3ESTAL   ALL PRODUCTS AND ESTIMATES?         
         BO    PRNXTCLT             - YEAH, WE NEED A NEW CLIENT                
         TM    MISCFLG3,MF3ESTAL   ALL ESTIMATES ONLY?                          
         BO    PRNXTPRD             - YES, NEED NEW PRODUCT                     
         B     PRNXTEST             - NOPE, WE NEED A NEW ESTIMATE              
*                                                                               
PR16     CLC   KEY(DCKEST-DOKEY),KEYSAVE   SAME CLT/PRD?                        
         BE    PR17                 - YUP, CONTINUE                             
         TM    MISCFLG3,MF3ESTAL   ALL ESTIMATES?                               
         BO    PRNXTPRD             - YES, WE NEED NEW PRODUCT                  
         B     PRNXTEST                                                         
*                                                                               
PR17     CLC   KEY(DCKSTA-DOKEY),KEYSAVE   SAME CLT/PRD/EST?                    
         BE    PR20                 - YUP, CONTINUE                             
         TM    MISCFLG3,MF3ESTAL   ALL ESTIMATE?                                
         BO    PR20                 - YUP, NO PROBLEMS                          
         B     PRNXTEST                                                         
*                                                                               
PR20     TM    FILTFLG1,FF1STA     DO WE HAVE A STATION FILTER?                 
         BNO   PR21                 - NOPE, CONTINUE                            
         CLC   KEY+7(L'SVSTA),SVSTA   STATION WE WANT?                          
         BNE   PR32                 - NOPE, NEXT!                               
*                                                                               
PR21     TM    FILTFLG2,FF2TRD                                                  
         BNO   PR21A                                                            
         TM    DCKFLAG,DCKFTRDE    IS IT A TRADE ORDER?                         
         BNO   PR32                 - NOPE, NEXT!                               
PR21A    TM    FILTFLG2,FF2NOTRD                                                
         BNO   PR21B                                                            
         TM    DCKFLAG,DCKFTRDE    IS IT A CASH ORDER?                          
         BO    PR32                 - NOPE, NEXT!                               
*                                                                               
PR21B    MVC   SAVEKEY(13),KEY                                                  
         GOTO1 GETREC                                                           
*                                                                               
         LA    R4,KEY              MAKE SURE R4 POINTING AT PASSIVE KEY         
         MVC   BCLT,DCKCLT         SAVE THE BCLIENT                             
         MVC   BPRD,DCKPRD         SAVE THE BPRODUCT AND BESTIMATE...           
         MVC   THISEST,DCKEST      ...FOR CURRENT PASS                          
         MVC   THISPRD,PRDCODE     PRDCODE IS BLANK IF PRD = ALL                
*                                                                               
         TM    MISCFLG3,MF3PRDAL   ALL PRODUCTS?                                
         BNO   PR23                 - NO, NO NEED FOR CONVERSION                
*                                                                               
         GOTO1 SHOWQPRD,DMCB,THISPRD   CONVERT BINARY PRODUCT TO EBCDIC         
*                                                                               
PR23     L     R4,AIO                                                           
         MVC   BINORDER,DOKORDER   SAVE THE ORDER NUMBER                        
*                                                                               
         BAS   RE,DISPLREC                                                      
         BE    PR23A                                                            
         TM    FILTFLG1,FFMKTBAD   IS ORDER IN FILTER MARKET?                   
         BNO   PR23A                - YES, ITS FINE, CONTINUE                   
         MVC   KEY(13),SAVEKEY     DIDN'T PASS MARKET FILTER TEST               
         B     PR30                WE NEED THE NEXT ORDER RECORD                
*                                                                               
PR23A    CLC   THISEST,PREVEST     IS IT THE SAME ESTIMATE AS BEFORE?           
         BNE   *+14                 - NO, GO GET THE DATES                      
         MVC   PESTDAT1(19),PREVDATE+7   SAME ESTIMATE, USE PREVDATE            
         B     PR25                                                             
*                                                                               
         BAS   RE,GETEST                                                        
         BE    PR24                ESTIMATE DATE WITHIN FILTER, GOOD            
         TM    MISCFLG3,MF3ESTAL   ALL ESTIMATES?                               
         BNO   PRNXTEST             - NO, NEED NEW ESTIMATE ON TABLE            
         MVC   KEY(13),SAVEKEY                                                  
         B     PR30                ESTIMATE DATES NOT WITHIN FILTER             
*                                                                               
PR24     MVC   PREVEST,THISEST     SAVE OFF THE ESTIMATE NUMBER                 
*                                                                               
PR25     XC    KEY,KEY             LOOK FOR MAKEGOODS                           
         LA    R4,KEY                                                           
         USING MOKEY,R4                                                         
         MVI   MOKTYPE,MOKTYPQ                                                  
         MVI   MOKSUBTY,MOKSTYPQ                                                
         MVC   MOKAGMD,BAGYMD                                                   
         MVC   MOKORDER,BINORDER                                                
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(MOKMGCD-MOKEY),KEYSAVE   MAKEGOOD FOR THIS ORDER?            
         BNE   PR27                                                             
*        MVC   PMGOOD,=C'**MKGD**'                                              
         CLC   PSTATUS,=CL6'CNFRMD'   IS THE STATUS 'CONFIRMED'?                
         BNE   *+10                                                             
         MVC   PSTATUS,=CL6'OFFERS'    - YUP, IT'S AN OFFER                     
         DROP  R4                                                               
*                                                                               
PR27     MVC   KEY(13),SAVEKEY                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
PR30     OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                RESTORE SEQUENCE(EVEN IF DELETED)            
PR32     GOTO1 SEQ                                                              
         TM    KEY+13,X'80'        DELETED                                      
         BO    PR32                GO UNTIL NOT DELETED                         
         NI    DMINBTS,X'FF'-X'08' TURN OFF READ FOR DELETES                    
         B     PR13                                                             
*****                                                                           
*****                                                                           
PRNXTEST TM    MISCFLG3,MF3ESTAL   ALL ESTIMATES?                               
         BO    PR09                                                             
         LA    R6,ESTLNQ(R6)       BUMP ESTTAB                                  
         CLI   ESTNUM,X'FF'                                                     
         BE    PRNXTPRD                                                         
         B     PR09                                                             
*****                                                                           
*****                                                                           
PRNXTPRD TM    MISCFLG3,MF3PRDAL   ALL PRODUCTS?                                
         BO    PRNXTP10             - YUP, DON'T BUMP TABLE                     
*                                                                               
         LA    R3,PRDLNQ(R3)       BUMP PRDTAB                                  
         MVC   THISBPRD,BPRDCODE                                                
         B     PRNXTP20                                                         
*                                                                               
PRNXTP10 XR    R0,R0               ...NO NEED TO BUMP IT                        
         IC    R0,THISBPRD         WE'RE GONNA AUGMENT THISBPRD...              
         AHI   R0,1                                                             
         STC   R0,THISBPRD         ...SINCE WE WANT ALL PRODUCTS                
*                                                                               
PRNXTP20 CLI   PRDCODE,X'FF'                                                    
         BNE   PRNXTP30                                                         
         CLI   ESTNUM,X'FF'        END OF ESTIMATE TABLE?                       
         BE    PRNXTP25             - YUP, NEXT CHECK...                        
         CLI   ESTNUM,X'00'        ESTNUM USES X'00' INSTEAD OF X'FF'           
         BNE   PRNXTEST            ...FOR ALL ESTIMATES                         
*                                                     CLT PRD CLT               
PRNXTP25 CLC   KEY+3(2),KEYSAVE+3   SAME CLT OR NO?  (XXX ALL XXX)              
         BNE   PRNXTCLT                                                         
*                                                                               
PRNXTP30 LA    R6,ESTTAB           NEED TO RESET ESTTAB FOR NEW PRODUCT         
         B     PR07                                                             
*****                                                                           
*****                                                                           
PRNXTCLT TM    MISCFLG3,MF3CLTAL   ALL CLIENTS?                                 
         BO    PRNXTC10                                                         
         LA    R2,CLTLNQ(R2)       BUMP CLTTAB                                  
         CLI   CLTCODE,X'FF'                                                    
         BE    PRCLTX                                                           
*                                                                               
PRNXTC10 LA    R6,ESTTAB           NEED TO RESET ESTTAB AND PRDTAB...           
         LA    R3,PRDTAB           ...FOR NEW CLIENT                            
         XC    PREVEST,PREVEST     CLEAR OUT PREVIOUS SAVED ESTIMATE            
         B     PR05                                                             
*                                                                               
PRCLTX   TM    MISCFLG3,MF3CGRP    ARE WE DOING CLIENT GROUPS                   
         BNO   PRX                  - NOPE WE'RE NOT, EXIT PLZ                  
         MVC   KEY,CGRPKEY         PUT CLIENT GROUP BACK INTO SEQUENCE          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    PREVEST,PREVEST     CLEAR OUT PREVIOUS SAVED ESTIMATE            
         LA    R4,KEY              R4 NEEDS TO BE POINTING TO KEY               
         B     PRCG05                                                           
*****                                                                           
*****                                                                           
*                                                                               
PRX      B     XIT                                                              
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CHECK IF THE CLIENT GROUP OPTION IS VALID                                     
***********************************************************************         
CGRPCHK  NTR1                                                                   
         LA    R6,SCANBLK                                                       
         USING SCANBLKD,R6                                                      
*                                                                               
         LA    R4,1                COUNTER FOR CGRPKEY COMPARE LENGTH           
**  WE START WITH 1 BECAUSE THE FIRST ONE IS A LETTER                           
         LA    RE,4                4 NUMBERS IN THE CODE TO CHECK               
         LA    R2,SC2NDFLD         D1234                                        
*                                                                               
CGVAL10  LA    R2,1(R2)             ****                                        
         CLI   0(R2),C' '                                                       
         BNE   CGVAL20                                                          
         TM    CGFLAG,CGSPACON     WAS 1ST SPACE/ASTERISK FOUND YET?            
         BO    CGVAL40                                                          
         OI    CGFLAG,CGSPACON     IT IS FOUND!                                 
         B     CGVAL40                                                          
*                                                                               
CGVAL20  CLI   0(R2),C'*'                                                       
         BNE   CGVAL30             NOT * OR SPACE, NEED FURTHER CHECK           
         TM    CGFLAG,CGSPACON     WAS 1ST SPACE/ASTERISK FOUND YET?            
         BO    CGRPERR1                                                         
         OI    CGFLAG,CGSPACON     WE GOT AN ASTERISK!                          
         MVI   0(R2),C' '          CHANGE THE * TO SPACE!!                      
         B     CGVAL40                                                          
*                                                                               
CGVAL30  CLI   0(R2),C'0'                                                       
         BL    CGRPERR1            NOT A NUMBER, NO GOOD                        
         CLI   0(R2),C'9'                                                       
         BH    CGRPERR1            NOT A NUMBER, NO GOOD                        
         LA    R4,1(R4)            IT'S A NUMBER, COUNT IT                      
*                                                                               
CGVAL40  BCT   RE,CGVAL10                                                       
*                                                                               
         STC   R4,CGRPCOMP         LET'S STORE THE LENGTH                       
*                                                                               
***  WE'RE GOING TO READ THE CLIENT GROUP DEFINITIONS RECORD                    
         XC    SAVEKEY2,SAVEKEY2                                                
         LA    R4,SAVEKEY2                                                      
         USING GRPRECD,R4                                                       
         MVI   GRPKTYP,GRPKTYPQ    TYPE '0D'                                    
         MVI   GRPKSTYP,GRPKCTYQ   SUBTYPE '04' CLIENT GROUP                    
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,SC2NDFLD     GROUP ID                                     
         MVC   KEY,SAVEKEY2                                                     
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CGRPERR1                                                         
*                                                                               
         MVC   AIO,AIO3            USE AIO3 TO STORE THE DEF RECORD             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            RESTORE AIO                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         MVI   ELCODE,GRPBRKCQ     X'10' BREAK DESCRIPTION ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                BREAK DESCRIPTION ELEMENT MUST EXIST         
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   BREAK1LN,GRPBK1LN   SAVE BREAK LENGTHS                           
         MVC   BREAK2LN,GRPBK2LN                                                
         DROP  R6                  BELOW ALL ARE VALID                          
*                                  D                                            
         ZIC   RE,BREAK1LN         D*                                           
         ZIC   RF,BREAK2LN         D1          (IF 1ST BREAK LENGTH 1)          
*                                  D1*                                          
         LA    R6,SCANBLK          D1234                                        
         USING SCANBLKD,R6                                                      
*                                                                               
         LA    R0,1                D                                            
         AR    R0,RE                                                            
         AR    R0,RF               R0 HOLDS THE TOTAL LENGTH OF CODE            
         CLM   R0,1,CGRPCOMP                                                    
         BL    CGRPERR2                                                         
*                                                                               
         LA    R2,SC2NDFLD         ---> D1234                                   
*                                       *                                       
CGBK1    LA    R2,1(R2)            ---> D1234                                   
         CLI   0(R2),C'0'                *                                      
         BL    CGBK1ER                                                          
         BCT   RE,CGBK1                                                         
         B     CGNEXT                                                           
*                                                                               
CGBK1ER  CLM   RE,1,BREAK1LN       ARE THEY EQUAL?                              
         BNE   CGRPERR2                                                         
         B     CGNOERR             FIRST SPOT IS SPACE, IS OK                   
*                                                                               
CGNEXT   LTR   RF,RF                                                            
         BZ    CGNOERR                                                          
*                                                                               
CGBK2    LA    R2,1(R2)                                                         
         CLI   0(R2),C'0'                                                       
         BL    CGBK2ER                                                          
         BCT   RF,CGBK2                                                         
         B     CGNOERR                                                          
*                                                                               
CGBK2ER  CLM   RF,1,BREAK2LN       ARE THEY EQUAL?                              
         BNE   CGRPERR2                                                         
         B     CGNOERR                                                          
*                                                                               
CGRPERR1 XR    RE,RE               1ST TYPE OF ERROR, MAKE RE NEGATIVE          
         SHI   RE,1                                                             
         B     CGX                                                              
*                                                                               
CGRPERR2 LA    RE,1                2ND TYPE OF ERROR, MAKE RE POSITIVE          
         B     CGX                                                              
*                                                                               
CGNOERR  XR    RE,RE               NO ERROR                                     
*                                                                               
***  WE ARE PACKING THE GROUP CODE                                              
*                                                                               
CGX      MVC   FULL,SC2NDFLD+1     D1234, SC2NDFLD+1 = 1                        
         OC    FULL,=C'0000'                                                    
         PACK  DUB,FULL                                                         
         L     R0,DUB+4                                                         
         SRA   R0,4                                                             
         STCM  R0,3,CGRPPCOD       SAVE OFF THE GROUP CODE                      
         MVC   CGRPID,SC2NDFLD     SAVE OFF THE GROUP ID AS WELL                
*                                                                               
         LTR   RE,RE               SET THE CONDITION CODE                       
         B     XIT                 WE'RE DONE                                   
*                                                                               
***********************************************************************         
* SHOW THE EBCDIC PRODUCT CODE                                                  
***********************************************************************         
SHOWQPRD NTR1                                                                   
         CLI   BPRD,X'FF'                                                       
         BNE   SHWQPRD0                                                         
         L     R2,DMCB                                                          
         MVC   0(3,R2),=C'***'                                                  
         B     SHWQPRDX                                                         
*                                                                               
SHWQPRD0 LA    RE,SVCLIST                                                       
SHWQPRD1 OC    0(4,RE),0(RE)                                                    
         BZ    SHWQPRDX                                                         
         CLC   BPRD,3(RE)                                                       
         BE    SHWQPRD2                                                         
         LA    RE,4(RE)                                                         
         B     SHWQPRD1                                                         
SHWQPRD2 L     R2,DMCB                                                          
         MVC   0(3,R2),0(RE)                                                    
SHWQPRDX XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CHECK MARKET FILTER                                                           
***********************************************************************         
MKTCHK   NTR1                                                                   
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
*                                                                               
         MVI   STAKTYPE,STAKTYPQ   C'S'                                         
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,PSTATION                                                
         CLI   QMED,C'T'           THE MEDIA IS T                               
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'T'     WE NEED A T IN THE STA CALL LETTERS          
         MVC   STAKAGY,TWAAGY                                                   
         MVC   STAKCLT,QCLT                                                     
         L     R4,AIO3             NEED R4 POINTING TO THE RECORD NOW           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO3                 
*                                                                               
         CLC   KEY(STAKFILL-STAKTYPE),0(R4)     SAME STA/AGY/CLT?               
         BE    MKTCHK10                                                         
         MVC   KEY+9,=C'000'       NO CLT SPECIFIC STATION, USE C'000'          
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO3                 
*                                                                               
         CLC   KEY(STAKCLT-STAKTYPE),0(R4)     SAME STA/AGY?                    
         BNE   MKTCHKNO                                                         
*                                                                               
MKTCHK10 CLC   CMVMKTK,SMKT        IS THE MARKET WHAT WE WANT?                  
         BNE   MKTCHKNO             - NO, SIR                                   
*                                                                               
MKTCHKYS SR    RC,RC                                                            
         B     MKTCHKX                                                          
*                                                                               
MKTCHKNO OI    FILTFLG1,FFMKTBAD   ORDER DOES NOT PASS MARKET FILTER            
         OI    FILTFLG1,FF1BDSTA   SO IT DOESN'T PASS LABEL  DISEL01B           
MKTCHKX  LTR   RC,RC                                                            
         B     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
* GET THE ESTIMATE DATES                                                        
***********************************************************************         
GETEST   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTRECD,R4                                                       
*                                                                               
         MVI   EKEYTYPE,EKEYTYPQ   X'00'                                        
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,THISPRD                                                  
         CLC   =C'***',EKEYPRD     IS IT 3 STARS?                               
         BNE   *+10                 - NAH                                       
         MVC   EKEYPRD,=C'POL'      - YES, REPLACE IT WITH POL                  
         MVC   EKEYEST,THISEST                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(EKCNTRL-EKEYTYPE),KEYSAVE   SAME CLT/PRD/EST?                
         BE    GETEST40                                                         
**       MVC   PREVDATE+7(19),=CL19'ESTIMATE NOT FOUND'   NOT FOUND             
**       MVC   PESTDAT1(19),PREVDATE+7                                          
****  IF THE ESTIMATE IS NOT FOUND, (CLOSED OUT) SKIP ORDER!!!                  
         B     GESTNO              LET'S GET OUT NOW                            
*                                                                               
GETEST40 MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R4,AIO3                                                          
         TM    FILTFLG1,FF1DATE1+FF1DATE2   IS THERE A DATE FILTER?             
         BNO   GETEST50             - NOPE, CONTINUE                            
*                                                                               
         CLC   EEND,COMPDAT1       IS END DATE >= FILTER PERIOD START?          
         BL    GESTNO               - NO, BAD, LET'S GET OUT                    
         CLC   COMPDAT2,ESTART     IS FILTER PER. END >= START DATE?            
         BL    GESTNO               - NO, BAD, LET'S GET OUTTA HERE             
*                                                                               
GETEST50 MVC   THISDATE(6),ESTART   MOVE IN THE STARTING ESTIMATE DATE          
         MVC   THISDATE+6(6),EEND   MOVE IN THE ENDING ESTIMATE DATE            
*                                                                               
         GOTO1 DATCON,DMCB,(X'10',THISDATE),(X'85',PREVDATE)                    
*                                                                               
         MVC   PESTDAT1(19),PREVDATE+7   DATCON ADDS PERIOD, DON'T NEED         
*                                                                               
GESTYES  CR    RE,RE                                                            
         B     *+6                                                              
GESTNO   CR    R1,RE                                                            
         B     XIT                                                              
***********************************************************************         
* DISPLAY THE DARE RECORD                                                       
***********************************************************************         
DISPLREC NTR1                                                                   
         MVC   P,SPACES            INITIALIZE THE PRINT LINE TO SPACES          
         XC    THISCRTR,THISCRTR   CLEAR OUT CREATOR ID                         
         XC    AGYIDNUM,AGYIDNUM   CLEAR OUT AGYIDNUM AND REPIDNUM              
         XC    REPIDNUM,REPIDNUM                                                
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
*                                                                               
DISPRC10 MVC   HALF,DOKORDDT       SHOW THE ORDER NUMBER                        
         XC    HALF,=X'FFFF'                                                    
         ZICM  R0,HALF,2                                                        
         CVD   R0,DUB                                                           
         UNPK  PORDER(4),DUB                                                    
         OI    PORDER+3,X'F0'                                                   
         MVC   HALF,DOKORDSQ                                                    
         XC    HALF,=X'FFFF'                                                    
         ZICM  R0,HALF,2                                                        
         CVD   R0,DUB                                                           
         UNPK  PORDER+4(4),DUB                                                  
         OI    PORDER+7,X'F0'                                                   
*                                                                               
DISPRC20 MVI   MISCFLG1,0                                                       
         LA    R6,DORFRST                                                       
DISPRC22 CLI   0(R6),0                                                          
         BE    LR98                                                             
         CLI   0(R6),DOIDELQ       X'01' PRIMARY ID ELEMENT                     
         BE    DISPEL01                                                         
*                                                                               
         CLI   0(R6),DOI2ELQ       X'02' SECONDARY ID ELEMENT                   
         BE    DISPEL02                                                         
         CLI   0(R6),DOSPELQ       X'03' SUPPLEMENTARY ID ELEMENT               
         BE    DISPEL03                                                         
         CLI   0(R6),DOSTELQ       X'12' NEW STATUS LAYOUT                      
         BE    DISPEL12                                                         
*                                                                               
DISPRC25 XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DISPRC22                                                         
********                                                                        
* PRIMARY ID ELEMENT                                                            
********                                                                        
         USING DOIDELD,R6                                                       
DISPEL01 DS    0H                                                               
*                                                                               
         MVI   PPRDDASH,C' '       SPACE OUT PPRDDASH AND PPRD2                 
         MVC   PPRD2,SPACES        WE DON'T ALWAYS HAVE A PIGGYBACK             
*                                                                               
         MVC   PCLIENT,THISCLT                                                  
         MVC   PBUYER,DOIDBYR                                                   
         MVC   BPRD,DOIDPRD                                                     
         GOTO1 SHOWQPRD,DMCB,PPRODUCT                                           
*                                                                               
         CLI   DOIDPRD2,0                                                       
         BE    DISEL01A                                                         
         MVI   PPRDDASH,C'-'                                                    
         MVC   BPRD,DOIDPRD2                                                    
         GOTO1 SHOWQPRD,DMCB,PPRD2                                              
*                                                                               
DISEL01A EDIT  (B1,DOIDEST),(3,PESTIMAT),FILL=0                                 
         CLI   DOIDFLTN,0          IS THERE A FLIGHT NUMBER?                    
         BE    DISEL01B             - NOPE, NOTHING TO PRINT                    
         EDIT  (B1,DOIDFLTN),(2,PFLIGHT),FILL=0                                 
*                                                                               
DISEL01B CLC   DOISTA,PREVSTA      IS IT THE SAME AS THE PREV. STATION?         
         BNE   DISEL01C             - NOPE, WE NEED THE NONSENSE                
         TM    FILTFLG1,FF1BDSTA   SAME BAD STATION THAT'S NOT IN MKT?          
         BZ    DISEL1B0            NO, WE'RE OKAY THEN                          
         OI    FILTFLG1,FFMKTBAD                                                
         CR    RC,RD               RETURN CODE SET TO NE                        
         B     DISEL1C0                                                         
*                                                                               
DISEL1B0 MVC   PSTATION,PREVSTAC   JUST MOVE IN THE LAST STATION                
*                                                                               
         MVC   PMKT,PREVMKT        ..MOVE IN THE LAST MARKET AS WELL            
         MVC   PMKTNAME,PREVMKTN   ..MARKET NAME TOO                            
         B     DISEL01G                                                         
*                                                                               
DISEL01C NI    FILTFLG1,X'FF'-FF1BDSTA                                          
         XC    WORK(5),WORK                                                     
         MVC   WORK+2(3),DOISTA                                                 
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,PSTATION   STATION CALL LETTERS          
         CLI   PSTATION+3,C'-'                                                  
         BNE   *+8                                                              
         MVI   PSTATION,C' '                                                    
         MVC   PREVSTA,DOISTA      SAVE OFF THE BINARY STATION                  
         MVC   PREVSTAC,PSTATION   SAVE OFF THE CHAR STATION                    
         TM    FILTFLG1,FF1MKT     ARE WE FILTERING BY MARKET?                  
         BNO   DISEL01D             - NO, SKIP THE MARKET FILTER                
*                                                                               
         BRAS  RE,MKTCHK           CHECK THE MARKET TO FILTER                   
         BE    DISEL01D                                                         
DISEL1C0 MVC   P,SPACES                                                         
         B     XIT                                                              
*                                                                               
DISEL01D XC    FAKEFLD,FAKEFLD                                                  
         XC    FAKEFLDH,FAKEFLDH                                                
*                                                                               
         LA    R2,FAKEFLDH         VALIDATE STATION TO GET MARKET NAME          
         MVI   5(R2),L'PSTATION    STORE INPUT LENGTH IN FIELD HEADER           
         OI    4(R2),X'04'         MAKE ALPHA                                   
         MVC   FAKEFLD(L'PSTATION),PSTATION                                     
*                                                                               
         LR    R0,R6               SAVE R6                                      
         MVC   QCLT,=C'000'                                                     
*                                                                               
         GOTO1 VALISTA             TO GET THE MARKET NUMBER AND NAME            
*                                                                               
         MVC   PMKT(L'QMKT),QMKT                                                
         MVC   PMKTNAME,MKTNM                                                   
         MVC   PREVMKT,PMKT        SAVE OFF THE MARKET                          
         MVC   PREVMKTN,PMKTNAME   SAVE OFF THE MARKET NAME                     
*                                                                               
         MVC   KEY,SAVEKEY         RESTORE THE SEQUENCE...                      
         GOTO1 HIGH                ...AND GET ORDER BACK IN AIO1                
         GOTO1 GETREC                                                           
         LR    R6,R0               RESTORE R6                                   
*                                                                               
DISEL01G OC    DOIDCON,DOIDCON     ANY REP CONTRACT NUMBER?                     
         BZ    DISPRC25                                                         
         MVC   PCONTNUM(8),DOIDCON                                              
         B     DISPRC25                                                         
*                                                                               
********                                                                        
* SECONDARY ID ELEMENT                                                          
********                                                                        
         USING DOI2ELD,R6                                                       
DISPEL02 DS    0H                                                               
         MVC   MISCFLG1,DOI2FLG1                                                
         B     DISPRC25                                                         
********                                                                        
* SUPPLEMENTARY ID ELEMENT                                                      
********                                                                        
         USING DOSPELD,R6                                                       
DISPEL03 DS    0H                                                               
         TM    DOSPFLG1,DOSPTRDE   IS IT A TRADE ORDER?                         
         BNO   LR50                                                             
         MVI   PTRDORD,C'T'                                                     
*        TM    FILTFLG2,FF1TRD     FILTERING BY TRADE BIT?                      
*        BNO   LR45                 - NOPE, CONTINUE                            
*        TM    DOSPFLG1,DOSPTRDE   IS IT A TRADE ORDER?                         
*        BO    LR50                 - YUP!!!                                    
*        B     LRGETOUT             - NOPE, NEXT RECORD PLZ                     
*                                                                               
*R45     TM    FILTFLG2,FF1NOTRD   FILTERING BY CASH ORDER?                     
*        BNO   LR50                 - NOPE CONTINUE                             
*        TM    DOSPFLG1,DOSPTRDE   IS IT A CASH ORDER?                          
*        BZ    LR50                 - YUP!!                                     
*                                                                               
*RGETOUT OI    FILTFLG1,FFMKTTRD    - NOT TRADE ORDER OR BAD MARKET             
*        CR    RC,RF               SET CONDITION TO NE                          
*        MVC   P,SPACES                                                         
*        B     XIT                                                              
*                                                                               
LR50     TM    DOSPFLG1,DOSPCFCM   CONFIRM WITH COMMENTS?                       
         BZ    *+8                                                              
         OI    MISCFLG1,MF1CNFCM                                                
*                                                                               
         CLI   DOSPREVN,0                 GOT A REVISION NUMBER?                
         BE    *+8                                                              
         OI    MISCFLG1,MF1REVOR          YES, REVISED ORDER                    
*                                                                               
         OC    DOSPCRTR,DOSPCRTR   DO WE A CREATOR ID NUM?                      
         BZ    DISPRC25                                                         
         MVC   THISCRTR,DOSPCRTR   SAVE OFF THE CREATOR ID NUM                  
         B     DISPRC25                                                         
********                                                                        
* TRANSMISSION ELEMENT                                                          
********                                                                        
         USING DOSTELD,R6                                                       
DISPEL12 DS    0H                                                               
         OI    MISCFLG1,MF1YES12   WE FOUND A X'12' ELEMENT                     
*                                                                               
LR60     DS    0H                                                               
         NI    MISCFLG2,X'FF'-MF2DLVR   RESET DELIVERY FLAG                     
LR60S01  CLI   DOSTSTAT,DSENT      SENT?                                        
         BNE   LR60S02                                                          
         MVC   THISSTAT(6),=CL6'*SENT'                                          
*                                                                               
         MVC   AGYIDNUM,DOSTIDNM   WE HAVE THE AGYIDNUM (SENT)                  
         OI    MISCFLG2,MF2SENT    IT'S SENT                                    
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'*VRSNT'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'*RVSNT'                                         
         B     LRCHKERR                                                         
*                                                                               
LR60S02  CLI   DOSTSTAT,DFXSENT    FAX SENT?                                    
         BE    LR60S02E                                                         
         CLI   DOSTSTAT,DFXRSNT    FAX RESENT?                                  
         BNE   LR60S03                                                          
*                                                                               
LR60S02E MVC   THISSTAT(6),=CL6'*FXSNT'                                         
         MVC   AGYIDNUM,DOSTIDNM   WE HAVE THE AGYIDNUM (SENT)                  
         OI    MISCFLG2,MF2SENT    IT'S SENT                                    
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'*VFSNT'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'*RFSNT'                                         
         B     LRCHKERR                                                         
*                                                                               
LR60S03  CLI   DOSTSTAT,DEMSENT    EMAIL SENT?                                  
         BNE   LR60D01                                                          
         MVC   THISSTAT(6),=CL6'*EMSNT'                                         
*                                                                               
         MVC   AGYIDNUM,DOSTIDNM   WE HAVE THE AGYIDNUM (SENT)                  
         OI    MISCFLG2,MF2SENT    IT'S SENT                                    
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'*VESNT'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'*RESNT'                                         
         B     LRCHKERR                                                         
*                                                                               
LR60D01  CLI   DOSTSTAT,DDLVRD     DELIVERED?                                   
         BNE   LR60D02                                                          
         OI    MISCFLG2,MF2DLVR    IT'S DELIVERED                               
         OI    MISCFLG2,MF2DLFND   DELIVERY IS FOUND                            
         MVC   THISSTAT(6),=CL6'SENT'                                           
*                                                                               
         MVC   REPIDNUM,DOSTIDNM   WE HAVE THE REPIDNUM (DELIVERED)             
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VARSNT'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'REVSNT'                                         
         B     LRCHKERR                                                         
*                                                                               
LR60D02  CLI   DOSTSTAT,DFXDLVD    FAX DELIVERED?                               
         BNE   LR60D03                                                          
         OI    MISCFLG2,MF2DLVR    IT'S DELIVERED                               
         OI    MISCFLG2,MF2DLFND   DELIVERY IS FOUND                            
         MVC   THISSTAT(6),=CL6'FXSENT'                                         
         OI    MISCFLG2,MF2FAXDL   IT'S BY FAX                                  
*                                                                               
         MVC   REPIDNUM,DOSTIDNM   WE HAVE THE REPIDNUM (DELIVERED)             
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VFXSNT'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'RFXSNT'                                         
         B     LRCHKERR                                                         
*                                                                               
LR60D03  CLI   DOSTSTAT,DEMDLVD    EMAIL DELIVERED?                             
         BNE   LR61                                                             
         OI    MISCFLG2,MF2DLVR    IT'S DELIVERED                               
         OI    MISCFLG2,MF2DLFND   DELIVERY IS FOUND                            
         MVC   THISSTAT(6),=CL6'EMSENT'                                         
*                                                                               
         MVC   REPIDNUM,DOSTIDNM   WE HAVE THE REPIDNUM (DELIVERED)             
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VEMSNT'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'REMSNT'                                         
         B     LRCHKERR                                                         
*                                                                               
LR61     CLI   DOSTSTAT,QRJCT      REJECTED?                                    
         BNE   LR62                                                             
         MVC   THISSTAT(6),=CL6'RJCTED'                                         
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VARREJ'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'REVREJ'                                         
         B     LRCHKERR                                                         
*                                                                               
LR62     CLI   DOSTSTAT,QAPP       APPROVED?                                    
         BNE   LR63                                                             
         MVC   THISSTAT(6),=CL6'OPENED'                                         
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VAROPN'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'REVOPN'                                         
         B     LRCHKERR                                                         
*                                                                               
LR63     CLI   DOSTSTAT,QCFMDPND   CONFIRM PENDING?                             
         BNE   LR64                                                             
         MVC   THISSTAT(6),=CL6'CFMPND'   WAITING FOR OTHER PRD ORDERS          
         B     LRCHKERR                  THAT WERE SPAWNED TO GET CFRMD         
*                                                                               
LR64     CLI   DOSTSTAT,QCFMD      CONFIRMED?                                   
         BNE   LR66                                                             
         MVC   THISSTAT(6),=CL6'CNFRMD'                                         
         TM    MISCFLG1,MF1CNFCM   CONFIRM WITH COMMENTS?                       
         BO    LR64A                - YUP                                       
*                                                                               
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VARCNF'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'REVCNF'                                         
         B     LRCHKERR                                                         
                                                                                
LR64A    MVC   THISSTAT(6),=CL6'**PCFM'                                         
         TM    MISCFLG1,MF1VAROR                                                
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'**VPCF'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'**RPCF'                                         
         B     LRCHKERR                                                         
*                                                                               
LR66     CLI   DOSTSTAT,QFAXDLVD   FAX DELIVERED?                               
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'DELFAX'                                         
         B     LRCHKERR                                                         
*                                                                               
         CLI   DOSTSTAT,QFAXCNCL   FAX CANCELLED?                               
         BNE   LR70                                                             
         MVC   THISSTAT(6),=CL6'CANFAX'                                         
         OI    MISCFLG1,MF1ERROR   WE HAVE 1 OF THE ERROR STATUSES              
         B     LRCHKERR                                                         
*                                                                               
LR70     CLI   DOSTSTAT,QERRORED   ORDER IN ERROR?                              
         BNE   LR72                                                             
         MVC   THISSTAT(6),=CL6'ERROR'                                          
         OI    MISCFLG1,MF1ERROR   WE HAVE 1 OF THE ERROR STATUSES              
         B     LRCHKERR                                                         
*                                                                               
LR72     CLI   DOSTSTAT,QBYRCNFM   BUYER CONFIRMED?                             
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'BYRCFM'                                         
         B     LRCHKERR                                                         
*                                                                               
         CLI   DOSTSTAT,QEMPTY     EMPTY STATUS - NO BUYS?                      
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'EMPTY'                                          
         B     LRCHKERR                                                         
*                                                                               
         CLI   DOSTSTAT,QUNDARE    UNDARED?                                     
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'UNDARD' UNDARED                                 
         B     LRCHKERR                                                         
*                                                                               
         CLI   DOSTSTAT,QNODARE    NOT DARED?                                   
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'NTDARE' NOT DARE                                
         B     LRCHKERR                                                         
*                                                                               
         CLI   DOSTSTAT,QRECALL    RECALLED?                                    
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RECALL' RECALLED                                
         B     LR80                                                             
*                                                                               
         CLI   DOSTSTAT,QRCLAPPR   RECALLED, REP STATUS APPROVED?               
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLAPP' YES                                     
         B     LR80                                                             
*                                                                               
         CLI   DOSTSTAT,QRCLCONF   NOT RECALLED, REP STATUS CONFIRMED?          
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLCFM' YES                                     
         B     LR80                                                             
*                                                                               
         CLI   DOSTSTAT,QRCLDELN   RECALLED, REP STATUS DELIVERED?              
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLDNT' YES                                     
         B     LR80                                                             
*                                                                               
         CLI   DOSTSTAT,QRCLREJD   NOT RECALLED, REP STATUS REJECTED?           
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLREJ' YES                                     
         B     LR80                                                             
*                                                                               
         CLI   DOSTSTAT,QRCLTRNS   NOT RECALLED, REP STATUS TRANSMITTED         
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLTRN' YES, REP TRANSMITTED TO STATION         
         B     LR80                                                             
*                                                                               
         CLI   DOSTSTAT,QRCLWIP    RECALLED, WORK IN PROGRESS                   
         BNE   LR81                                                             
         MVC   THISSTAT(6),=CL6'RCLWIP' YES                                     
*                                                                               
LR80     TM    MISCFLG1,MF1VAROR   THIS CHECK IS FOR ALL RECALLS                
         BZ    *+14                ...BESIDES RECALL UNKNOWN                    
         MVC   THISSTAT(6),=CL6'VARRCL'                                         
         B     LRCHKERR                                                         
         TM    MISCFLG1,MF1REVOR                                                
         BZ    LRCHKERR                                                         
         MVC   THISSTAT(6),=CL6'REVRCL'                                         
         B     LRCHKERR                                                         
*                                                                               
LR81     CLI   DOSTSTAT,QRCLUNKN   RECALLED, REP STATUS UNKNOWN                 
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLUNK' NOT RECALLED, REP STAT UNKNOWN          
         B     LRCHKERR                                                         
*                                                                               
         CLI   DOSTSTAT,QSNTPNDG   SENT PENDING AFTER A RECALL                  
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'*PNDNG'                                         
         B     LRCHKERR                                                         
*                                                                               
         CLI   DOSTSTAT,QSNTXCNF   SENT CANCELLED, RECALL CONFIRMED             
         BE    LR64A                - SAME TREATMENT AS CONFIRMED W/COM         
*                                                                               
         CLI   DOSTSTAT,QSNTXREJ   SENT CANCELLED, RECALL REJECTED              
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RJCTED'                                         
         B     LRCHKERR                                                         
*                                                                               
         CLI   DOSTSTAT,QTOBESNT   TO BE SENT VIA SCRIPT                        
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'*SENT'                                          
         B     LRCHKERR                                                         
*                                                                               
         MVC   THISSTAT(6),=CL6'??????'   DUNNO THIS STATUS                     
*                                                                               
LRCHKERR DS    0H                                                               
LR90     ST    R6,SAVEDR6          SAVE OFF R6, NEED FOR LATER                  
*                                                                               
         TM    MISCFLG1,MF1ERROR   DO WE HAVE AN ERROR STATUS?                  
         BNO   *+10                 - NO WE DON'T.. DON'T CLEAR REPID           
         XC    REPIDNUM,REPIDNUM                                                
*                                                                               
         OC    REPIDNUM,REPIDNUM                                                
         BZ    LR98                                                             
         CLC   =X'FFFF',REPIDNUM                                                
         BNE   LR91                                                             
         MVC   PREPID,=CL10'FAXED'                                              
         B     LR98                                                             
*                                                                               
LR91     LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   CTIKNUM,REPIDNUM                                                 
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),(0,=C'CTFILE'),KEY,AIO2              
         L     R6,AIO2                                                          
         USING CTIKEY,R6                                                        
         LA    R6,CTIDATA                                                       
LR93     CLI   0(R6),0                                                          
         BE    LR98                                                             
         CLI   0(R6),X'02'                                                      
         BE    LR94                                                             
         XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LR93                                                             
********                                                                        
* REP SIGN-ON ID                                                                
********                                                                        
LR94     MVC   PREPID(10),2(R6)                                                 
*                                                                               
LR98     DS    0H                                                               
         DROP  R6                                                               
         TM    MISCFLG1,MF1YES12   DID WE FIND ANY X'12' ELEMENTS?              
         BO    *+10                 - YUP, CONTINUE                             
         MVC   PSTATUS(6),=C'UNSENT'   - NOPE, MAKE IT 'UNSENT'                 
*                                                                               
         OC    THISCRTR,THISCRTR   IF THERE IS A CREATOR ID...                  
         BZ    LR100                                                            
         MVC   AGYIDNUM,THISCRTR   ...USE IT FOR AGENCY ID FIELD                
*                                                                               
LR100    OC    AGYIDNUM,AGYIDNUM                                                
         BZ    LR108                                                            
*                                                                               
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   CTIKNUM,AGYIDNUM                                                 
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),(0,=C'CTFILE'),KEY,AIO3              
         L     R6,AIO3                                                          
         USING CTIKEY,R6                                                        
         LA    R6,CTIDATA                                                       
LR103    CLI   0(R6),0                                                          
         BE    LR108                                                            
         CLI   0(R6),X'02'                                                      
         BE    LR104                                                            
         XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LR103                                                            
********                                                                        
* AGY SIGN-ON ID                                                                
********                                                                        
LR104    MVC   PAGYID(10),2(R6)                                                 
*                                                                               
         DROP  R6                                                               
LR108    DS    0H                                                               
         L     R6,SAVEDR6          RESTORE R6                                   
         USING DOSTELD,R6                                                       
*                                                                               
***********************************************************************         
*                            STATUS LOGIC                             *         
*  81,83,85 FIRST                        ====>  81,83,85              *         
*  82,84,86 FIRST, FOLLOWED BY 81,83,85  ====>  82,84,86              *         
*  82,84,86 FIRST, FOLLOWED BY OTHER     ====>  OTHER                 *         
*  OTHER FIRST                           ====>  OTHER                 *         
*                                                                     *         
***********************************************************************         
         TM    MISCFLG1,MF1YES12   ANY X'12' ELEMENTS?                          
         BNO   XIT                  - NOPE, GET OUTTA HERE                      
*                                                                               
         TM    MISCFLG1,MF1NOFST   IS THIS NOT THE FIRST PASS?                  
         BNO   LR110                - THIS ISN'T THE FIRST PASS                 
         TM    MISCFLG2,MF2SENT    IS IT *SENT YET?                             
         BO    LR115                                                            
         TM    MISCFLG2,MF2FIRST   FIRST NON DELIVERY STATUS FOUND?             
         BO    LR115                - YUP, DON'T OVERWRITE STATUS               
         TM    MISCFLG2,MF2DLVR    IS THE CURRENT STATUS DELIVERY?              
         BO    LR120                - YUP, NEXT STATUS PLZ                      
*                                                                               
LR110    OI    MISCFLG1,MF1NOFST   NOT THE FIRST PASS ANYMORE                   
         MVC   PSTATUS,THISSTAT    MOVE STATUS TO PRINTLINE                     
         TM    MISCFLG2,MF2DLVR                                                 
         BO    LR115                                                            
         OI    MISCFLG2,MF2FIRST   FIRST NON-DELIVERY STATUS OBTAINED           
         OI    MISCFLG2,MF2DLFND   TURN ON EVEN IF WE DIDN'T FIND IT            
*                                                                               
LR115    TM    MISCFLG2,MF2SENT    IS IT ALREADY SENT?                          
         BO    XIT                  - YES, GET OUTTA HERE                       
*                                                                               
LR120    MVI   ELCODE,DOSTELQ      X'12' NEW STATUS ELEMENT                     
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         B     LR60                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        HEADSPECS                                                              
***********************************************************************         
*                                                                               
HDSPECS  SSPEC H1,50,C'DARE CLIENT ORDER'                                       
         SSPEC H2,50,C'-----------------'                                       
         SSPEC H1,2,AGYNAME                                                     
         SSPEC H2,2,AGYADD                                                      
         SSPEC H1,113,PAGE                                                      
         SSPEC H2,113,REQUESTOR                                                 
         SSPEC H3,113,RUN                                                       
         SSPEC H4,1,C'   '                                                      
         SSPEC H4,2,C'MEDIA     :'                                              
*                                                                               
         SSPEC H8,2,C'AGENCY ID'                                                
         SSPEC H9,2,C'---------'                                                
         SSPEC H8,13,C'CLT'                                                     
         SSPEC H9,13,C'---'                                                     
         SSPEC H8,17,C'PRD'                                                     
         SSPEC H9,17,C'---'                                                     
         SSPEC H8,25,C'EST'                                                     
         SSPEC H9,25,C'---'                                                     
         SSPEC H8,29,C'FL'                                                      
         SSPEC H9,29,C'--'                                                      
         SSPEC H8,32,C'ESTIMATE DATE'                                           
         SSPEC H9,32,C'-------------'                                           
         SSPEC H8,52,C'BYR'                                                     
         SSPEC H9,52,C'---'                                                     
         SSPEC H8,56,C'STATION'                                                 
         SSPEC H9,56,C'-------'                                                 
         SSPEC H8,65,C'MKT'                                                     
         SSPEC H9,65,C'---'                                                     
         SSPEC H8,70,C'MARKET'                                                  
         SSPEC H9,70,C'------'                                                  
         SSPEC H8,95,C'ORDER'                                                   
         SSPEC H9,95,C'-----'                                                   
         SSPEC H8,105,C'STATUS'                                                 
         SSPEC H9,105,C'------'                                                 
         SSPEC H8,112,C'REP  ID'                                                
         SSPEC H9,112,C'-------'                                                
         SSPEC H8,123,C'CONTRACT'                                               
         SSPEC H9,123,C'--------'                                               
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        HEADHOOKS                                                              
***********************************************************************         
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H4+13(L'QMED),QMED                                               
         MVC   H4+20(L'MEDNM),MEDNM                                             
*                                                                               
HDHKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CLRSCBLK --- SUBROUTINE TO CLEAR THE SCANBLOCK                      *         
***********************************************************************         
CLRSCBLK NTR1                                                                   
         LA    RE,SCANBLK          CLEAR THE IO AREA                            
         LHI   RF,SCANBLKQ         (16 X SCBLKLQ) SCBLKLQ = *-SCANBLKD          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*   DSETUP --- DATE SETUP FOR COMPARISON WITH ESIMATE DATES           *         
***********************************************************************         
DSETUP   NTR1                                                                   
*&&DO                                                                           
         LA    R0,8                (ESTDATE=) = 8 BYTES                         
*                                                                               
         LA    R4,CMVOPTS                                                       
         AR    R4,R0               HAVE R5 POINT TO THE FIRST DATE              
         LA    R3,FILTDAT1                                                      
         XR    R1,R1               COUNTER                                      
*                                                                               
DSET10   CLI   0(R4),C'-'          DASH IS OK                                   
         BE    DSET20                                                           
         CLI   0(R4),C'/'          SLASHES ARE OK TOO                           
         BE    DSET15                                                           
         CLI   0(R4),C'A'                                                       
         BL    DSETNO              IT'S BAD IF IT'S LOWER THAN C'A'             
*                                                                               
DSET15   MVC   0(1,R3),0(R4)       MOVE SINGLE CHARACTER                        
         LA    R1,1(R1)            BUMP COUNTER                                 
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         B     DSET10                                                           
*                                                                               
DSET20   STC   R1,FLTDATLN                                                      
*&&                                                                             
         OI    FILTFLG1,FF1D1BAD   ASSUME "FROM" DATE IS IN ERROR               
         MVI   DUB2,0                                                           
         OI    DUB2,PVINSGLO       SINGLE DATE ONLY IS VALID                    
         OI    DUB2,PVINSGLS       SINGLE DATE RETURNED AS SINGLE               
*        OI    DUB2,X'01'          ENGLISH LANGUAGE                             
*                                                                               
         GOTO1 PERVAL,DMCB,(CMVDAT1H+5,CMVDAT1),(DUB2,PERVBLK)                  
*                                                                               
         TM    4(R1),PVRCINV1      DATE ONE INVALID?                            
         BO    DSETNO               - YEP, ERROR                                
*                                                                               
         LA    RE,PERVBLK          RE-->PERVAL OUTPUT BLOCK                     
         USING PERVALD,RE                                                       
         TM    PVALASSM,PVALASM+PVALASY   ASSUMED MONTH OR YEAR?                
         BNZ   DSETNO               - YEP, ERROR                                
*                                                                               
         MVC   COMPDAT1,PVALESTA                                                
         DROP  RE                                                               
*&&DO                                                                           
         XR    R1,R1                                                            
         IC    R1,FLTDATLN         RESTORE THE COUNTER                          
         LA    R1,1(R1)            FOR THE DASH                                 
         LA    R4,1(R4)            FOR THE DASH...AS WELL                       
         AR    R0,R1               WE'RE MOVING ON TO THE SECOND DATE           
         LA    R3,FILTDAT2         WE NEED THE END DATE NOW                     
         XR    R1,R1               RESET THE COUNTER                            
*                                                                               
DSET30   CLI   0(R4),C'/'          SLASHES ARE OK =)                            
         BE    DSET35                                                           
         CLI   0(R4),C'A'                                                       
         BL    DSET40              IF IT'S LOWER THAN C'A', CONTINUE            
*                                                                               
DSET35   MVC   0(1,R3),0(R4)       MOVE SINGLE CHARACTER                        
         LA    R1,1(R1)            BUMP COUNTER                                 
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         B     DSET30                                                           
*                                                                               
DSET40   STC   R1,FLTDATLN                                                      
*&&                                                                             
         NI    FILTFLG1,X'FF'-FF1D1BAD                                          
         MVI   DUB2,0                                                           
         OI    DUB2,PVINSGLO       SINGLE DATE ONLY IS VALID                    
         OI    DUB2,PVINSGLS       SINGLE DATE RETURNED AS SINGLE               
*        OI    DUB2,X'01'          ENGLISH LANGUAGE                             
*                                                                               
         GOTO1 PERVAL,DMCB,(CMVDAT2H+5,CMVDAT2),(DUB2,PERVBLK)                  
*                                                                               
         TM    4(R1),PVRCINV1      DATE TWO INVALID?                            
         BO    DSETNO               - YEP, ERROR                                
*                                                                               
         LA    RE,PERVBLK          RE-->PERVAL OUTPUT BLOCK                     
         USING PERVALD,RE                                                       
         TM    PVALASSM,PVALASM+PVALASY   ASSUMED MONTH OR YEAR?                
         BNZ   DSETNO               - YEP, ERROR                                
*                                                                               
         MVC   COMPDAT2,PVALESTA                                                
*                                                                               
         CLC   COMPDAT1,COMPDAT2                                                
         BNL   DSETNO              INITIAL DATE IS LATER, ERROR                 
*                                                                               
         GOTO1 PERVERT,DMCB,COMPDAT1,COMPDAT2,WORK                              
         CLI   WORK,90             LESS THAN 90 DAYS?                           
         BL    DSETYES                                                          
***                                                                             
         CLC   =C'SOON',CONWHEN    WE HAVE A SOON JOB?                          
         BNE   DSETYES              - NOPE, GOOD TO GO                          
         MVI   REQSML,C'L'          - YUP, MAKE IT A LONGJOB                    
***                                                                             
DSETYES  CR    RE,RE                                                            
         B     *+6                                                              
DSETNO   CR    R1,RE                                                            
         STC   R0,THISDIST         UPDATED DISTANCE IN FIELD                    
         B     XIT                                                              
         DROP  RE                                                               
*                                                                               
***********************************************************************         
*   PSETUP --- INSERT CORRESPONDING PRODUCT NUMBERS IN PRODUCT TABLE  *         
***********************************************************************         
PSETUP   NTR1                                                                   
         USING CLTHDRD,R4                                                       
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   CKEYTYPE,CKEYTYPQ                                                
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,THISBCLT    MOVE THE BINARY CLIENT                       
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     DID WE FIND THE RIGHT CLIENT RECORD?         
         BE    *+6                                                              
         DC    H'0'                DEATH CUZ IT SHOULD BE FOUND                 
*                                                                               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R4,AIO2                                                          
         LA    R2,CLIST                                                         
         LA    R3,PRDTAB                                                        
         USING PRDTABD,R3                                                       
*                                                                               
PSETUP50 CLI   PRDCODE,X'FF'       END OF TABLE?                                
         BE    PSETUPX              - YUP, WE'RE DONE                           
         CLC   PRDCODE,0(R2)                                                    
         BL    NOPRD               NO SUCH PRODUCT FOR THIS CLIENT              
         BH    NEXTPRD             TRY NEXT PRODUCT ON CLIST                    
         MVC   BPRDCODE,3(R2)      FOUND, SAVE THE PRODUCT NUMBER               
         LA    R3,PRDLNQ(R3)       BUMP THE PRDTAB                              
         LA    R2,CLIST                                                         
         B     PSETUP50                                                         
*                                                                               
NEXTPRD  LA    R2,4(R2)            BUMP CLIST                                   
         OC    0(4,R2),0(R2)       IS THE LIST OVER?                            
         BZ    NOPRD               YUP, NO PROD IS FOUND                        
         B     PSETUP50                                                         
*                                                                               
NOPRD    MVI   BPRDCODE,0          PUT A 0 TO SIGNIFY NO SUCH PRODUCT           
         LA    R3,PRDLNQ(R3)       BUMP THE PRDTAB                              
         LA    R2,CLIST            RESET THE CLIST                              
         B     PSETUP50                                                         
*                                                                               
PSETUPX  B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GENERAL ERROR/INFO MESSAGES AND STUFF                                         
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
INVLOPT  MVI   GERROR1,INVFILT                                                  
         B     ERREXIT                                                          
INVLDATE MVI   GERROR1,INVDATE                                                  
         B     ERREXIT                                                          
INVLMKT  MVI   GERROR1,INVMKT                                                   
         B     ERREXIT                                                          
DATRANGE MVC   GERROR,=AL2(RANGEDAT)                                            
         B     ERREXIT                                                          
PRDALL   MVC   GERROR,=AL2(ALLPRD)                                              
         B     ERREXIT                                                          
ESTALL   MVC   GERROR,=AL2(ALLEST)                                              
         B     ERREXIT                                                          
NOTOVREQ MVC   GERROR,=AL2(OVERNITE)                                            
         B     ERREXIT                                                          
NOCGDEF  MVC   GERROR,=AL2(NOGRPDEF)                                            
         B     ERREXIT                                                          
DEFWRONG MVC   GERROR,=AL2(WRONGDEF)                                            
         B     ERREXIT                                                          
*                                                                               
RANGEDAT EQU   266                                                              
ALLPRD   EQU   268                                                              
ALLEST   EQU   269                                                              
OVERNITE EQU   270                                                              
NOGRPDEF EQU   308                 NO CLIENT GROUP DEFINITIONS FOUND            
WRONGDEF EQU   309                 CG DOESN'T AGREE WITH DEFINITIONS            
*                                                                               
NEEDFLDS MVI   GERROR1,REQFIELD                                                 
*                                                                               
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* DDBIGBOX                                                                      
* FAGETTXTD                                                                     
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* DDCOMFACSD                                                                    
* DMPRTQL                                                                       
* FATIOB                                                                        
* FAFACTS                                                                       
* DDSCANBLKD                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* SPGENCLT                                                                      
* SPGENMKT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDPERVALD                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
*                                                                               
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSD1D          (OUR CLIENT ORDER SCREEN)                    
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE SPGENGRP          (CLIENT GROUP RECORDS)                       
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRORD        (RECORD DSECTS)                              
         EJECT                                                                  
       ++INCLUDE SPGENDRMKO                                                     
         EJECT                                                                  
ESTRECD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* SYSSPARE SAVED STORAGE AREA                                                   
***********************************************************************         
SAVED    DSECT                                                                  
MISCFLG1 DS    X                   MISCELLANEOUS FLAGS 1                        
MF1VAROR EQU   X'80'                - ORDER IS A VAR ORDER                      
MF1VARCF EQU   X'40'                - VAR ORDER NOW CONFIRMED                   
MF1REVOR EQU   X'20'                - ORDER IS A REVISED ORDER                  
MF1CNFCM EQU   X'10'                - CONFIRM WITH COMMENTS                     
MF1YES12 EQU   X'08'                - X'12' ELEMENT HAS BEEN FOUND              
MF1NOFST EQU   X'04'                - NOT THE FIRST PASS ANYMORE                
MF1ERROR EQU   X'02'                - ERROR STAT, NEED TO CLEAN REPID           
*                                                                               
MISCFLG2 DS    XL1                 MISCELLANEOUS FLAGS 2                        
MF2SHWES EQU   X'80'                - SHOWED ESTIMATE ALREADY                   
MF2SENT  EQU   X'40'                - GOT THE *SENT STATUS                      
MF2FAXDL EQU   X'20'                - IT'S BEEN FAX DELIVERED                   
MF2FIRST EQU   X'10'                - FIRST NON-DELIVERY STATUS FOUND           
MF2DLFND EQU   X'08'                - DELIVERY STATUS FOUND ALREADY             
MF2DLVR  EQU   X'04'                - WE HAVE DELIVERY STAT (THIS LOOP)         
*                                                                               
MISCFLG3 DS    XL1                 MISCELLANEOUS FLAGS 3                        
MF3CGRP  EQU   X'80'                - WE'RE DOING CLIENT GROUP                  
MF3CLTAL EQU   X'40'                - WE'RE DOING ALL CLIENTS                   
MF3PRDAL EQU   X'20'                - WE'RE DOING ALL PRODUCTS                  
MF3ESTAL EQU   X'10'                - WE'RE DOING ALL ESTIMATES                 
*                                                                               
CGFLAG   DS    XL1                 CLIENT GROUP FLAG                            
CGSPACON EQU   X'80'                - ALREADY HAVE 1 SPACE/ASTERISK             
*                                                                               
FILTFLG1 DS    XL1                 FILTER FLAG 1                                
FF1MKT   EQU   X'80'                - WE'RE FILTERING BY MARKET                 
FFMKTBAD EQU   X'40'                - ORDER NOT IN MARKET                       
FF1DATE1 EQU   X'20'                - THERE IS A "FROM" DATE                    
FF1DATE2 EQU   X'10'                - THERE IS A "TO" DATE                      
FF1D1BAD EQU   X'08'                - THERE "FROM" DATE IS BAD                  
FF1BDSTA EQU   X'01'                - PREV STA IS BAD (NOT IN MKT)              
FF1STA   EQU   X'02'                - WE'RE FILTERING BY STATION                
*                                                                               
FILTFLG2 DS    XL1                 FILTER FLAG 2                                
FF2TRD   EQU   X'80'                - WE'RE FILTERING BY TRADE ORDER            
FF2NOTRD EQU   X'40'                - CASH ORDERS ONLY, NO TRADE                
*                                                                               
STATFLAG DS    X                   STATUS FLAG                                  
STFVAROR EQU   X'80'               VAR ORDER                                    
STFCNFCM EQU   X'40'               CONFIRM WITH COMMENTS                        
STFREVOR EQU   X'20'               REVISED ORDER                                
STFNOSTD EQU   X'10'               NO STATUS DATE                               
STFNODLV EQU   X'08'               NO DELIVERY DATE                             
*                                                                               
TEMPFLAG DS    X                                                                
TEMPBPRD DS    X                                                                
*                                                                               
SAVEDR6  DS    A                   SAVE OFF ADDRESS FOR R6                      
SAVEDR3  DS    A                   SAVE OFF ADDRESS FOR R3                      
SAVEDR2  DS    A                   SAVE OFF ADDRESS FOR R2                      
SAVEDR1  DS    A                   SAVE OFF ADDRESS FOR R1                      
BINORDER DS    XL4                 SAVE ORDER NUMBER                            
AGYIDNUM DS    XL2                 AGY ID NUMBER                                
REPIDNUM DS    XL2                 REP ID NUMBER                                
THISCLT  DS    CL3                 THE CURRENT CLIENT                           
THISBCLT DS    XL2                 THE CURRENT BINARY CLIENT                    
THISPRD  DS    CL3                 THE CURRENT PRODUCT                          
THISBPRD DS    CL1                 THE CURRENT BINARY PRODUCT                   
THISEST  DS    XL1                 THE CURRENT ESTIMATE                         
THISCRTR DS    XL2                 CREATOR ID                                   
THISDATE DS    CL12                TEMP DATE INPUT FIELD FOR DATCON             
THISDIST DS    XL1                 DISTANCE FROM BEGINNING OF FIELD             
COMPDAT1 DS    CL6                 COMPARING START DATE                         
COMPDAT2 DS    CL6                 COMPARING END DATE                           
THISSTAT DS    CL6                 THE CURRENT STATUS                           
DUB2     DS    CL1                                                              
PREVSTA  DS    CL3                 THE LAST STATION (BINARY)                    
PREVSTAC DS    CL8                 THE LAST STATION (CHARACTER)                 
PREVMKT  DS    CL4                 THE LAST MARKET (CHARACTER)                  
PREVMKTN DS    CL24                THE LSAT MARKET NAME                         
PREVEST  DS    XL1                 THE LAST ESTIMATE (BINARY)                   
PREVDATE DS    CL26                SAVE THE PREVIOUS ESTIMATE DATE              
CODECHAR DS    CL5                 GROUP CODE CHARACTER                         
CGRPCOMP DS    XL1                 LENGTH TO COMPARE ON THE CGKEY CODE          
CGRPID   DS    CL1                 CLIENT GROUP ID                              
CGRPCCOD DS    CL5                 CHARACTER GROUP CODE                         
CGRPPCOD DS    XL2                 PACKED GROUP CODE                            
CGRPKEY  DS    XL(L'KEY)           SAVED CLIENT GROUP KEY                       
CGRPSKEY DS    XL(L'KEY)           INITIAL CLIENT GROUP KEY                     
SAVEKEY  DS    XL(L'KEY)           SAVED KEY                                    
SAVEKEY2 DS    XL(L'KEY)           SAVED KEY 2                                  
SVCLTKEY DS    XL(L'KEY)           SAVED CLIENT KEY                             
*                                                                               
CHECKER  DS    CL1                                                              
ORDERNUM DS    XL4                                                              
SVMKT1   DS    CL4                 MARKET FILTERS                               
SVMKT2   DS    CL4                                                              
SVMKT3   DS    CL4                                                              
SVSTA    DS    XL3                 STATION FILTER                               
JSTDATE  DS    XL3                 START DATE FILTER (JULIAN PWOS)              
JNDDATE  DS    XL3                 END   DATE FILTER (JULIAN PWOS)              
*                                                                               
LASTSTA  DS    XL3                                                              
BCHKCLT  DS    XL2                 BINARY CLIENT TO CHECK                       
CHKCLT   DS    CL3                 CLIENT TO CHECK                              
MKTMATCH DS    CL1                                                              
*                                                                               
MAXMKTS  EQU   3                                                                
MKTSTAT  DS    0XL5                                                             
MARKET   DS    XL2                                                              
STATION  DS    XL3                                                              
*                                                                               
QORDER   DS    CL8                 EBCDIC ORDER NO.                             
FAKEFLDH DS    XL8                 FAKE HEADER                                  
FAKEFLD  DS    XL80                FAKE FIELD                                   
PERVALST DS    XL56                BLOCK FOR PERVAL                             
SVSTAKEY DS    XL(L'STAKEY)                                                     
SVCOLCOL DS    CL1                                                              
SVCOLDAT DS    CL2                                                              
TDYLSS2W DS    XL2                 TODAY LESS 2 WEEKS                           
*                                                                               
BREAK1LN DS    XL1                 CLT GRP DEF FIRST BREAK LENGTH               
BREAK2LN DS    XL1                 CLT GRP DEF SECOND BREAK LENGTH              
*                                                                               
STATABLE DS    100CL(STABLNQ)                                                   
STATABND DS    X                                                                
*                                                                               
CLTTAB   DS    160CL5                                                           
         DC    X'FF'                                                            
*                                                                               
PRDTAB   DS    15CL4                                                            
         DC    X'FF'                                                            
*                                                                               
ESTTAB   DS    20CL1                                                            
         DC    X'FF'                                                            
*                                                                               
SCANBLK  DS    16CL(SCBLKLQ)                                                    
SCANBLKQ EQU   *-SCANBLK                                                        
*                                                                               
PERVBLK  DS    CL64                                                             
*                                                                               
***********************************************************************         
* CLIENT TABLE DSECT                                                            
***********************************************************************         
CLTTABD  DSECT                                                                  
CLTCODE  DS    CL3                                                              
BCLTCODE DS    XL2                                                              
CLTLNQ   EQU   *-CLTTABD                                                        
*                                                                               
***********************************************************************         
* PRODUCT TABLE DSECT                                                           
***********************************************************************         
PRDTABD  DSECT                                                                  
PRDCODE  DS    CL3                                                              
BPRDCODE DS    XL1                                                              
PRDLNQ   EQU   *-PRDTABD                                                        
*                                                                               
***********************************************************************         
* ESTIMATE TABLE DSECT                                                          
***********************************************************************         
ESTTABD  DSECT                                                                  
ESTNUM   DS    XL1                                                              
ESTLNQ   EQU   *-ESTTABD                                                        
*                                                                               
***********************************************************************         
* STATION TABLE DSECT                                                           
***********************************************************************         
STABLED  DSECT                                                                  
STSTATN  DS    XL3                 BINARY STATION                               
STCLT    DS    CL3                 CLIENT OVERRIDE                              
STSTATUS DS    XL1                 STATUS                                       
STSTNOT  EQU   X'80'               STATION NOT IN ANY REQUESTED MKTS            
STABLNQ  EQU   *-STABLED                                                        
*                                                                               
***********************************************************************         
* CLIENT GROUP DSECT                                                            
***********************************************************************         
*                                                                               
***********************************************************************         
* PRINT LINE DSECT                                                              
***********************************************************************         
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL1                                                              
PAGYID   DS    CL10                                                             
         DS    CL1                                                              
PCLIENT  DS    CL3                 CLIENT                                       
         DS    CL1                                                              
PPRODUCT DS    CL3                 PRODUCT                                      
PPRDDASH DS    CL1                                                              
PPRD2    DS    CL3                                                              
         DS    CL1                                                              
PESTIMAT DS    CL3                 ESTIMATE                                     
PFLTDASH DS    CL1                 DASH FOR FLIGHT                              
PFLIGHT  DS    CL2                 FLIGHT NUMBER                                
         DS    CL1                                                              
PESTDAT1 DS    CL8                 ESTIMATE DATE START                          
         DS    CL1                                                              
PESTDASH DS    CL1                                                              
         DS    CL1                                                              
PESTDAT2 DS    CL8                 ESTIMATE DATE END                            
         DS    CL1                                                              
PBUYER   DS    CL3                 BUYER                                        
         DS    CL1                                                              
PSTATION DS    CL8                 STATION                                      
         DS    CL1                                                              
PMKT     DS    CL4                 MARKET                                       
         DS    CL1                                                              
PMKTNAME DS    CL24                MARKET NAME                                  
         DS    CL1                                                              
PORDER   DS    CL8                 ORDER NUMBER                                 
PTRDORD  DS    CL1                 C'T' IF TRADE ORDER                          
         DS    CL1                                                              
PSTATUS  DS    CL6                 STATUS                                       
         DS    CL1                                                              
PREPID   DS    CL10                MAKEGOOD GROUP CODE                          
         DS    CL1                                                              
PCONTNUM DS    CL8                 CONTRACT NUMBER                              
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPOMS21   01/16/07'                                      
         END                                                                    
