*          DATA SET SPGOL01    AT LEVEL 083 AS OF 11/19/19                      
*PHASE T20201C                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE DPTRD                                                                  
         TITLE 'SPGOL01 - SPOTPAK GOALS HEADLINE EDITS'                         
***********************************************************************         
* DATE    LEV WHO  DESCRIPTION                                        *         
* ------- --- ---- -------------------------------------------------- *         
* 13SEP95 35  AROT CHILD SPOT CHANGES                                 *         
* 13SEP95 36  SPRI IDR OPTION                                         *         
* 16FEB00 53  SPRI SAVE EFLAG FOR BRAND ESTIMATE                      *         
* 05APR01     MHER  SUPPORT PURPOSE CODES                             *         
* 07DEC05 74  EJOR  REMOVE HARDCODE FOR AGY GB                        *         
* 08AUG11 75  MHER  SUPPORT FOR PG GOAL INPUT                                   
***********************************************************************         
T20201   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20201                                                         
*                                                                               
         L     RC,0(R1)                                                         
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T202FFD,RA                                                       
*                                                                               
         OC    SVAPROF,SVAPROF     TEST FIRST TIME                              
         BNZ   MD                                                               
* READ AGYHDR                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),AGYALPHA                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         USING AGYHDRD,R6                                                       
*                                                                               
         GOTO1 GETREC                                                           
         MVC   SVAPROF,AGYPROF                                                  
         DROP  R6                                                               
*                                  READ AGY LEVEL SD PROFILE                    
         MVI   SDAGY,C'N'          IS IT A SD AGENCY                            
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0SD'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         XC    WORK2(16),WORK2                                                  
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,WORK2,VDATAMGR                                    
         OC    WORK2(16),WORK2                                                  
         BZ    *+8                                                              
         MVI   SDAGY,C'Y'          IF PROFILE EXISTS THEN YES                   
         SPACE 1                                                                
*=====================================================*                         
* INITIALIZE SECRET                                                             
*=====================================================*                         
         SPACE 1                                                                
         OC    T202FFD+4(2),T202FFD+4  TEST NEW SECURITY                        
         BNZ   *+14                                                             
         OC    T202FFD+6(2),T202FFD+6  TEST ANY LIMIT ACCESS                    
         BZ    MD                                                               
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',SECBLK),0                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MD       LA    R2,GOLMDH                                                        
         TM    4(R2),X'20'                                                      
         BO    CLT                                                              
*                                                                               
         BAS   RE,CLRMD                                                         
*                                                                               
         MVI   ERRCD,INVMED                                                     
         CLI   5(R2),1                                                          
         BNE   GLERR                                                            
*                                                                               
         GOTO1 =V(MEDGET),DMCB,(GOLMD,AGYALPHA),VDATAMGR,WORK,RR=RB             
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BE    GLERR                                                            
*                                                                               
         MVC   SVAGYMD,WORK                                                     
         MVI   SVADVAGY,0          CLEAR THIS FIELD FOREVER!                    
         MVC   GOLMDNM(10),WORK+1                                               
         OI    4(R2),X'20'                                                      
*                                                                               
         EJECT                                                                  
CLT      LA    R2,GOLCLH                                                        
         TM    4(R2),X'20'                                                      
         BO    PRD                                                              
*                                                                               
         BAS   RE,CLRCL                                                         
*                                                                               
         MVI   ERRCD,INVCLI                                                     
         CLI   5(R2),0                                                          
         JE    GLERR                                                            
         GOTO1 MOVE                                                             
*                                                                               
         CLI   5(R2),2                                                          
         BL    GLERR                                                            
         CLI   5(R2),3                                                          
         BH    GLERR                                                            
*                                                                               
         MVC   QCLT,WORK                                                        
         MVC   DMCB+4(4),=X'D9000A14'  CLPACK                                   
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),QCLT,SVCLT                                             
         CLI   0(R1),0                                                          
         BNE   GLERR                                                            
* READ CLIENT                                                                   
CLT2     XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
*                                                                               
         GOTO1 HIGH                                                             
         MVI   ERRCD,INVCLI                                                     
         CLC   KEY(13),KEYSAVE                                                  
         JNE   GLERR                                                            
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         USING CLTHDRD,R6                                                       
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
CLT6     MVC   GOLCLNM,CNAME                                                    
         MVC   SVPROF(15),CPROF                                                 
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   SVRFPGRP,CRFPGRP                                                 
         MVC   SVCACCS,CACCESS                                                  
         MVI   SVMACCS,X'FF'       SUPPRESS MKT LIMIT FOR NOW                   
         MVC   SVOFFC,COFFICE                                                   
         DROP  R6                                                               
*                                                                               
         CLI   T202FFD+6,C'+'      TEST MKT LIMIT ACCESS                        
         BE    CLT8                                                             
         BRAS  RE,CALLOFCR         ELSE CHECK LIMIT ACCESS NOW                  
         SPACE 1                                                                
* READ G0 PROFILE *                                                             
         SPACE 1                                                                
CLT8     XC    WORK,WORK                                                        
         XC    SVPPROF,SVPPROF     CLEAR IN CASE NOT FOUND                      
         MVC   WORK(4),=C'S0G0'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),GOLMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         MVC   WORK+16(16),WORK    ** SAVE SEARCH KEY                           
         MVC   WORK+6(4),=X'FFFFFFFF' FORCE NO MATCH ON MEDIA/CLT               
         XC    WORK+10(6),WORK+10                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVPPROF,VDATAMGR                                  
                                                                                
*===========================================================                    
******** MEDIA CODES R AND X ARE ALWAYS 1 DECIMAL                               
* MEDIA CODES R ARE ALWAYS 1 DECIMAL                                            
*===========================================================                    
                                                                                
         MVI   TWODEC,C'N'                                                      
         MVI   TWODCIMPS,C'N'                                                   
         CLI   GOLMD,C'R'                                                       
         JE    CLT10                                                            
***      CLI   GOLMD,C'X'            MEDIA X WILL FOLLOW MEDIA T'S              
***      JE    CLT10                                                            
         MVC   TWODEC,SVPPROF+7    FOR MEDIA T, USE AGENCY OPTION               
         MVC   TWODCIMPS,SVPPROF+8                                              
* READ B0 PROF                                                                  
CLT10    XC    SVB0PROF,SVB0PROF   CLEAR IN CASE NOT FOUND                      
         MVC   WORK(16),WORK+16    RESTORE SEARCH ARGS                          
         MVC   WORK(4),=C'S0B0'                                                 
         GOTO1 (RF),DMCB,WORK,SVB0PROF,VDATAMGR                                 
*                                                                               
CLTX     OI    4(R2),X'20'                                                      
         B     PRD                                                              
         EJECT                                                                  
PRD      LA    R2,GOLPRH                                                        
         TM    4(R2),X'20'                                                      
         BO    EST                                                              
*                                                                               
         BAS   RE,CLRPR                                                         
         XC    QPRD2,QPRD2         CLEAR PARTNER                                
         MVI   SVPRD2,0                                                         
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         XC    ELEM,ELEM                                                        
         GOTO1 VSCANNER,DMCB,(R2),ELEM,C',=-='                                  
*                                                                               
         MVI   ERRCD,INVPROD                                                    
         LA    R4,ELEM                                                          
         CLI   0(R4),2                                                          
         BL    GLERR                                                            
         CLI   0(R4),3                                                          
         BH    GLERR                                                            
         CLI   1(R4),0                                                          
         BNE   GLERR                                                            
         MVC   QPRD,12(R4)                                                      
         CLC   QPRD,=C'AAA'                                                     
         BE    GLERR                                                            
*                                                                               
         LA    R4,32(R4)                                                        
         OC    0(2,R4),0(R4)       TEST FOR MORE INPUT                          
         BZ    PRD4                                                             
         SPACE 1                                                                
* EDIT PARTNER PRD CODE *                                                       
         SPACE 1                                                                
         CLI   0(R4),2                                                          
         BL    GLERR                                                            
         CLI   0(R4),3                                                          
         BH    GLERR                                                            
         CLI   1(R4),0                                                          
         BNE   GLERR                                                            
         MVC   QPRD2,12(R4)                                                     
         CLC   QPRD2,=C'AAA'                                                    
         BE    GLERR                                                            
* PRD CODES SHOULD BE IN ALPHA SEQ                                              
         CLC   QPRD,QPRD2                                                       
         BNL   GLERR                                                            
*                                                                               
         CLC   QPRD,=C'POL'        CPP SHOULD NOT HAVE PARTNER                  
         BE    GLERR                                                            
         B     PRD10                                                            
*                                                                               
PRD4     CLC   =C'POL',QPRD                                                     
         BNE   PRD10                                                            
         CLC   =C'LOCKIN',GOLPLNR                                               
         BE    GLERR                                                            
         MVC   GOLPRNM,=CL20'COST PER POINT'                                    
         MVI   SVPRD,X'FF'                                                      
         B     PRDX                                                             
         SPACE 1                                                                
* READ PRODUCT HEADER(S) *                                                      
         SPACE 1                                                                
PRD10    XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),QPRD                                                    
*                                                                               
         GOTO1 HIGH                                                             
         MVI   ERRCD,BADPRD                                                     
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GLERR                                                            
*                                                                               
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         USING PRDHDRD,R6                                                       
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   GOLPRNM,PNAME                                                    
         MVC   SVPRD,PCODE+1                                                    
         MVC   SVPTAL,PTAL                                                      
*                                                                               
         OC    QPRD2,QPRD2                                                      
         BZ    PRDX                                                             
         SPACE 1                                                                
* READ PARTNER PRODUCT HEADER *                                                 
         SPACE 1                                                                
         MVC   KEY+4(3),QPRD2                                                   
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERRCD,BADPRD2                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GLERR                                                            
*                                                                               
         GOTO1 GETREC                                                           
         MVI   GOLPRNM+10,C'/'                                                  
         MVC   GOLPRNM+11(9),PNAME                                              
         MVC   SVPRD2,PCODE+1                                                   
*                                                                               
PRDX     OI    4(R2),X'20'                                                      
         DROP  R6                                                               
         EJECT                                                                  
EST      LA    R2,GOLESH                                                        
         TM    4(R2),X'20'                                                      
         BO    TGT                                                              
*                                                                               
EST00    BAS   RE,CLRES                                                         
*                                                                               
         MVI   ERRCD,INVEST                                                     
         CLI   5(R2),0                                                          
         JE    GLERR                                                            
*                                                                               
         GOTO1 PACK                                                             
*                                                                               
         LTR   R0,R0                                                            
         BNP   GLERR                                                            
         CHI   R0,255                                                           
         BH    GLERR                                                            
         STC   R0,SVEST                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),SVEST                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   GLERR                                                            
*                                                                               
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         USING ESTHDRD,R6                                                       
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVSTART(12),ESTART  SAVE EBC START/END DATES                     
         MVC   SVMSTR,EMSTRIND     SAVE MASTER EST FLAG                         
         MVC   SVEOWSDY,EOWSDAY    SAVE OUT-OF-WEEK DATA FLAG                   
         MVC   SVEDAILY,EDAILY     SAVE DAILY EST FLAG                          
         MVC   SVEFLAG1,EFLAG1     SAVE EFLAG (FOR SUPERDESK)                   
                                                                                
* READ PG PROF IF NEEDED                                                        
                                                                                
         CLI   SVCXTRA+8,C'P'      TEST P&G GOALS REQ'D                         
         BNE   EST01                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0PG'                                                 
         GOTO1 VDATCON,DMCB,SVEND,(X'20',WORK+16)  GET END YYMMDD               
         MVC   WORK+3(1),WORK+17    MOVE BDCST YEAR TO PROFILE                  
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),GOLMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVPGPROF,VDATAMGR                                 
*                                                                               
         CLC   SVPGPROF+0(1),SVEST TEST DOING RIGHT ESTIMATE                    
         BNE   BADPGEST                                                         
*                                                                               
EST01    MVI   SVESTYP,0           RESET                                        
         CLI   SVPRD,X'FF'         TEST POL                                     
         BNE   *+10                                                             
         MVC   SVESTYP,ECPPTYPE    SAVE EST TYPE FOR CPP INPUT                  
*                                                                               
         XC    SVCPPCL(4),SVCPPCL  CLEAR CPP CLT/EST CODES                      
         CLI   SVPRD,X'FF'                                                      
         BE    *+16                                                             
         MVC   SVCPPCL,ECPPCLT                                                  
         MVC   SVCPPES,ECPPEST                                                  
         EJECT                                                                  
* GET MMMDD/YY DATES                                                            
         GOTO1 VDATCON,DMCB,(0,ESTART),(5,GOLESNM)                              
         MVI   GOLESNM+8,C'-'                                                   
*                                                                               
         GOTO1 (RF),(R1),(0,EEND),(5,GOLESNM+9)                                 
*                                                                               
         MVC   SVDEMOS(36),EDEMOS  SAVE FIRST FOUR DEMOS                        
         MVI   SVNEWDEM,0                                                       
         TM    ECNTRL,X'01'        TEST CONVERTED EST                           
         BZ    EST2                NO                                           
         SPACE 1                                                                
* NEW DEMO PROCESSING *                                                         
         SPACE 1                                                                
         MVI   SVNEWDEM,C'Y'                                                    
         XC    SVDEMOS,SVDEMOS                                                  
* BUILD DBLOCK                                                                  
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING DBLOCKD,RE                                                       
         MVC   DBFILE(3),=C'TP '                                                
         MVI   DBSELMED,C'T'       FORCE MEDIA CODE                             
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         MVC   DBCOMFCS,VCOMFACS                                                
* GET DEMOCON ADDRESS                                                           
         XC    DMCB(8),DMCB                                                     
         GOTO1 VCALLOV,DMCB,,X'D9000AE0'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JH    EST01A                                                           
         GOTO1 (RF),DMCB,(2,EDEMOS),(2,WORK),(C'S',ELEM),EUSRNMS                
         J     EST01B                                                           
EST01A   GOTO1 (RF),DMCB,(2,EDEMOS),(2,WORK),(C'S',ELEM),EUSRNMS,      *        
               ENONTDMS                                                         
* NOW SAVE FIRST TWO DEMO CODES AND NAMES                                       
EST01B   MVC   SVDEMOS(3),EDEMOS                                                
         MVC   SVDEMOS+3(7),WORK                                                
         MVC   SVDEMOS+10(3),EDEMOS+3                                           
         MVC   SVDEMOS+13(7),WORK+7                                             
         B     EST2                                                             
*                                                                               
BADPGEST MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(PGESTERR)                                              
         B     GLERR                                                            
*                                                                               
EST2     CLI   SVCPPES,0                                                        
         BE    EST3                                                             
* READ TO GET CPP EST DATES                                                     
         OC    SVCPPCL,SVCPPCL                                                  
         BNZ   *+10                                                             
         MVC   SVCPPCL,SVCLT                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCPPCL                                                 
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),SVCPPES                                                 
         GOTO1 HIGH                                                             
         MVI   ERRCD,BADCPPES                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GLERR                                                            
         GOTO1 GETREC                                                           
*                                                                               
         CLC   SVEND(6),EEND       BRAND EST END TO CPP EST END                 
         BNH   EST3                IF LOW OR EQUAL, DONE                        
         CLI   ECPPEST,0           TEST FOR CONTINUATION                        
         BE    GLERR               NO - ERROR                                   
         MVC   KEY+7(1),ECPPEST                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         CLC   SVEND(6),EEND                                                    
         BH    GLERR                                                            
         MVC   SVCPPES2,KEY+7      SAVE CONT EST NUM                            
EST3     DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),GOLMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVSPPROF,VDATAMGR                                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
* READ DAYPART MENU *                                                           
         SPACE 1                                                                
EST4     DS    0H                                                               
         LA    R1,DMCB                                                          
         MVC   0(2,R1),AGYALPHA                                                 
         MVC   2(1,R1),GOLMD                                                    
         MVC   3(1,R1),EDAYMENU                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 =V(DPTRD),(R1),,REC,VDATAMGR,RR=RB                               
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BE    *+12                                                             
         TM    8(R1),X'08'                                                      
         BZ    EST5                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(BADMENU)                                             
         B     GLERR                                                            
         SPACE 1                                                                
* SAVE VALID DAYPART CODES                                                      
         SPACE 1                                                                
EST5     XC    SVMENU,SVMENU                                                    
*                                                                               
         LA    R1,REC                                                           
         LA    R4,SVMENU                                                        
EST6     CLI   0(R1),0             TEST FOR EOT                                 
         BE    EST7                                                             
         MVC   0(1,R4),0(R1)                                                    
         LA    R4,1(R4)                                                         
         LA    R1,5(R1)                                                         
         B     EST6                                                             
         EJECT                                                                  
* BUILD SUB-EST LIST FOR MASTER EST                                             
         SPACE 1                                                                
EST7     CLI   SVMSTR,C'M'                                                      
         BNE   EST20                                                            
*                                                                               
         LA    R4,SVSUBEST                                                      
         MVI   KEY+7,0             RESET EST NUM IN KEY                         
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         USING ESTHDRD,R6                                                       
*                                                                               
EST10    MVC   KEY+8(5),=5X'FF'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   EST20                                                            
         CLC   KEY+7(1),SVEST      IS IT US                                     
         BE    EST10               YES - SKIP                                   
         GOTO1 GETREC                                                           
         CLI   EMSTRIND,C'S'       TEST SUB-EST                                 
         BNE   EST10                                                            
         CLC   EMSTREST,SVEST      TEST RIGHT MASTER                            
         BNE   EST10                                                            
* ADD EST TO LIST                                                               
         MVC   0(1,R4),EKEY+7      EST NUM                                      
         GOTO1 VDATCON,DMCB,ESTART,(2,1(R4))                                    
         GOTO1 (RF),(R1),EEND,(2,3(R4))                                         
         LA    R4,5(R4)            NEXT SUB-EST                                 
         B     EST10                                                            
*                                                                               
EST20    DS    0H                                                               
         CLI   QPRD2,0             TEST PARTNER                                 
         BE    EST30               NO                                           
* READ PARTNER ESTIMATE *                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),QPRD2                                                   
         MVC   KEY+7(1),SVEST                                                   
         GOTO1 HIGH                                                             
         MVI   ERRCD,BADPR2ES                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GLERR                                                            
*                                                                               
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVSTART2(12),ESTART     SAVE PARTNER DATES                       
         TM    EFLAG1,EF1SDE           IF PIGGYBACK NOT ON                      
         BO    *+8                                                              
         NI    SVEFLAG1,X'FF'-EF1SDE   TURN IT OFF                              
*                                                                               
* NEED TO GET FLAG FROM POL EST, ALSO                                           
*                                                                               
EST30    XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),SVEST                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ESTX                                                             
*                                                                               
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         OC    SVEFLAG1,EFLAG1                                                  
*                                                                               
ESTX     OI    4(R2),X'20'                                                      
         DROP  R6                                                               
         EJECT                                                                  
TGT      LA    R2,GOLTGH                                                        
         TM    4(R2),X'20'                                                      
         BO    TGTX                                                             
*                                                                               
         BAS   RE,CLRTG                                                         
         MVI   SVOPT2,0                                                         
*                                                                               
* TARGET SELECTION NOT IMPLEMENTED                                              
*                                                                               
         OC    SVCPPCL,SVCPPCL                                                  
         BZ    TGT1                                                             
* DISPLAY CPP CLIENT/ESTIMATE                                                   
         MVC   GOLTGNM,SPACES                                                   
         MVC   GOLTGNM(7),=C'CPPEST='                                           
         MVC   DMCB+4(4),=X'D9000A15'   CLUNPK                                  
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),SVCPPCL,GOLTGNM+7                                      
         LA    R4,GOLTGNM+9                                                     
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'/'                                                       
         ZIC   R0,SVCPPES                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R4),DUB                                                      
*                                                                               
TGT1     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    TGT10                                                            
         CLC   =C'BRDDOL',GOLTG                                                 
         BE    *+14                                                             
         CLC   =C'BD',GOLTG                                                     
         BNE   TGT2                                                             
         MVC   GOLTGNM(13),=C'BRAND DOLLARS'                                    
         B     TGT4                                                             
TGT2     CLC   =C'BP',GOLTG                                                     
         BNE   TGT3                                                             
* MAKE SURE TIER IS 2 DIGITS                                                    
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(BADTIER)                                             
         CLI   GOLTG+2,C'0'                                                     
         BL    GLERR                                                            
         CLI   GOLTG+2,C'9'                                                     
         BH    GLERR                                                            
         CLI   GOLTG+3,C'0'                                                     
         BL    GLERR                                                            
         CLI   GOLTG+3,C'9'                                                     
         BH    GLERR                                                            
         B     TGT3X                                                            
*                                                                               
TGT3     CLC   =C'BRDPCT',GOLTG                                                 
         BNE   TGT10                                                            
         MVI   ERRCD,INVERR                                                     
         CLI   GOLTG+2,0                                                        
         BNL   *+12                                                             
         LA    R2,GOLTGH                                                        
         B     GLERR                                                            
TGT3X    MVC   GOLTGNM(14),=C'BRAND PERCENTS'                                   
*                                                                               
TGT4     MVI   ERRCD,NOTMSTR                                                    
         CLI   SVMSTR,C'M'                                                      
         BE    *+12                                                             
         LA    R2,GOLESH                                                        
         B     GLERR                                                            
* PRD MUST NOT BE POL                                                           
         CLI   SVPRD,X'FF'                                                      
         BNE   TGTX                                                             
         MVI   ERRCD,INVERR                                                     
         LA    R2,GOLPRH                                                        
         B     GLERR                                                            
*                                                                               
TGT10    CLI   SVMSTR,C'M'         NO MASTER EST INPUT EXCEPT BP/BD             
         BNE   TGTX                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOTBPBD)  INPUT MUST BE BP OR BD                     
         LA    R2,GOLTGH                                                        
         B     GLERR                                                            
*                                                                               
TGTX     DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
* EDIT OPTIONS                                                                  
         SPACE 1                                                                
         XC    SVOPTS,SVOPTS                                                    
         XC    SVIDR,SVIDR                                                      
         LA    R2,GOLOPTSH                                                      
         CLI   5(R2),0                                                          
         BE    OPT100                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         GOTO1 VSCANNER,DMCB,(R2),ELEM                                          
*                                                                               
         LA    R4,ELEM                                                          
         B     *+8                                                              
*                                                                               
OPT2     LA    R4,32(R4)                                                        
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   0(R4),0                                                          
         BE    OPT100                                                           
         CLI   0(R4),8                                                          
         BH    GLERR                                                            
         ZIC   R5,0(R4)            GET INPUT LENGTH                             
         BCTR  R5,0                                                             
         EX    R5,TEST30FR                                                      
         BNE   *+12                                                             
         MVI   SVOPT1,C'X'                                                      
         B     OPT2                                                             
*                                                                               
         EX    R5,TESTTRA                                                       
         BNE   *+12                                                             
         MVI   SVOPT1,C'X'                                                      
         B     OPT2                                                             
*                                                                               
         EX    R5,TESTCOPY                                                      
         BNE   *+12                                                             
         MVI   SVOPT1,C'C'                                                      
         B     OPT2                                                             
*                                                                               
         EX    R5,TESTLOCK                                                      
         BNE   OPT2B                                                            
         MVI   SVOPT1,C'L'                                                      
         B     OPT2                                                             
*                                                                               
OPT2B    EX    R5,TESTGOAL         TEST GOAL LOCKIN DISPLAY                     
         BE    OPT2C                                                            
         EX    R5,TESTGOL2                                                      
         BNE   OPT2D                                                            
OPT2C    MVI   SVOPT1,C'G'                                                      
         B     OPT2                                                             
*                                                                               
OPT2D    CLI   SVB0PROF+9,C'Y'     TEST PURPOSE CODES REQD                      
         BE    OPTPC                                                            
         CLC   =C'IDR ',12(R4)                                                  
         BNE   GLERR               INVALID OPTION                               
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL                                 
         BE    *+12                                                             
         CLI   SVPPROF+4,C'Y'      MUST BE IN PROFILE TO USE IDR OPTION         
         BNE   PRFNOTST                                                         
         CLI   SVPRD2,0            CANNOT USE 2ND PRODUCT WITH ID               
         BNE   PRDIDROP                                                         
         CLC   =C'LIST',22(R4)     CANNOT HAVE ID STARTING W/ LIST              
         BE    GLERR                                                            
         CLI   1(R4),5             ID CAN BE 5-6 CHAR ONLY                      
         BL    GLERR                                                            
         CLI   1(R4),6                                                          
         BH    GLERR                                                            
         MVC   SVIDR,22(R4)                                                     
         OC    SVIDR,SPACES                                                     
         B     OPT2                                                             
         SPACE 1                                                                
*=========================================================                      
* SPECIAL FOR IDR=PURPOSE CODE                                                  
*=========================================================                      
         SPACE 1                                                                
OPTPC    CLC   =C'PUR',12(R4)                                                   
         BNE   GLERR                                                            
*                                                                               
         CLI   1(R4),0                                                          
         BE    OPTPCERR                                                         
         CLI   1(R4),6             MAX LEN 6                                    
         BH    OPTPCERR                                                         
         MVC   SVIDR,22(R4)                                                     
         OC    SVIDR,SPACES                                                     
* MAKE SURE PURPOSE CODE IS VALID                                               
K        USING PRPRECD,KEY         READ PURPOSE CODE RECORD                     
*                                                                               
         XC    KEY,KEY                                                          
         MVI   K.PRPKTYP,PRPKTYPQ                                               
         MVI   K.PRPKSUB,PRPKSUBQ                                               
         MVC   K.PRPKAGY,AGYALPHA                                               
         MVC   K.PRPKMED,GOLMD                                                  
         MVC   K.PRPCODE,SVIDR                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   OPTPCER2                                                         
         B     OPT2                                                             
         DROP  K                                                                
*                                                                               
OPTPCERR LA    R0,BADPURP                                                       
         B     *+8                                                              
OPTPCER2 LA    R0,PURPNF                                                        
*                                                                               
         STCM  R0,3,NERRCD                                                      
         MVI   ERRCD,NEWERRS                                                    
         B     GLERR                                                            
*                                                                               
OPT100   XC    GOLTGNM,GOLTGNM                                                  
         MVC   GOLTGNM(7),SVDEMOS+3                                             
*                                                                               
         CLI   SVEDAILY,C'Y'                                                    
         BNE   *+10                                                             
         MVC   GOLTGNM+8(7),=C'*DAILY*'                                         
*                                                                               
         OI    GOLTGNMH+6,X'80'                                                 
*                                                                               
         CLC   =C'LOCKIN',GOLPLNR                                               
         BNE   *+8                                                              
         MVI   SVOPT1,C'L'                                                      
*                                                                               
         CLI   SVOPT1,C'L'                                                      
         BNE   OPT102                                                           
         MVC   GOLTGNM(15),=CL15'*LOCKED BUYS* '                                
         B     OPT104                                                           
*                                                                               
OPT102   CLI   SVOPT1,C'G'                                                      
         BNE   OPT104                                                           
         MVC   GOLTGNM(15),=CL15'*LOCKED GOALS*'                                
*                                                                               
OPT104   OI    4(R2),X'20'         SET VALIDATED                                
         B     OPTX                                                             
*                                                                               
TEST30FR CLC   12(0,R4),=CL8'XFR     '                                          
TESTTRA  CLC   12(0,R4),=CL8'TRANSFER'                                          
TESTCOPY CLC   12(0,R4),=CL8'COPY    '                                          
TESTLOCK CLC   12(0,R4),=CL8'LOCKIN  '                                          
TESTGOAL CLC   12(0,R4),=CL8'GLOCKIN '                                          
TESTGOL2 CLC   12(0,R4),=CL8'LOCKGOAL'                                          
         SPACE 2                                                                
OPTX     CLI   SVOPT1,C'X'         TEST TRANSFER OPTION                         
         BE    EXXMOD                                                           
         CLI   SVOPT1,C'C'         TEST COPY OPTION                             
         BE    EXXMOD                                                           
         CLI   SVOPT1,C'L'         TEST LOCKIN OPTION                           
         BE    EXXMOD                                                           
         CLI   SVOPT1,C'G'         TEST GOAL LOCKIN OPTION                      
         BE    EXXMOD                                                           
         CLI   SVSCRN,X'F4'        TEST TRANSFER SCREEN ACTIVE BEFORE           
         BE    EXXMOD              YES - EXIT                                   
         LA    R2,GOLACT1H                                                      
         CLI   5(R2),0             ANY DATA INPUT                               
         BNE   EXXMOD                                                           
         SPACE 1                                                                
* CLEAR THE SCREEN *                                                            
         SPACE 1                                                                
         LA    R4,GOLACT1H                                                      
CLRSC2   SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,GLCLROC                                                       
         BZ    CLRSC4                                                           
         EX    R5,GLCLRXC                                                       
         FOUT  (R4)                                                             
CLRSC4   LA    R4,9(R5,R4)                                                      
         CLI   0(R4),0                                                          
         BNE   CLRSC2                                                           
         B     EXXMOD                                                           
*                                                                               
PRFNOTST MVI   ERRCD,NEWERRS       G0 PROFILE NOT SETUP FOR IDR FEATURE         
         MVC   NERRCD,=AL2(PRFNTSET)                                            
         B     GLERR                                                            
*                                                                               
PRDIDROP MVI   ERRCD,NEWERRS       CANNOT USE A 2ND PRD 2/ IDR OPTION           
         MVC   NERRCD,=AL2(NPRDIDR)                                             
         B     GLERR                                                            
*                                                                               
GLCLROC  OC    8(0,R4),8(R4)                                                    
GLCLRXC  XC    8(0,R4),8(R4)                                                    
         SPACE 2                                                                
*                                                                               
GLERR    GOTO1 ERROR                                                            
*                                                                               
EXIT     OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
EXXMOD   XIT1                                                                   
         EJECT                                                                  
CLRMD    NI    GOLMDH+4,X'DF'                                                   
         XC    GOLMDNM,GOLMDNM                                                  
         FOUT  GOLMDNMH                                                         
*                                                                               
CLRCL    NI    GOLCLH+4,X'DF'                                                   
         XC    GOLCLNM,GOLCLNM                                                  
         XC    SVQOLD,SVQOLD       CLEAR SAVED REQUEST LISTS                    
         XC    SVQNEW,SVQNEW                                                    
         FOUT  GOLCLNMH                                                         
*                                                                               
CLRPR    NI    GOLPRH+4,X'DF'                                                   
         XC    GOLPRNM,GOLPRNM                                                  
         FOUT  GOLPRNMH                                                         
*                                                                               
CLRES    NI    GOLESH+4,X'DF'                                                   
         XC    GOLESNM,GOLESNM                                                  
         FOUT  GOLESNMH                                                         
*                                                                               
         XC    SVMKT,SVMKT         CLEAR LAST MARKET REQUESTED                  
         MVI   SVMSTR,0            RESET MASTER EST FLAG                        
         XC    SVSUBEST,SVSUBEST   CLEAR SUB-EST LIST                           
         XC    SVTLNT,SVTLNT       CLEAR TALENT FACTORS                         
*                                                                               
CLRTG    NI    GOLTGH+4,X'DF'                                                   
         XC    GOLTGNM,GOLTGNM                                                  
         FOUT  GOLTGNMH                                                         
*                                                                               
         NI    GOLOPTSH+4,X'DF'                                                 
         SPACE 1                                                                
* CLEAR 'EDITED' FLAGS *                                                        
         SPACE 1                                                                
         CLI   SVSCRN,X'FE'        TEST XFR SCREEN ACTIVE                       
         BE    CLRXFR                                                           
*                                                                               
         LA    R4,GOLACT1H                                                      
         SR    R0,R0                                                            
CLRFLAGS NI    4(R4),X'DF'                                                      
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   CLRFLAGS                                                         
         BR    RE                                                               
*                                                                               
CLRXFR   LA    R4,GOLOPTSH         TRANSFER SCREEN IS DIFFERENT                 
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0               POINT TO PERIOD FIELD                        
         NI    4(R2),X'DF'         AND CLEAR EDITED FLAG                        
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T202FFD+6                                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,SVAGYMD                                                 
         MVC   OFCLMT(4),T202FFD+6                                              
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD                                                       
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK2),VCOMFACS                                  
         MVI   ERRCD,ERRLOCK                                                    
         CLI   0(R1),0                                                          
         JE    EXXMOD                                                           
         GOTO1 ERROR                                                            
         LTORG                                                                  
         SPACE 1                                                                
         EJECT                                                                  
* DEDBLOCK                                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE SPGOLWRK                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPGENPURP                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083SPGOL01   11/19/19'                                      
         END                                                                    
