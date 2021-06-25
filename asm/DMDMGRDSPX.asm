*          DATA SET DMDMGRDSPX AT LEVEL 005 AS OF 10/16/00                      
*PHASE DMGRDSPA                                                                 
*INCLUDE FATABOFF                                                               
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE PERVAL                                                                 
*INCLUDE XSORT                                                                  
         TITLE 'DMDMGRSPC - HANDLE DATAMGR DATA SPACE '                         
         PRINT NOGEN                                                            
DMGRSPC  CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,**DMSP**,=A(WORKAREA),RA,R9                          
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,INIT             READ CARDS ECT                               
         CLI   MODE,C'I'                                                        
         BNE   DM010                                                            
         BAS   RE,MAIN             MAIN LOOP FOR INIT                           
         BAS   RE,SETWAIT                                                       
         B     XBASE                                                            
*                                                                               
DM010    CLI   MODE,C'C'                                                        
         BNE   *+8                                                              
         BAS   RE,CLEAR            MAIN PROG FOR CLEAR                          
DM020    CLI   MODE,C'B'                                                        
         BNE   *+8                                                              
         BAS   RE,BUILDPGM                                                      
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
EXITEQ   CR    RB,RB                                                            
         B     EXIT                                                             
EXITNE   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
M24SET   SLL   RE,1                SET AMODE 24                                 
         SRL   RE,1                                                             
         BSM   0,RE                                                             
*                                                                               
M31SET   ICM   RE,8,=X'80'         SET AMODE 31                                 
         BSM   0,RE                                                             
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
*                                                                               
         LA    R3,CARD                                                          
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT020                                                          
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BE    INIT010                                                          
         B     XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
INIT020  CLI   MODE,C'I'           TEST INIT MODE                               
         BNE   EXITEQ                                                           
*                                                                               
         BAS   RE,SETOPS           SET UP OPER COMMS                            
*                                                                               
         LA    R0,4                MAKE JOB NON SWAPPABLE                       
         LNR   R0,R0                                                            
         SVC   247                                                              
*                                                                               
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM                                       *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
         LA    RF,(RMAXRES-1)/64   PAGES FOR HEADERS                            
*                                                                               
         LA    R1,TTABLE           TABLE TABLE                                  
MAIN005  CLI   0(R1),X'FF'                                                      
         BE    MAIN010                                                          
*                                                                               
         ST    RF,PAGES            UPDATE NUMBER OF PAGES                       
         SR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         AR    RF,R0                                                            
         LA    R1,L'TTABLE(R1)     NEXT TABLE TABLE                             
         B     MAIN005                                                          
*                                                                               
MAIN010  LA    R1,STABLE           SYSTEM TABLE                                 
MAIN011  ST    RF,PAGES                                                         
         CLI   0(R1),X'FF'         FF = END OF TABLE                            
         BE    MAIN020                                                          
         SR    R0,R0                                                            
         IC    R0,2(R1)            NUMBER OF PAGES FOR SYS                      
         SR    RE,RE                                                            
         IC    RE,3(R1)            NUMBER OF PAGES FOR BUFFER                   
         AR    RF,R0                                                            
         AR    RF,RE                                                            
         LA    R1,L'STABLE(R1)                                                  
         B     MAIN011                                                          
*                                                                               
MAIN020  LA    R1,RTABLE           CALCULATE NUMBER OF PAGES                    
MAIN022  ST    RF,PAGES                                                         
         CLI   0(R1),X'FF'         FF = END OF TABLE                            
         BE    MAIN030                                                          
         SR    R0,R0                                                            
         IC    R0,10(R1)           NUMBER OF PAGES FOR THIS ONE                 
         AR    RF,R0                                                            
         LA    R1,L'RTABLE(R1)                                                  
         B     MAIN022                                                          
*                                                                               
MAIN030  BAS   RE,FREESPC          FREE ANY OLD DATASPACES                      
         BAS   RE,MAKESPC          MAKE A NEW ONE                               
         BAS   RE,GETSPC           GET ADDRESS OF IT                            
*                                                                               
         MVC   AHEADER,DMOFFS                                                   
         MVC   ADSDATA,DMOFFS      STARTING FROM OFFS                           
         L     R1,ADSDATA                                                       
         LA    RF,(RMAXRES-1)/64   PAGES FOR HEADERS                            
         LA    RF,1(RF)                                                         
         SLL   RF,12               * 1024                                       
         AR    R1,RF                                                            
         ST    R1,ADSDATA                                                       
         BAS   RE,HEADERS          BUILD HEADERS                                
*NOP     BAS   RE,BUILDPGM         BUILD PROGRAM TABLE                          
*                                                                               
MAIN040  LA    R3,STABLE                                                        
MAIN041  CLI   0(R3),X'FF'         TEST FOR EOT                                 
         BE    MAINX                                                            
*                                                                               
         MVC   SEOPN,1(R3)         BUILD AN ENTRY FOR EACH SE                   
         MVC   RESNUM,1(R3)                                                     
         BAS   RE,OPENSYS                                                       
         BAS   RE,BUILDDS          BUILD SYSTEM DATASPACE BLOCK                 
MAIN050  LA    R3,L'STABLE(R3)                                                  
         B     MAIN041                                                          
*                                                                               
MAINX    B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        BUILD HEADER INFO                                  *                   
*************************************************************                   
         SPACE 1                                                                
HEADERS  NTR1                                                                   
*                                                                               
         SAC   512                 SET UP AMODE                                 
         BAS   RE,M31SET                                                        
         LAM   R2,R2,DMALET                                                     
*                                                                               
HDR010   LA    R1,TTABLE           TABLE TABLE                                  
HDR011   CLI   0(R1),X'FF'                                                      
         BE    HDR020                                                           
*                                                                               
         L     R2,AHEADER                                                       
         A     R2,0(R1)                                                         
         MVC   0(4,R2),ADSDATA     SAVE A(TABLE)                                
         L     RF,ADSDATA                                                       
         SR    R0,R0                                                            
         IC    R0,4(R1)            SET R0 TO NUMBER OF PAGES                    
         SLL   R0,12                                                            
         AR    RF,R0               BUMP SYSTEM DATA BY 4096 PER PAGE            
         ST    RF,ADSDATA                                                       
*                                                                               
         LA    R1,L'TTABLE(R1)                                                  
         B     HDR011                                                           
*                                                                               
HDR020   LA    R3,STABLE           R3=SYSTEM TABLE                              
HDR021   CLI   0(R3),X'FF'         TEST FOR EOT                                 
         BE    HDR090                                                           
*                                                                               
         L     R2,AHEADER          HEADER STARTS AT R2                          
         SR    R1,R1                                                            
         ICM   R1,3,0(R3)          RESOURCE NUMBER                              
         SLL   R1,6                * 64                                         
         AR    R2,R1                                                            
         USING DMSPACED,R2                                                      
*                                                                               
         L     R4,=V(SELIST)       MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
HDR030   CLC   1(1,R3),SESYS       TEST NAME                                    
         BE    HDR040                                                           
         BXLE  R4,R0,HDR030        NEXT                                         
         DC    H'0'                ERROR SE SYS NOT FOUND                       
*                                                                               
HDR040   MVC   DSPNAME(7),SENAME                                                
         MVI   DSPNAME+7,C' '                                                   
         MVC   DSPECB,ADSDATA                                                   
         DROP  R4,R2                                                            
*                                                                               
         L     R2,ADSDATA          DATA STARTS AT R2                            
*                                                                               
         SR    R1,R1               BUMP ADSDATA                                 
         SR    RF,RF                                                            
         IC    R1,2(R3)                                                         
         IC    RF,3(R3)                                                         
         AR    R1,RF                                                            
         SLL   R1,12               PAGES*4K                                     
         A     R1,ADSDATA                                                       
         ST    R1,ADSDATA                                                       
*                                                                               
         LA    R3,L'STABLE(R3)     NEXT RESOURCE                                
         B     HDR021                                                           
*                                                                               
HDR090   LA    R3,RTABLE           R3=RESOURCE TABLE                            
*                                                                               
HDR100   CLI   0(R3),X'FF'         TEST FOR EOT                                 
         BE    HEADERX                                                          
*                                                                               
         L     R2,AHEADER          HEADER STARTS AT R2                          
         SR    R1,R1                                                            
         IC    R1,9(R3)            RESOURCE NUMBER                              
         SLL   R1,6                * 64                                         
         AR    R2,R1                                                            
         USING DMSPACED,R2                                                      
         MVC   DSPNAME,0(R3)       SET NAME AND ADDRESS                         
         MVC   DSPECB,ADSDATA                                                   
         DROP  R2                                                               
*                                                                               
         L     R2,ADSDATA          DATA STARTS AT R2                            
*                                                                               
         SR    R1,R1               BUMP ADSDATA                                 
         IC    R1,10(R3)                                                        
         SLL   R1,12               PAGES*4K                                     
         A     R1,ADSDATA                                                       
         ST    R1,ADSDATA                                                       
*                                                                               
         LA    R3,L'RTABLE(R3)     NEXT RESOURCE                                
         B     HDR100                                                           
*                                                                               
HEADERX  SAC   0                   RESET AMODE                                  
         BAS   RE,M24SET                                                        
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        OPENSYS / OPEN ALL FILES FOR SEOPNN                *                   
*************************************************************                   
         SPACE 1                                                                
OPENSYS  NTR1                                                                   
         L     R4,=V(SELIST)       MUST HAVE SELIST                             
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
OPENS1   EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SESYS,SEOPN         TEST NUM                                     
         BE    OPENS2                                                           
         BXLE  R4,R0,OPENS1        NEXT                                         
         DC    H'0'                ERROR SE SYS NOT FOUND                       
*                                                                               
OPENS2   MVC   SEOPNN,SENAME        SAVE NUMBER                                 
         DROP  R4                                                               
*                                                                               
         MVC   UTL+4(1),SEOPN      SET UTL                                      
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+11(1),SEOPN                                                 
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'SYSFLES'                          
         ICM   R1,15,12(R1)                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,0(R1)            R1=SE FILES                                  
         ST    R1,ASYSFLES                                                      
*                                                                               
         CLC   SEOPN,0(R1)         CONFIRM FILE LIST                            
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    SEFILN,SEFILN       CLEAR DOWN                                   
         LA    R4,SEFILN                                                        
*                                                                               
         SR    RE,RE               RE=NUMBER OF FILES                           
         ICM   RE,3,2(R1)                                                       
         LA    R1,4(R1)            POINT R1 TO FILES                            
*                                                                               
OPENS5   SR    RF,RF                                                            
         ICM   RF,7,5(R1)          RF=DTF ADDR                                  
         MVI   0(R4),C'N'                                                       
         MVC   1(7,R4),22(RF)      GET NAME                                     
*                                                                               
         LA    R4,8(R4)            NEXT FILE                                    
         LA    R1,8(R1)                                                         
         BCT   RE,OPENS5                                                        
         MVI   0(R4),C'X'          END OF                                       
*                                                                               
OPENSXX  B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        BUILD  DATASPACE BLOCK FOR SYSTEM                  *                   
*************************************************************                   
         SPACE 1                                                                
BUILDDS  NTR1                                                                   
*                                                                               
         SAC   512                 SET UP AMODE                                 
         BAS   RE,M31SET                                                        
         LAM   R2,R2,DMALET                                                     
*                                                                               
         L     R2,AHEADER          USE HEADER ECB TO LOCATE DATA                
         SR    R1,R1                                                            
         IC    R1,RESNUM           RESOURCE NUMBER                              
         SLL   R1,6                *64                                          
         AR    R2,R1                                                            
         USING DMSPACED,R2                                                      
         ICM   R1,7,DSPECB+1       OFFSET IS IN ECB                             
         MVC   SEOPNN,DSPNAME                                                   
         LR    R2,R1                                                            
         DROP  R2                                                               
         USING DMSYSHDR,R2                                                      
*                                                                               
         MVC   DSYSID,=C'****        ****'                                      
         MVC   DSYSID+5(7),SEOPNN                                               
         MVC   DSYSENUM+1(1),SEOPN                                              
*                                                                               
         L     R1,ASYSFLES                                                      
         SR    RE,RE               RE=NUMBER OF FILES                           
         ICM   RE,3,2(R1)                                                       
         STH   RE,DSYFILES                                                      
         LR    RF,RE               CALCULATE SPACE FOR FILES                    
         SLL   RF,5                                                             
         LA    RF,64(RF)           64 +32 BYTES PER FILE                        
         AR    RF,R2                                                            
*                                                                               
         ST    RF,DSYAJOBS         SET A(JOBTAB)                                
         LA    R0,32               MAX 32 JOBS                                  
         SLL   R0,4                * 16 BYTES PER JOBTAB ENTRY                  
         AR    RF,R0                                                            
*                                                                               
         ST    RF,DSYALKEY         SET A(LOCKKEY)                               
         LA    R0,32               MAX 32 LOCKKEY ENTRIES                       
         SLL   R0,5                * 32 PER ENTRY                               
         AR    RF,R0                                                            
*                                                                               
         ST    RF,DSYAREQT         CALCULATE A(REQTABLE)                        
         LA    R0,1024             1K FOR REQTABLE                              
         AR    RF,R0                                                            
*                                                                               
         ST    RF,DSYALOCK         CALCULATE A(LOCKTAB)                         
*                                                                               
         SR    RF,RF               RF=NUMBER OF PAGES FOR SYS                   
         IC    RF,2(R3)                                                         
         SLL   RF,12               RF*4096                                      
         AR    RF,R2                                                            
         ST    RF,DSYABUFF         SAVE A(BUFFER)                               
*                                                                               
         S     RF,DSYALOCK         SUBTRACT A(LOCKTAB)                          
         SRL   RF,3                DIVIDE BY 8                                  
         SH    RF,=H'2'            LESS 2 FOR HEADER                            
*                                                                               
         ST    R2,FULL                                                          
         L     R2,DSYALOCK         SET UP LOCKTAB HEADER                        
         XC    0(16,R2),0(R2)                                                   
         STH   RF,0(,R2)           SAVE MAX NUMBER OF LOCKS                     
         L     R2,FULL                                                          
*                                                                               
         ST    R2,FULL                                                          
         L     R2,DSYAREQT         SET UP REQTABLE HEADER                       
         MVC   0(2,R2),=H'8'                                                    
         MVC   6(2,R2),=C'00'                                                   
         LR    RF,R2                                                            
         AH    RF,=Y(127*8+6-1)                                                 
         STCM  RF,15,2(R2)                                                      
         L     R2,FULL                                                          
*                                                                               
         CLI   3(R3),0             CLEAR A(BUFFER) IF PAGES=0                   
         BNE   *+10                                                             
         XC    DSYABUFF,DSYABUFF                                                
*                                                                               
         LA    R1,4(R1)            POINT R1 TO FILES                            
*                                                                               
BLD050   LA    R2,64(R2)                                                        
*                                                                               
BLD051   MVC   0(1,R2),3(R1)       FILE NUMBER                                  
         SR    RF,RF                                                            
         ICM   RF,7,5(R1)          RF=DTF ADDR                                  
*                                                                               
BLD070   LA    R1,8(R1)                                                         
         LA    R2,32(R2)                                                        
         BCT   RE,BLD051           NEXT                                         
         LA    R2,32(R2)                                                        
         LR    R1,R2                                                            
*                                                                               
BUILDXX  SAC   0                   RESET AMODE                                  
         BAS   RE,M24SET                                                        
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        SET UP OPERATOR COMMS                              *                   
*************************************************************                   
         SPACE 1                                                                
SETOPS   ST    RE,SAVERE                                                        
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        CLEAR SYSTEM                                       *                   
*************************************************************                   
         SPACE 1                                                                
CLEAR    NTR1                                                                   
*                                                                               
         BAS   RE,GETSPC           GET ADDRESS OF DSPACE                        
*                                                                               
         SAC   512                 SET UP AMODE                                 
         BAS   RE,M31SET                                                        
         LAM   R2,R2,DMALET                                                     
         L     R2,DMOFFS                                                        
*                                                                               
         CLI   SYSTEM,0                                                         
         BE    EXITEQ                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SYSTEM           SYSTEM = RESOURCE NUMBER                     
         SLL   R1,6                *64                                          
         AR    R2,R1                                                            
         USING DMSPACED,R2                                                      
         ICM   R1,7,DSPECB+1       OFFSET IS IN ECB                             
         LR    R2,R1                                                            
         DROP  R2                                                               
         USING DMSYSHDR,R2                                                      
*                                                                               
         ICM   RF,15,DSYABUFF      CLEAR REOVERY HEADER                         
         BZ    CLR010                                                           
         LR    R2,RF                                                            
         XC    0(96,R2),0(R2)                                                   
*                                                                               
CLR010   LR    R2,R1                                                            
         ICM   RF,15,DSYALOCK      CLEAR LOCKTAB                                
         LR    R2,RF                                                            
         LH    R3,0(,R2)           R3=MAX NUMBER OF LOCKS                       
         SLL   R3,3                *8                                           
         XC    2(2,R2),2(R2)       CLEAR NUMBER OF LOCKS                        
         LA    R2,16(RF)                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               CLEAR LOCK TABLE                             
*                                                                               
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        BUILD PROGRAMS TABLE                               *                   
*************************************************************                   
         SPACE 1                                                                
BUILDPGM NTR1                                                                   
*                                                                               
         SAC   512                 SET UP AMODE                                 
         BAS   RE,M31SET                                                        
         LAM   R2,R2,DMALET                                                     
         L     R2,DMOFFS                                                        
         L     R2,DHAPGMS-DMDHDR(,R2)                                           
         SAC   0                                                                
         BAS   RE,M24SET                                                        
*                                                                               
         MVI   UTL+4,1                                                          
         GOTO1 =V(DATAMGR),DMCB,OPEN,=C'SER',=C'NCTFILE X'                      
*                                                                               
         L     R5,=A(BIGBLOCK)                                                  
         USING PGMLSTD,R5                                                       
         LA    R3,IOAREA                                                        
         USING CT3REC,R3                                                        
         XC    CT3KEY,CT3KEY                                                    
         MVI   CT3KTYP,CT3KTYPQ                                                 
         MVI   CT3PSUB,CT3PRGQ                                                  
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,(R3),(R3)                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CT3KTYP(2),=C'3P'   IGNORE THIS IF NONE FOUND                    
         BNE   EXITEQ                                                           
         LA    R0,0                                                             
BLDP001  AH    R0,=H'1'            R0=PROGRAM COUNT                             
         LA    R4,CT3DATA                                                       
         USING CTPGMD,R4                                                        
BLDP010  CLI   CTPGMEL,CTPGMELQ    FIND PROGRAM ELEMENT                         
         BE    BLDP020                                                          
         SR    R0,R0                                                            
         ICM   R0,1,CTPGMLEN                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     BLDP010                                                          
*                                                                               
BLDP020  MVC   0(1,R5),CT3PSYS     KEY SYSTEM                                   
         MVC   1(1,R5),CT3PCTRY    KEY EXP COUNTRY                              
         XI    1(R5),X'FF'                                                      
         MVC   2(1,R5),CTPGMSRT    SORT FACTOR                                  
         MVC   3(3,R5),CTPGMNAM    KEY NAME                                     
         LA    R5,6(R5)                                                         
*                                                                               
         XC    PGMNAME(PGMLSTX-PGMLSTD),PGMNAME                                 
*                                                                               
         MVC   PGMNAME,CTPGMNAM                                                 
         MVC   PGMIND,CTPGMIN1                                                  
         MVC   PGMNUM,CTPGMNUM                                                  
         MVC   PGMCOSYS,CTPGMOVL                                                
         MVC   PGMPRTY,CTPGMPRT                                                 
         MVC   PGMIND2,CTPGMIN2                                                 
         MVC   PGMTSKMX,CTPGMMAX                                                
         MVC   PGMCTRY,CTPGMCTR                                                 
         MVC   PGMTEXT,CTPGMTXT                                                 
         MVC   PGMIND3,CTPGMIN3                                                 
         MVC   PGMIND4,CTPGMIN4                                                 
*                                                                               
         LA    R5,PGMLSTX-PGMLSTD(R5)                                           
*                                                                               
         CLI   CTPGMLEN,CTPGML1Q   CHECK NO AGENCY LIST                         
         BE    BLDP030                                                          
*                                                                               
         STCM  R2,7,PGMAGYLA       SET UP AGENCY LIST IN BLOCK                  
         SR    R1,R1                                                            
         IC    R1,CTPGMLEN                                                      
         SH    R1,=Y(CTPGML1Q)                                                  
         SAC   512                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),CTPGMAGY                                                 
         AR    R2,R1                                                            
         XC    0(2,R2),0(R2)                                                    
         LA    R2,2(,R2)                                                        
         SAC   0                                                                
*                                                                               
BLDP030  GOTO1 =V(DATAMGR),DMCB,DMRSEQ,CTFILE,(R3),(R3)                         
         TM    8(R1),X'80'                                                      
         BO    BLDP050                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CT3KTYP,CT3KTYPQ    STILL PROGRAM RECORDS                        
         BNE   BLDP050                                                          
         CLI   CT3PSUB,CT3PRGQ                                                  
         BNE   BLDP050                                                          
         B     BLDP001             NEXT ENTRY                                   
*                                                                               
BLDP050  L     R5,=A(BIGBLOCK)     SORT THE BLOCK INTO ORDER                    
         GOTO1 =V(XSORT),DMCB,(R5),(R0),PGMLSTX-PGMLSTD+6,6,0                   
         L     R5,=A(BIGBLOCK)                                                  
*                                                                               
         SAC   512                                                              
BLDP100  MVC   0(PGMLSTX-PGMLSTD,R2),6(R5)                                      
         LA    R5,PGMLSTX-PGMLSTD(R5)                                           
         LA    R5,6(R5)                                                         
         LA    R2,PGMLSTX-PGMLSTD(,R2)                                          
         BCT   R0,BLDP100                                                       
         SAC   0                                                                
*                                                                               
         B     EXITEQ                                                           
*************************************************************                   
*        DATASPACE ROUTINES                                 *                   
*************************************************************                   
         SPACE 1                                                                
FREESPC  ST    RE,SAVERE           DELETE DATASPACE                             
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'DEL '                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                 NB - IGNORE CONDITON CODE                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
MAKESPC  ST    RE,SAVERE           CREATE NEW DATASPACE                         
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'MAKE'                                                 
         MVC   WORK+4(12),DSPACE                                                
         ICM   RF,15,PAGES         NUMBER OF 4K PAGES                           
         STCM  RF,15,WORK+16                                                    
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GETSPC   ST    RE,SAVERE           GET ALET                                     
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
         MVC   DMOFFS,WORK+20      EXTRACT VALUES                               
         MVC   DMALET,WORK+24                                                   
         MVC   DMTOKN,WORK+28                                                   
         OC    DMALET,DMALET                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SETWAIT - SET TIMER AND WAIT INDEFINITELY - ONLY RETURN WHEN AN     *         
* INTERRUPT IS DETECTED                                               *         
***********************************************************************         
         SPACE 1                                                                
SETWAIT  NTR1  ,                                                                
         L     R1,AOPERECB                                                      
         WAIT  ECB=(1)                                                          
*                                                                               
SETWAITX B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         SPACE 1                                                                
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,T1         PRINT TITLE                                  
         PUT   SYSPRINT,T2                                                      
         PUT   SYSPRINT,T3                                                      
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN),X'FLAGS',AL3(OUTPUT)             
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*                                                                               
         SPACE 1                                                                
CARDTAB  DS    0F                                                               
         DC    C'MODE   ',AL1(3,10),X'0000',AL3(MODE)                           
         DC    C'SYSTEM ',AL1(5,00),X'C000',AL3(VALSE)                          
         DC    C'DSPACE ',AL1(5,12),X'0000',AL3(DSPACE)                         
         DC    C'SYLIST ',AL1(5,00),X'8000',AL3(SLIST)                          
         DC    C'TYPE   ',AL1(3,01),X'0000',AL3(SSB+SSODSPAC-SSOOFF)            
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
MODE     DC    CL10'INIT'          DEFAULT TO INIT                              
DSPACE   DC    CL12' '                                                          
SYLIST   DC    C'N'                                                             
SYSTEM   DC    X'00'                                                            
         EJECT                                                                  
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQU                                                          
*                                                                               
         CLI   SYLIST,C'Y'         ARE WE LOADING SYSLIST                       
         BNE   VALC001                                                          
*                                                                               
         CLC   0(7,R2),=C'SYLISTX'                                              
         BNE   *+12                                                             
         MVI   SYLIST,C'N'         SYSLIST FINISHED                             
         B     EXITEQU                                                          
*                                                                               
         BAS   RE,VALSYS           VALIDATE SYSTEM ENTRY                        
         B     EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC500                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         CLC   0(2,RF),=C'  '      EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC500                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC500                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC500                                                          
*                                                                               
VALC080  TM    9(R4),X'01'         DATE INPUT                                   
         BZ    VALC400                                                          
         LA    R0,1(R1)            SET R0 INPUT LEN                             
         ST    RF,FULL                                                          
         GOTO1 =V(PERVAL),DMCB,((R0),(R2)),(X'60',WORK)                         
         L     RF,FULL                                                          
         CLI   4(R1),X'04'                                                      
         BNE   CERRDAT                                                          
         MVC   0(2,RF),WORK+PVALCSTA-PERVALD                                    
         B     VALC500                                                          
*                                                                               
VALC400  CLI   8(R4),0             DONT CARE                                    
         BE    VALC410                                                          
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         SR    RE,RE                                                            
         IC    RE,8(R4)            PAD OUT TO SPACES                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SPACES                                                   
VALC410  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC500  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC500                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDAT  LA    R1,=C'INVALID DATE    '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU  *                   
*************************************************************                   
         SPACE 1                                                                
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALF                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BAS   RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BAS   RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BAS   RE,VALTADD                                                       
         B     EXITEQU                                                          
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
*************************************************************                   
*        SYLIST AND SYLISTX                                 *                   
*************************************************************                   
         SPACE 1                                                                
SLIST    MVI   SYLIST,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        GET SYSTEM FROM 0(R2) (R1)=EX LEN                  *                   
*************************************************************                   
         SPACE 1                                                                
VALSYS   NTR1                                                                   
         LA    R0,8                                                             
         LR    R1,R2               SET R1 TO L'SYSNAME                          
VALSYSA  LA    R1,1(R1)                                                         
         CLI   0(R1),C'='                                                       
         BE    *+12                                                             
         BCT   R0,VALSYSA                                                       
         B     CERRDEL                                                          
         ST    R1,FULL             SAVE A(=)                                    
         SR    R1,R2                                                            
         BCTR  R1,0                                                             
         LR    RF,R1                                                            
*                                                                               
         L     R4,=V(SELIST)       MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
VALSYS0  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SENAME      TEST NAME                                    
         BE    VALSYS1                                                          
         BXLE  R4,R0,VALSYS0       NEXT                                         
         B     CERRSES             ERROR SE SYS NOT FOUND                       
*                                                                               
VALSYS1  LA    R1,STABLE           ADD SE TO STABLE                             
         CLC   0(2,R1),=X'FFFF'                                                 
         BE    VALSYS2                                                          
         LA    R1,4(R1)                                                         
         B     *-14                                                             
VALSYS2  MVI   0(R1),0                                                          
         MVC   1(1,R1),SESYS                                                    
*                                                                               
         L     RE,FULL                                                          
*                                                                               
         TM    1(RE),X'F0'         VALIDATE N,N                                 
         BNO   CERRDEC                                                          
         CLI   2(RE),C','                                                       
         BNE   CERRDEL                                                          
         TM    3(RE),X'F0'                                                      
         BNO   CERRDEC                                                          
*                                                                               
         MVC   BYTE,1(RE)          FIRST IS NUMBER OF SYSPAGES                  
         NI    BYTE,X'0F'                                                       
         MVC   2(1,R1),BYTE                                                     
         MVC   BYTE,3(RE)          2ND IS NUMBER OF BUFFER PAGES                
         NI    BYTE,X'0F'                                                       
         MVC   3(1,R1),BYTE                                                     
*                                                                               
         B     EXITEQU                                                          
         EJECT                                                                  
SETBXLE  LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
VALSE    NTR1                                                                   
         LR    RF,R1                                                            
         L     R4,=V(SELIST)       MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
VALSE0   EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SENAME      TEST NAME                                    
         BE    VALSE1                                                           
         BXLE  R4,R0,VALSE0        NEXT                                         
         B     CERRSES             ERROR SE SYS NOT FOUND                       
*                                                                               
VALSE1   MVC   SYSTEM,SESYS        SET NUMBER                                   
         B     EXITEQU                                                          
         DROP  R4                                                               
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
         SPACE 1                                                                
DMREAD   DC    CL8'DMREAD '                                                     
DMRSEQ   DC    CL8'DMRSEQ '                                                     
DMRDHI   DC    CL8'DMRDHI '                                                     
OPEN     DC    CL8'OPEN'                                                        
CTFILE   DC    CL8'CTFILE'                                                      
SPACES   DC    CL166' '                                                         
STARS    DC    16C'*'                                                           
FFS      DC    16X'FF'                                                          
MAXLINE  DC    PL3'60'                                                          
LWAIT    DC    AL4(30*100)                                                      
         SPACE 2                                                                
T1       DC    166C' '                                                          
T2       DC    166C' '                                                          
T3       DC    166C' '                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DCBS & ADCONS                                      *                   
*************************************************************                   
         SPACE 2                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
AOPERECB DC    A(0)                                                             
ACOMM    DC    A(0)                                                             
*                                                                               
UTL      DC    F'0',AL1(10),XL250'00'                                           
*                                                                               
SSB      DC    H'0',X'FF',XL252'00'                                             
         EJECT                                                                  
*************************************************************                   
*        RESOURCE TABLE                                     *                   
*************************************************************                   
         SPACE 1                                                                
RMAXSYS  EQU   512                                                              
RMAXRES  EQU   512                                                              
*                                                                               
*+0      XL4   OFFSET OF BLOCK ADDRESS IN DMDHDR                                
*+4      AL1   NUMBER OF PAGES TO ALLOCATE                                      
*+5      AL3   SPARE                                                            
*                                                                               
         DS    0D                                                               
TTABLE   DS    0XL8                                                             
         DC    AL4(DHAECBS-DMDHDR),AL1(1),AL3(0)                                
         DC    AL4(DHACOMM-DMDHDR),AL1(1),AL3(0)                                
         DC    AL4(DHALOCK-DMDHDR),AL1(16),AL3(0)                               
         DC    AL4(DHAADVS-DMDHDR),AL1(1),AL3(0)                                
         DC    AL4(DHAFACS-DMDHDR),AL1(1),AL3(0)                                
* 16 TIMES NUMBER OF LIVE FACPAKS                                               
*&&UK*&& DC    AL4(DHATOR-DMDHDR),AL1(16*4),AL3(0)                              
* IN US, 7 ADVS, 3 REPS, 1 DARE, 1 SPARE                                        
*&&US*&& DC    AL4(DHATOR-DMDHDR),AL1(16*12),AL3(0)                             
         DC    AL4(DHAPGMS-DMDHDR),AL1(8),AL3(0)                                
         DC    AL4(DHALOCT-DMDHDR),AL1(16),AL3(0)                               
         DC    AL4(DHAPQS-DMDHDR),AL1(PQQLEN),AL3(0)                            
         DC    X'FFFFFFFFFFFFFFFF'                                              
*                                                                               
PQQLEN   EQU   (((TPQQCNT*TPQQLEN)+TPQLENQ+TPQLID+4095)/4096)                   
*                                                                               
STABLE   DS    0XL4                                                             
*                                                                               
*+0      XL2   SE NUMBER ALSO RESOURCE NUMBER                                   
*+2      AL1   PAGES FOR FILES AND LOCKS                                        
*+4      AL1   PAGES FOR RECOVERY BUFFER                                        
*                                                                               
         DC    512XL4'FFFFFFFF'                                                 
*                                                                               
*                                                                               
RTABLE   DS    0XL16                                                            
*                                                                               
*+0      CL8   NAME                                                             
*+8      CL1   TYPE (S)YSTEM (B)UFFER                                           
*+9      AL1   RESOURCE NUMBER                                                  
*+10     AL1   NUMBER OF 4K PAGES TO ALLOCATE                                   
*                                                                               
         DC    XL2'FFFF'                                                        
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE DC                                 *                   
*************************************************************                   
         SPACE 1                                                                
         DS    0D                                                               
BIGBLOCK DS    60000C                                                           
AGENCYBL DS    2000C                                                            
         DS    0D                                                               
WORKAREA DC    60000X'00'                                                       
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
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
DMCB     DS    6F                                                               
CARDEND  DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
ADSDATA  DS    A                   ADDRESS OF DS BLOCK                          
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
WAITER   DS    A                                                                
*                                                                               
DMOFFS   DS    A                   DATASPACE OFFSET                             
DMALET   DS    A                   ALET                                         
DMTOKN   DS    CL8                 TOKEN                                        
*                                                                               
ASYSFLES DS    A                                                                
RESNUM   DS    XL1                 RESOURCE NUMBER                              
SEOPN    DS    CL1                 SE NUMBER                                    
SEOPNN   DS    CL7                 SE NAME                                      
SEFILN   DS    CL256               FILENAMES FOR OPEN                           
*                                                                               
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
IOAREA   DS    4096C                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 2                                                                
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         SPACE 1                                                                
* CTGENFILE                                                                     
* DMDSYSHDR                                                                     
* DMDSHDR                                                                       
* DMSPACED                                                                      
* DMDFTPH                                                                       
* DMDTFIS                                                                       
* DDPERVALD                                                                     
* FADSECTS                                                                      
* FASSBOFF                                                                      
* FATABSPQ                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DMDSYSHDR                                                      
       ++INCLUDE DMDSHDR                                                        
       ++INCLUDE DMSPACED                                                       
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE FATABSPQ                                                       
       ++INCLUDE FAD                                                            
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DMDMGRDSPX10/16/00'                                      
         END                                                                    
