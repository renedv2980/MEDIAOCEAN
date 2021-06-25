*          DATA SET SPREPFXYKM AT LEVEL 108 AS OF 11/01/00                      
*PHASE SPFX02M                                                                  
         TITLE 'FIX NEW INVOICE RECORDS FOR MATCHMAKER'                         
         SPACE 2                                                                
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                    REQUEST FIRST                                    *         
* MAIN BODY --  GET MASTER KEY FOR MINOPEN                            *         
***********************************************************************         
FX       DS    0H                                                               
*                                                                               
         XC    JANOOC,=X'FFFF'     FIND COMPLEMENT                              
         ZAP   E8FOUND,=P'0'                                                    
         ZAP   FEFOUND,=P'0'                                                    
         ZAP   FEADDED,=P'0'                                                    
         ZAP   TOTRECS,=P'0'                                                    
         XC    MYKEY,MYKEY         FOR THE 1ST TIME TROUGH                      
         BAS   RE,INITMIN          INITIALIZE MINIO                             
*                                                                               
         USING SNVKEYD,R2                                                       
         LA    R2,XKEY                                                          
         XC    XKEY,XKEY                                                        
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
*                                                                               
         MVC   XKEY2,XKEY                                                       
GETSNV05 GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY                     
         OC    MYKEY,MYKEY                                                      
         BZ    GETSNV20                                                         
         CLC   XKEY,MYKEY          SHOULD BE SAME KEY                           
         BE    *+6                 IF YES, GET NEXT KEY                         
         DC    H'0'                IF NO, I WANT TO KNOW ABOUT IT               
*                                                                               
GETSNV10 GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY                     
*                                                                               
GETSNV20 CLC   XKEY(2),XKEY2       INVOICE RECORD ?                             
         BNE   GETSNVX             NO, DONE                                     
*        MVC   BYTE,SNVKAM         CHECK FOR SJR                                
*        NI    BYTE,X'F0'          ONLY FOR NOW                                 
*        CLI   BYTE,X'C0'                                                       
*        BNE   GETSNV10                                                         
         CLC   SNVKMINK,EFFS       PROCESS RECS WITH FFS IN EL KEY              
         BNE   GETSNV10            WILL PREVENT REPETITION                      
         AP    TOTRECS,=P'1'                                                    
         CLC   SNVKMOS,JANOOC      IS DATE AFTER JAN1/00 ?                      
         BH    GETSNV10            IF NOT - IGNORE                              
*                                                                               
         MVC   MYKEY,XKEY          SAVE INVOICE KEY                             
         BAS   RE,SNVPROC          PROCESS THIS MASTER KEY                      
         MVC   XKEY,MYKEY          RESTORE INVOICE KEY                          
         B     GETSNV05                                                         
*                                                                               
GETSNVX  DS    0H                                                               
         MVC   P1(19),=C'E8 ELEMENTS FOUND :'                                   
         EDIT  E8FOUND,(16,P1+20),COMMAS=YES,ZERO=NOBLANK                       
         MVC   P2(19),=C'FE ELEMENTS FOUND :'                                   
         EDIT  FEFOUND,(16,P2+20),COMMAS=YES,ZERO=NOBLANK                       
         MVC   P3(19),=C'FE ELEMENTS ADDED :'                                   
         EDIT  FEADDED,(16,P3+20),COMMAS=YES,ZERO=NOBLANK                       
         MVC   P4(19),=C' TOTAL RECDS READ :'                                   
         EDIT  TOTRECS,(16,P4+20),COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
SNVPROC  NTR1                                                                   
*                                                                               
         L     R5,=A(MINBLK)                                                    
         USING MINBLKD,R5                                                       
         XC    MINMKEY,MINMKEY                                                  
         MVC   MINMKEY(L'SNVKMAST),XKEY        GET MASTER KEY                   
*        GOTO1 HEXOUT,DMCB,MINMKEY,P,24                                         
*                                                                               
         GOTO1 VMINIO,DMCB,('MINOPN',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    SNV20                                                            
         CLI   MINERR,MINESNF      RECORD DOESN'T EXIST                         
         BE    *+6                 OK                                           
         DC    H'0'                ELSE ERROR                                   
*                                                                               
* DO I NEED IT ?                                                                
*                                                                               
SNV05    DS    0H                                                               
         L     R4,=A(MELEM)        ADD ACTIVITY ELEMENT                         
         USING ACTVD,R4                                                         
         XC    0(L'MELEM,R4),0(R4)   CLEAR MINELEM AREA                         
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,ACTVLENQ                                                 
         MVC   ACTVADDT,TODAYB                                                  
         MVC   ACTVADID,RCORIGID                                                
*                                                                               
         GOTO1 VMINIO,DMCB,('MINADD',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         DROP  R4                                                               
*&&DO                                                                           
SNV10    DS    0H                                                               
         TM    MINSTAT,MINDELQ     RECORD SET IS DELETED                        
         BNO   SNV20                                                            
         GOTO1 VMINIO,DMCB,('MINRSF',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
SNV20    DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         LA    R4,MINEKEY                                                       
         MVI   0(R4),X'E8'                                                      
*                                                                               
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
*                                                                               
         L     R4,=A(MELEM)          A(AREA FOR ELEM OR CLUSTER)                
         CLI   MINERR,0              TREAT ALL ERRS HERE AS EOF                 
         BNE   SNV30                                                            
         CLI   0(R4),X'E8'           FOUND E8 ELEM ?                            
         BNE   SNV30                 NO, LOOK FOR FE ELEM                       
*        MVC   P+55(10),=C' E8 FOUND '                                          
         AP    E8FOUND,=P'1'                                                    
         B     EXIT                  YES, THEN DONE                             
                                                                                
SNV30    DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         LA    R4,MINEKEY                                                       
         MVI   0(R4),X'FE'                                                      
*                                                                               
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
*                                                                               
         L     R4,=A(MELEM)          A(AREA FOR ELEM OR CLUSTER)                
         CLI   MINERR,0              TREAT ALL ERRS HERE AS EOF                 
         BNE   SNV40                                                            
         CLI   0(R4),X'FE'           FOUND FE ELEM ?                            
         BNE   SNV40                 NO, ADD IT                                 
*        MVC   P+55(10),=C' FE FOUND '                                          
         AP    FEFOUND,=P'1'                                                    
         B     EXIT                  YES, THEN DONE                             
*                                    NO, ADD FE ELEM FOR LEN OF 100             
SNV40    XC    0(L'MELEM,R4),0(R4)   CLEAR MINELEM AREA                         
         MVI   0(R4),X'FE'           ELEMENT CODE                               
         MVI   1(R4),100             ELEMENT LENGTH                             
         GOTO1 VMINIO,DMCB,('MINADD',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(24),=C' FE ADDED, MINMKEY IS: '                                
         GOTO1 HEXOUT,DMCB,MINMKEY,P+24,24                                      
         GOTO1 REPORT                                                           
         AP    FEADDED,=P'1'                                                    
*                                                                               
SNV50    DS    0H                                                               
         GOTO1 VMINIO,DMCB,('MINCLS',MINBLKD)                                   
SNVX     XIT1                                                                   
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        INITIALIZE MINIO, ETC.                                                 
*                                                                               
***********************************************************************         
INITMIN  NTR1                                                                   
         L     R5,=A(MINBLK)                                                    
         USING MINBLKD,R5                                                       
         CLI   INITMINO,C'Y'                                                    
         BE    INITMINX                                                         
         MVI   INITMINO,C'Y'                                                    
                                                                                
         GOTO1 LOADER,DMCB,=CL8'T00A74',0,0                                     
         MVC   VMINIO,DMCB+4       SET MINIO ADDRESS                            
                                                                                
         L     R0,=A(MINBLK)       CLEAR MINBLOCK                               
         LA    R1,MINBLKL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   MINRECUP,RECUP        A(RECUP)                                   
         MVC   MINCOMF,ACOMFACS      A(COMFACS)                                 
         MVI   MINOPEN,C'N'          SET NOT OPEN                               
         MVC   MINFIL,=CL8'XSPFIL'   FILE NAME                                  
         MVC   MINDIR,=CL8'XSPDIR'   DIR NAME                                   
         MVI   MINFKLEN,L'SNVKEY     KEY LENGTH                                 
         MVI   MINEKLEN,L'SNVKMINK   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'SNVKMAST   DISPLACEMENT TO ELEMENT KEY                
         MVI   MINNCTL,L'SNVDSTAT    NUMBER OF CONTROL BYTES                    
         MVC   MINFRCLM,=H'3975'     MAXIMUM RECORD LENGTH                      
                                                                                
         L     RF,=A(MBUFF1)         POINT TO START OF MINIO BUFFERS            
         ST    RF,MINBUFF            A(FIRST BUFFER)                            
         MVI   MINNBUF,2             USE TWO BUFFERS                            
                                                                                
         L     RF,=A(MRTAB)                                                     
         ST    RF,MINRTAB            A(AREA FOR RECORD TABLE)                   
         MVC   MINRTABL,=Y(L'MRTAB)  LENGTH OF RECORD TABLE                     
                                                                                
         L     RF,=A(MELEM)          A(AREA FOR ELEM OR CLUSTER)                
         ST    RF,MINELEM                                                       
         XC    0(L'MELEM,RF),0(RF)   CLEAR MINELEM AREA                         
                                                                                
         MVC   MINMAXEL,=Y(L'MELEM)  MAX LENGTH OF ELEM OR CLUSTER              
                                                                                
         MVI   MINDELSW,C'Y'       PROCESS DELETED RECORDS                      
         CLI   RCWRITE,C'Y'          IF WRITES NOT ALLOWED                      
         BE    *+8                                                              
         MVI   MINWRITE,C'N'         DISABLE WRITES                             
INITMINX XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
EFFS     DC    XL6'FFFFFFFFFFFF'                                                
JANOOC   DC    XL2'C821'           COMPRESSED JAN1/00                           
VMINIO   DS    A                                                                
RELO2    DS    F                                                                
X        DS    XL256                                                            
*                                                                               
INITMINO DS    C                                                                
XKEY     DS    CL70                                                             
XKEY2    DS    CL70                                                             
MYKEY    DS    CL70                                                             
*                                                                               
E8FOUND  DS    PL8                 COUNTER FOR FOUND E8 ELEMS                   
FEFOUND  DS    PL8                 COUNTER FOR FOUND FE ELEMS                   
FEADDED  DS    PL8                 COUNTER FOR ADDED FE ELEMS                   
TOTRECS  DS    PL8                 COUNTER FOR TOTAL RECS READ                  
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
         DC    CL8'*MINBLK*'                                                    
MINBLK   DS    XL(MINBLKL)                                                      
         DS    0D                                                               
         DC    CL8'*MELEM**'                                                    
MELEM    DS    XL256                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*MBUFF**'                                                    
MBUFF1   DS    XL4000                                                           
MBUFF2   DS    XL4000                                                           
         DS    0D                                                               
         DC    CL8'*MRTAB**'                                                    
MRTAB    DS    XL(200*(6+L'BGRKMAST))                                           
*                                                                               
*                                                                               
         PRINT OFF                                                              
* SPREPWORKD                                                                    
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPREPPTBUF                                                     
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
* MEDPRTOPT                                                                     
*       +INCLUDE MEDRPTOPT                                                      
         EJECT                                                                  
* SPREPMODES                                                                    
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
* SPMEDBLOCK                                                                    
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
* SPGENBUY                                                                      
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
* SPGENGOAL                                                                     
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENSLH                                                       
       ++INCLUDE SPGENSLK                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE SPGENBGR                                                       
       ++INCLUDE SPGENMSR                                                       
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
* DDDICTATED                                                                    
       ++INCLUDE DDDICTATED                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'108SPREPFXYKM11/01/00'                                      
         END                                                                    
